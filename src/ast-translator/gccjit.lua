-- Copyright (C) 2024 Amrit Bhogal
--
-- This file is part of teal-compiler.
--
-- teal-compiler is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- teal-compiler is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with teal-compiler.  If not, see <http://www.gnu.org/licenses/>.

local gccjit = require("backends.gccjit")
local ffi    = require("ffi")
local ctx = gccjit.Context.acquire()
local utilities = require("utilities")

---@param node tl.Where | tl.Node
---@return gccjit.Location*?
local function get_loc(node)
    return ctx:new_location(node.f, node.y, node.x)
end

---@param type tl.Type
---@return gccjit.Type*
local function conv_teal_type(type)
    ---@param x gccjit.Type*.Type
    ---@return gccjit.Type*
    local function t(x) return ctx:get_type(x) end
    return assert(utilities.match(type.typename) {
        ["integer"] = function() return t"int32_t" end,
        ["boolean"] = function() return t"bool" end,
        ["number"] = function() return t"double" end,
        ["string"] = function() return t"const char *" end,
        ["tuple"] = function ()
            --[[@cast type tl.TupleType]]
            if #type.tuple == 1 then
                return conv_teal_type(type.tuple[1])
            else
                ---@type gccjit.Field*[]
                local types = {}
                for _, tl_type in ipairs(type.tuple) do
                    local ty = conv_teal_type(tl_type)
                    local field = ctx:new_field(ty, string.format("field_%d_%d", #types, tl_type.typeid), get_loc(type))
                    table.insert(types, field)
                end
                return ctx:new_struct_type(string.format("tuple_%d", type.typeid), types, get_loc(type))
            end
        end,
        ["nil"] = function() return t"void" end,
        default = function(x)
            error(string.format("Unsupported type: %s", x))
        end
    })
end

---@type { [tl.NodeKind] : fun(node: tl.Node, variables: { [string] : gccjit.RValue* }, func: gccjit.Function*?, block: gccjit.Block*?, ...): gccjit.Object*? }
local visitor = {}
---@param node tl.Node
---@param vars { [string] : gccjit.RValue* }
---@param func gccjit.Function*?
---@param block gccjit.Block*?
---@return gccjit.Object* | gccjit.Object*[] | nil
local function visit(node, vars, func, block, ...) return visitor[node.kind](node, vars, func, block, ...) end

function visitor.statements(node, vars, func, block)
    local stmnts = {}
    for _, stmt in ipairs(node) do
        table.insert(stmnts, visit(stmt, vars, func, block))
    end
    return stmnts
end

visitor["return"] = function (node, vars, func, block)
    local expr = visit(node.exps, vars, func, block) --[[@as gccjit.RValue*]]
    block:end_with_return(expr, get_loc(node))
end

function visitor.expression_list(node, vars, func, block)
    return visit(node[1], vars, func, block)
end

function visitor.op(node, vars, func, block)
    local e1 = visit(node.e1, vars, func, block) --[[@as gccjit.RValue*]]
    local e2 = visit(node.e2, vars, func, block) --[[@as gccjit.RValue*]]
    local op = assert(node.op.op)

    return ctx:new_binary_op(ctx:get_type "int32_t", e1, op, e2, get_loc(node))
end

function visitor.variable(node, vars, func, block)
    local name = assert(node.tk)
    return assert(vars[name])
end

function visitor.global_function(node, vars)
    local ret = conv_teal_type(node.rets)
    local name = assert(node.name.tk)
    ---@type gccjit.Param*[]
    local params = {}
    for _, param in ipairs(node.args) do
        local arg_t = conv_teal_type(param.argtype)
        local name = assert(param.tk)
        local p = ctx:new_param(arg_t, name)
        table.insert(params, p)
        vars[name] = ffi.cast("gcc_jit_rvalue *", p) --[[@as gccjit.RValue*]]
    end

    local fn = ctx:new_function("exported", name, ret, params, false, get_loc(node))
    for i, tl_block in ipairs(node.body) do
        local block = fn:new_block(string.format("fn_%s_block_%d", name, i))
        visit(tl_block, vars, fn, block)
    end
    return fn
end

return {
    compiler_context = ctx,
    compile = visit,
    visitor = visitor,
}
