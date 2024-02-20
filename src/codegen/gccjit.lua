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
-- along with teal-compiler. If not, see <http://www.gnu.org/licenses/>.

local gccjit = require("backends.gccjit")
local ffi = require("ffi")
local utilities = require("utilities")

local ctx = gccjit.Context.acquire()

---@param node tl.Where | tl.Node
---@return gccjit.Location*?
local function loc(node)
    return ctx:new_location(node.f, node.y, node.x)
end

---@generic T
---@param x T
---@return fun(): T
local function ret(x) return function() return x end end

---@param type tl.Type
---@return gccjit.Type*
local function conv_teal_type(type)
    ---@param x gccjit.Type*.Type
    ---@return gccjit.Type*
    local function t(x) return ctx:get_type(x) end
    return assert(utilities.match(type.typename) {
        ["integer"] = ret(t"int32_t"),
        ["boolean"] = ret(t"bool"),
        ["number"]  = ret(t"double"),
        ["string"]  = ret(t"const char *"),
        ["tuple"] = function ()
            --[[@cast type tl.TupleType]]
            if #type.tuple == 1 then
                return conv_teal_type(type.tuple[1])
            else
                ---@type gccjit.Field*[]
                local types = {}
                for _, tl_type in ipairs(type.tuple) do
                    local ty = conv_teal_type(tl_type)
                    local field = ctx:new_field(ty, string.format("field_%d_%d", #types, tl_type.typeid), loc(type))
                    table.insert(types, field)
                end
                return ctx:new_struct_type(string.format("tuple_%d", type.typeid), types, loc(type))
            end
        end,
        ["nil"] = ret(t"void"),
        default = function(x)
            error(string.format("Unsupported type: %s", x))
        end
    })
end

---@class TypeIs
---@field rvalue fun(x: ffi.cdata*): gccjit.RValue*
---@field lvalue fun(x: ffi.cdata*): gccjit.LValue*
---@field type fun(x: ffi.cdata*): gccjit.Type*

---@type TypeIs
local type_is = setmetatable({}, {
    __index = function (self, tname)
        local t_expected = ffi.typeof("gcc_jit_"..tname.." *")
        self[type] = function (x)
            local t = ffi.typeof(x)
            assert(t == t_expected, string.format("Expected %s, got %s", t_expected, t))
            return x --[[@as any]]
        end
        return self[type]
    end
})

--cache the results of type_is, this will fill the table
_=type_is.rvalue
_=type_is.lvalue
_=type_is.type


---@type { [tl.NodeKind] : fun(node: tl.Node, variables: { [string] : gccjit.RValue* }, func: gccjit.Function*?, block: gccjit.Block*?, ...): any? }
local visitor = {}

function visitor.statements(node, vars, func, block)
    local stmnts = {}
    for _, stmt in ipairs(node) do
        table.insert(stmnts, visitor[stmt.kind](stmt, vars, func, block))
    end
    return stmnts
end

visitor["return"] = function (node, vars, func, block)
    local expr = type_is["rvalue"](visitor[node.exps.kind](node.exps, vars, func, block)[1])
    return block:end_with_return(expr, loc(node))
end

function visitor.expression_list(node, vars, func, block, expected_type)
    local exprs = {}
    for _, expr in ipairs(node) do
        table.insert(exprs, visitor[expr.kind](expr, vars, func, block, expected_type))
    end
    return exprs
end

function visitor.op(node, vars, func, block)
    local e1 = type_is["rvalue"](visitor[node.e1.kind](node.e1, vars, func, block))
    local e2 = type_is["rvalue"](visitor[node.e2.kind](node.e2, vars, func, block))
    local op = assert(node.op.op)

    return ctx:new_binary_op(e1:get_type(), e1, op, e2, loc(node))
end

function visitor.variable(node, vars, func, block)
    local name = assert(node.tk)
    return assert(vars[name])
end

function visitor.variable_list(node, vars, func, block)
    local vnames = {}
    for _, expr in ipairs(node) do
        table.insert(vnames, visitor[expr.kind](expr, vars, func, block))
    end
    return vnames
end

function visitor.identifier(node)
    return assert(node.tk)
end

function visitor.local_declaration(node, vars, func, block)
    --Locals could have multple (i.e local a, b, c), but for now only support 1
    local vname = visitor[node.vars.kind](node.vars, vars, func, block)[1] --[[@as string]]
    local type = conv_teal_type(node.decltuple)

    vars[vname] = type_is["rvalue"](visitor[node.exps.kind](node.exps, vars, func, block)[1])
end

---@param type gccjit.Function*.Kind
---@param node tl.Node
---@param vars { [string] : gccjit.RValue* }
---@return gccjit.Function*
local function new_function(type, node, vars)
    local ret = conv_teal_type(node.rets)
    local name = assert(node.name.tk)
    vars = {}
    ---@type gccjit.Param*[]
    local params = {}
    for _, param in ipairs(node.args) do
        local arg_t = conv_teal_type(param.argtype)
        local name = assert(param.tk)
        local p = ctx:new_param(arg_t, name)
        table.insert(params, p)
        vars[name] = ffi.cast("gcc_jit_rvalue *", p) --[[@as gccjit.RValue*]]
    end

    local fn = ctx:new_function(type, name, ret, params, false, loc(node))
    -- for i, tl_block in ipairs(node.body) do
    --     print("block", i, tl_block.kind, tl_block)

    -- end
    local block = fn:new_block(string.format("fn_%s_block", name))
    visitor[node.body.kind](node.body, vars, fn, block)
    return fn
end

function visitor.global_function(node, vars)
    return new_function("exported", node, vars) --for now, all global functions are exported
end

function visitor.local_function(node, vars)
    return new_function("internal", node, vars)
end

return {
    compiler_context = ctx,
    visitor = visitor,
    ---@param node tl.Node
    compile = function(node) return visitor[node.kind](node, {}) end
}
