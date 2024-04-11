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
local abi = require("abi")

local ctx = gccjit.Context.acquire()

local int_t, bool_t, number_t, string_t, nil_t
    = ctx:get_type("int64_t"), ctx:get_type("bool"), ctx:get_type("double"), ctx:get_type("const char *"), ctx:get_type("void")

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
    return assert(utilities.match(type.typename) {
        ["integer"] = ret(int_t),
        ["boolean"] = ret(bool_t),
        ["number"]  = ret(number_t),
        ["string"]  = ret(string_t),
        ["nil"]     = ret(nil_t),
        ["tuple"] = function ()
            --[[@cast type tl.TupleType]]
            if #type.tuple == 1 then
                return conv_teal_type(type.tuple[1])
            else
                ---@type gccjit.Field*[]
                local types = {}
                for i, tl_type in ipairs(type.tuple) do
                    local ty = conv_teal_type(tl_type)
                    local field = ctx:new_field(ty, string.format("field_%d_%d", i, tl_type.typeid), loc(type))
                    table.insert(types, field)
                end
                return ctx:new_struct_type(string.format("tuple_%d", type.typeid), types, loc(type))
            end
        end,
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
        local expected_t = ffi.typeof("gcc_jit_"..tname.." *")
        self[tname] = function (x)
            local t = ffi.typeof(x)
            if x.as_rvalue and tname == "rvalue" then
                return x:as_rvalue()
            end
            assert(t == expected_t, string.format("Expected %s, got %s", expected_t, t))
            return x --[[@as any]]
        end
        return rawget(self, tname)
    end
})

--cache the results of type_is, this will fill the table
_=type_is.rvalue
_=type_is.lvalue
_=type_is.type


---@type { [tl.NodeKind] : fun(node: tl.Node, variables: { [string] : gccjit.LValue* }, func: gccjit.Function*?, block: gccjit.Block*?, ...): any? }
local visitor = {}

---@param node tl.Node
---@param vars { [string] : gccjit.LValue* }
---@param func gccjit.Function*
---@param block gccjit.Block*
---@param ... any
---@return any?
local function visit(node, vars, func, block, ...)
    local vtor = visitor[node.kind]
    if not vtor then error(string.format("Unsupported node kind: %s", node.kind)) end
    return vtor(node, vars, func, block, ...)
end

function visitor.statements(node, vars, func, block)
    local stmnts = {}
    for _, stmt in ipairs(node) do
        stmnts[#stmnts+1] = visit(stmt, vars, func, block) --This may return `nil`, so we can't use `table.insert`
    end
    return stmnts
end

visitor["return"] = function (node, vars, func, block)
    local expr = type_is["rvalue"](visit(node.exps, vars, func, block)[1])

    return block:end_with_return(expr, loc(node))
end

function visitor.assignment(node, vars, func, block)
    local lval = type_is["lvalue"](visit(node.vars, vars, func, block)[1])
    local rval = type_is["rvalue"](visit(node.exps, vars, func, block)[1])
    return block:add_assignment(lval, rval, loc(node))
end

function visitor.expression_list(node, vars, func, block, expected_type)
    local exprs = {}
    for _, expr in ipairs(node) do
        table.insert(exprs, visit(expr, vars, func, block, expected_type))
    end
    return exprs
end


function visitor.integer(node)
    return ctx:new_rvalue(int_t, "long", assert(tonumber(node.tk)))
end

function visitor.op(node, vars, func, block)
    local e1 = visit(node.e1, vars, func, block) --[[@as gccjit.RValue* | gccjit.LValue*]]
    local e2 = visit(node.e2, vars, func, block) --[[@as gccjit.RValue* | gccjit.LValue*]]
    local op = assert(node.op.op)

    return ctx:new_binary_op(e1:as_rvalue():get_type(), e1:as_rvalue(), op, e2:as_rvalue(), loc(node))
end

function visitor.variable(node, vars)
    local name = assert(node.tk)
    return assert(vars[name])
end

function visitor.variable_list(node, vars, func, block)
    local vnames = {}
    for _, expr in ipairs(node) do
        table.insert(vnames, visit(expr, vars, func, block))
    end
    return vnames
end

function visitor.identifier(node)
    return assert(node.tk)
end

function visitor.local_declaration(node, vars, func, block)
    --Locals could have multple (i.e local a, b, c), but for now only support 1
    local vname = visit(node.vars, vars, func, block)[1] --[[@as string]]

    local val = type_is["rvalue"](visit(node.exps, vars, func, block)[1])
    local lcl = func:new_local(val:get_type(), vname, loc(node))
    block:add_assignment(lcl, val, loc(node))

    vars[vname] = lcl
end

---@param type gccjit.Function*.Kind
---@param node tl.Node
---@return gccjit.Function*
local function new_function(type, node)
    local ret = conv_teal_type(node.rets)
    local name = assert(node.name.tk)
    ---@type { [string] : gccjit.LValue* }
    local vars = {}
    ---@type gccjit.Param*[]
    local params = {}
    for _, param in ipairs(node.args) do
        local arg_t = conv_teal_type(param.argtype)
        local name = assert(param.tk)
        local p = ctx:new_param(arg_t, name)
        table.insert(params, p)
        vars[name] = p
    end

    local fn = ctx:new_function(type, name, ret, params, false, loc(node))
    local block = fn:new_block(string.format("fn_%s_block", name))
    visit(node.body, vars, fn, block)
    return fn
end

function visitor.global_function(node)
    return new_function("exported", node) --for now, all global functions are exported
end

function visitor.local_function(node)
    return new_function("internal", node)
end

return {
    compiler_context = ctx,
    visitor = visitor,
    compile = visit --[=[@as fun(node: tl.Node): gccjit.Object*[]]=]
}
