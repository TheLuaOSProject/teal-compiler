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

local COMPARISON_OPERATORS = {
    "==", "~=", "<", ">", "<=", ">="
}

---@param op string
---@return boolean
local function is_comparison(op)
    local is = false
    for _, v in ipairs(COMPARISON_OPERATORS) do
        if op == v then
            is = true
            break
        end
    end
    return is
end

local int8_t, int16_t, int32_t, int64_t = ctx:get_type("int8_t"), ctx:get_type("int16_t"), ctx:get_type("int32_t"), ctx:get_type("int64_t")
local uint8_t, uint16_t, uint32_t, uint64_t = ctx:get_type("uint8_t"), ctx:get_type("uint16_t"), ctx:get_type("uint32_t"), ctx:get_type("uint64_t")

local integer_t = int64_t
local boolean_t = ctx:get_type("bool")
local number_t = ctx:get_type("double")
local nil_t = ctx:get_type("void")
local cstring_t = ctx:get_type("const char *")
local cvaradict_t = ctx:new_opaque_struct_type("cvaradict")

local String_t = ctx:new_struct_type("string", {
    ctx:new_field(uint64_t, "length"),
    ctx:new_field(uint8_t:pointer(), "data")
})

---@type { [integer] : gccjit.Type* }
local tuple_cache = {}

---@class FunctionType
---@field args gccjit.Type*[]
---@field rets gccjit.Type*[]

---@param type tl.Type
---@return gccjit.Type*
local function conv_teal_type(type)
    return utilities.switch(type.typename) {
        ["integer"] = ret(integer_t),
        ["boolean"] = ret(boolean_t),
        ["number"]  = ret(number_t),
        ["string"]  = ret(String_t),
        ["nil"]     = ret(nil_t),
        ["tuple"] = function ()
            --[[@cast type tl.TupleType]]

            if tuple_cache[type.typeid] then return tuple_cache[type.typeid] end

            ---@type gccjit.Type*[]
            local tup_fields = {}

            for i, tl_type in ipairs(type.tuple) do
                local t = conv_teal_type(tl_type) --[[@as gccjit.Type*]]
                table.insert(tup_fields, ctx:new_field(t, "tuple-element_"..i.."_"..tl_type.typeid))
            end

            local t = ctx:new_struct_type("tuple-"..type.typeid, tup_fields, loc(type))
            tuple_cache[type.typeid] = t
            return t
        end,
        ["nominal"] = function ()
            --[[@cast type tl.NominalType]]
            local names = assert(type.names)
            if names[1] == "c" then
                return utilities.switch(names[2]) {
                    ["string"] = ret(String_t),
                    ["varadict"] = ret(cvaradict_t),
                    default = function(x)
                        error(string.format("Unsupported C type: %s", x))
                    end
                }
            else
                error(string.format("Unsupported nominal type: %s", table.concat(names, '.')))
            end
        end,
        ["function"] = function ()
            --[[@cast type tl.FunctionType]]

            local is_va = false
            ---@type gccjit.Type*[]
            local args = {}
            for _, arg in ipairs(type.args) do
                local a = conv_teal_type(arg)
                if a == cvaradict_t then
                    is_va = true
                    break
                end

                table.insert(args, a)
            end

            local ret = type.rets and conv_teal_type(type.rets[1]) or nil_t
            return ctx:new_function_ptr_type(ret, args, is_va, loc(type))
        end,
        default = function(x)
            error(string.format("Unsupported type: %s", x))
        end
    }
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
            assert(x, "Expected a value")

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

---@alias Visitor.Function fun(node: tl.Node, variables: { [string] : gccjit.LValue* }, func: gccjit.Function*?, block: gccjit.Block*?, funcs: { [string] : gccjit.Function* }, ...): ...

---@class Visitor
---@field [tl.NodeKind] Visitor.Function
local visitor = {}

---@type Visitor.Function
local function visit(node, vars, func, block, funcs, ...)
    local vtor = visitor[node.kind]
    if not vtor then error(string.format("Unsupported node kind '%s' at %s", node.kind, tostring(loc(node)))) end
    return vtor(node, vars, func, block, funcs, ...)
end

return {
    compiler_context = ctx,
    visitor = visitor,
    -- compile = visit --[=[@as fun(node: tl.Node): gccjit.Object*[]]=]
    ---@param node tl.Node
    ---@return any?
    compile = function (node, ...)
        return visit(node, {}, nil, nil, {}, ...)
    end
}
