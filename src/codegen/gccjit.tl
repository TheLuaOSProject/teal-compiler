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
local teal = require("teal.tl")

local utilities = require("utilities")

local ctx = gccjit.Context.acquire()

local l_type = type


---@alias Type.Type
---| '"function"'
---| '"tuple"'
---| '"tuple field"'
---| '"primitive"'
---| '"c"'

---@class Type
---@field id integer
---@field raw gccjit.Type*
---@field type Type.Type
---@field name string

---@class Type.Function.Parameter
---@field name string?
---@field type Type

---@class Type.Function : Type
---@field args Type.Function.Parameter[]
---@field return_types Type[]
---@field is_varadict boolean
---@field type "function"

---@class Type.Tuple : Type
---@field elements Type.Tuple.Field[]
---@field type "tuple"

---@class Type.Tuple.Field : Type
---@field raw gccjit.Field*
---@field backing_type Type
---@field type "tuple field"


local interface Type where self.type
    enum Type
        "function"
        "tuple"
        "tuple field"
        "primitive"
        "c"
    end

    id: integer
    raw: gccjit.Type
    type: Type.Type
    name: string
end

local record FunctionType is Type where self.type == "function"
    record Parameter
        name: string
        type: Type
    end

    args: {Parameter}
    return_values: {Type}
    is_varadict: boolean
end

local record TupleType is Type where self.type == "tuple"
    record Field is Type where self.type == "tuple field"
        raw: gccjit.Field
        backing_type: Type
    end

    elements: {Field}
end

local function type(x: any): string
    local l_t = l_type(x)
    if l_t == "cdata" then
        return tostring(ffi.typeof(x as ffi.CType))
    else
        return l_t
    end
end

local function loc(node: teal.Where | teal.Node): gccjit.Location
    return ctx:new_location(node.f, node.y, node.x)
end

return {
    compiler_context = ctx,
    compile = function(node: teal.Node) end
}
