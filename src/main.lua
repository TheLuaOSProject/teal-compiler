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

local ffi = require("ffi")
local gccjit = require("backends.gccjit")

---@param ret string
---@return fun(...: string): ffi.ctype*
local function fn(ret)
    return function(...)
        return ffi.typeof(string.format("%s(*)(%s)", ret, table.concat({...}, ", ")) --[[@as ffi.ctype*]])
    end
end

local ctx = gccjit.Context.acquire()
ctx:set_option("dump generated code", true)

local int_t = ctx:get_type("int32_t")
local x_param, y_param = ctx:new_param("x", int_t), ctx:new_param("y", int_t)

--- int32_t add(int32_t x, int32_t y)
local func = ctx:new_function("exported", "add", int_t, { x_param, y_param })
local block = func:new_block("main")
--- return x + y
local res = ctx:new_binary_op(int_t, x_param:as_rvalue(), "+", y_param:as_rvalue())
block:add_eval(res)
block:end_with_return(res)

local printf = ctx:new_function("imported", "printf", int_t, { ctx:new_param("fmt", ctx:get_type("const char *")) }, true)

local main = ctx:new_function("exported", "main", ctx:get_type "void")
local main_block = main:new_block("main")

main_block:add_eval(ctx:new_call(printf, {
    ctx:new_string_literal "Result of 3 + 1: %d\n",
    ctx:new_call(func, { ctx:new_rvalue(int_t, "int", 3), ctx:new_rvalue(int_t, "int", 1) })
}))

main_block:end_with_void_return()

print("Compiling...")
local res = ctx:compile()
if not res then error("Failed to compile") end

local main = res:get_code("main", fn"void"())
if not main then error("Failed to get code") end

print("Calling...")
main()
