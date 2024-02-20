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

local gccjit_translator = require("codegen.gccjit")
local ffi = require("ffi")

local teal = require("teal.tl")
local in_f = arg[1] or "test.tl"

---@type string
local contents do
    local f = assert(io.open(in_f, "r"))
    contents = f:read("*a")
    f:close()
end

local ast, errs, modules = teal.parse(contents, in_f)
if #errs > 0 then
    for _, err in ipairs(errs) do
        io.stderr:write(string.format("%s:%d:%d: %s\n", in_f, err.y, err.x, err.msg))
    end
    os.exit(1)
end

gccjit_translator.compile(ast) --[[ @as gccjit.Object*[] ]]
local ctx = gccjit_translator.compiler_context
ctx:set_option("dump generated code", true)
ctx:set_option("optimization level", 0)
local res = ctx:compile()
if not res then
    error("Failed to compile")
end

local add = res:get_code("add", "int64_t(*)(int64_t, int64_t)") --[[@as fun(x: integer, y: integer): integer]]
if not add then
    error("Failed to get add")
end
print(add(43, 321))

local my_func = res:get_code("my_func", "int64_t(*)(int64_t)") --[[@as fun(x: integer): integer]]
if not my_func then
    error("Failed to get my_func")
end

print(my_func(42))
