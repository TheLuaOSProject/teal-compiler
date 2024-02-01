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

local profiler = require("jit.p")

profiler.start("10vzai1", "jit.p.out")

local ffi = require("ffi")
local teal = require("teal.tl")
local gccjit = require("backends.gccjit")
local pretty = require("pl.pretty")

---@param ret string
---@return fun(...: string): ffi.ctype*
local function fn(ret)
    return function(...)
        return ffi.typeof(string.format("%s(*)(%s)", ret, table.concat({...}, ", ")) --[[@as ffi.ctype*]])
    end
end

local ctx = gccjit.Context.acquire()
ctx:set_option("dump generated code", true)

local in_f = arg[1] or "test.tl"

---@type string
local contents do
    local f = assert(io.open(in_f, "r"))
    contents = f:read("*a")
    f:close()
end

local ast, errs, modules = teal.parse(contents, in_f)
pretty(ast)

profiler.stop()

