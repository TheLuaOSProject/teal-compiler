#!/usr/bin/env luajit

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
-- along with teal-compiler.  If not, see <https://www.gnu.org/licenses/>.

local ffi = require("ffi")

package.path = package.path..";tools/?.lua;src/?.lua" --for utilities.lua
local utilities = require("utilities")

---@param msg string
local function warn(msg) return io.stderr:write("WARNING: "..msg.."\n") end

local USAGE = "Usage: ffi-gen.lua <header> [-o <modname>] [--c-preprocessor] <flags>"

local header = arg[1]
if not header then
    print(USAGE)
end

---@type string[]
arg = table.remove(arg, 1)

---@type string?
local out_f = nil
local c_preprocessor = "clang -E -Wno-error=unused-command-line-argument -fkeep-system-includes -P"
local cflags = {
    "\"-D__attribute__(...)=\"",
    "\"-D__has_feature(...)=0\"",
    "-Wno-builtin-macro-redefined",
    --make sure any extensions are disabled
    "-std=c11",
}

for i, v in ipairs(arg) do
    if v == "-o" then
        if not arg[i+1] then error("-o requires argument") end
        out_f = arg[i+1]
    elseif v == "--c-preprocessor" then
        if not arg[i+1] then error("--c-preprocessor requires an argument") end
        c_preprocessor = arg[i+1]
    elseif arg[i-1] ~= "-o" and arg[i-1] ~= "--c-preprocessor" then
        table.insert(cflags, v)
    end
end

local tmp = os.tmpname()
local cmd = string.format("%s %s %s %s", c_preprocessor, table.concat(cflags, ' '), header, tmp)
if os.execute(cmd) ~= 0 then error("Could not preprocess file! Command: "..cmd) end

local preproc_f = assert(io.open(tmp, "r"))
local contents = preproc_f:read("*a")
preproc_f:close()
os.remove(tmp)

--TODO: Generate ffi load script

print(string.format([[
local ffi = require("ffi")
ffi.cdef [=[
    typedef uint64_t time_t;

    %s
]=]

local lib do
    lib = ""
end

return
]], contents))
