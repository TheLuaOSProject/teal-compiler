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

local tl = require("tl")
local pretty = require("pl.pretty")

local input_file, output_file = arg[1], arg[2] or "ast.lua"
if not input_file then error("Missing input file as first argument") end

local f = assert(io.open(input_file, "rb"))
local ast, errors, modules = tl.parse(f:read("*a"), input_file)
f:close()

if #errors > 0 then
  for _, err in ipairs(errors) do
    io.stderr:write(string.format("%s:%d:%d: %s\n", input_file, err.y, err.x, err.msg))
  end
  os.exit(1)
end

local f = assert(io.open(output_file, "w+b"))
f:write("return ", pretty.write(ast as table))
f:close()
