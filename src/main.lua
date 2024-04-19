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
local utilities = require("utilities")
local teal = require("teal.tl") or require("tl")

if not arg[1] then
    io.stderr:write("Usage: "..arg[0].." <input file>\n")
    os.exit(1)
end

local in_f = arg[1]
local opt_level = 0
local debug_info = false
local use_gc_for_compile = false
---@type string?
local outfile
for i = 2, #arg do
    local v = arg[i]
    if v:match("-O%d") then
        opt_level = assert(tonumber(v:match("-O(%d)")) or tonumber(arg[i + 1]))
    end

    if v == "-o" then outfile = arg[i + 1] end
    if v == "-g" then debug_info = true end
    if v == "--use-gc-for-compile" then use_gc_for_compile = true  end
end

if not use_gc_for_compile then
    collectgarbage("stop")
    collectgarbage("stop")
end

if not outfile then outfile = in_f:gsub("%.tl$", ".so") end

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

gccjit_translator.compile(ast)
local ctx = gccjit_translator.compiler_context
ctx:set_option("optimization level", opt_level)
ctx:set_option("debuginfo", debug_info)

local out_ext = outfile:match("%.(%w+)$")
---@type gccjit.OutputKind | "gimple"
local kind = utilities.match(out_ext) {
    so = "dynamic library",
    S = "assembler",
    o = "object file",
    gimple = "gimple",
    default = "executable"
}

if kind == "gimple" then
    ctx:dump_to_file(outfile, true)
else
    ctx:compile_to_file(kind --[[@as gccjit.OutputKind]], outfile)
end
-- ctx:
-- local res = assert(ctx:compile())
-- local add = assert(res:get_code("add", "int64_t(*)(int64_t, int64_t)")) --[[@as (fun(x: integer, y: integer): integer)]]
-- local my_func = assert(res:get_code("my_func", "int64_t(*)(int64_t)")) --[[@as (fun(x: integer): integer)]]

-- print(add(43, 321))
-- print(my_func(42))
