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
local teal = require("teal.tl")

if not arg[1] or arg[1] == "-h" or arg[1] == "--help" then
    io.stderr:write(string.format([=[Usage: %s <input file> [-O<level>] [-o <output file>] [-g] [--use-gc-for-compile] [--dump-tree]]=], arg[0]))
    os.exit(1)
end

local in_f = arg[1]
-- local opt_level = 0
-- local debug_info = false
-- local use_gc_for_compile = false
-- local opt_level, debug_info, use_gc_for_compile = 0, false, false
local opts = {
    opt_level = 0,
    debug_info = false,
    use_gc_for_compile = false,
    ---@type string?
    outfile = nil,
    dump_tree = false
}

for i = 2, #arg do
    local v = arg[i]
    if v:match("-O%d") then
        opts.opt_level = assert(tonumber(v:match("-O(%d)")) or tonumber(arg[i + 1]))
    end

    if v == "-o" then opts.outfile = arg[i + 1] end
    if v == "-g" then opts.debug_info = true end
    if v == "--use-gc-for-compile" then opts.use_gc_for_compile = true end
    if v == "--dump-tree" then opts.dump_tree = true end
end

if not opts.use_gc_for_compile then
    collectgarbage("stop")
    collectgarbage("stop")
end

if not opts.outfile then opts.outfile = in_f:gsub("%.tl$", ".so") end

---@type string
local contents do
    local f = assert(io.open(in_f, "r"))
    contents = f:read("*a")
    assert(f:close())
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
ctx:set_option("optimization level", opts.opt_level)
ctx:set_option("debuginfo", opts.debug_info)
ctx:set_option("dump initial tree", opts.dump_tree)

local out_ext = opts.outfile:match("%.(%w+)$")
---@type gccjit.OutputKind | "gimple"
local kind = utilities.match(out_ext) {
    so = "dynamic library",
    S = "assembler",
    o = "object file",
    gimple = "gimple",
    default = "executable"
}

if kind == "gimple" then
    ctx:dump_to_file(opts.outfile, true)
else
    ctx:compile_to_file(kind --[[@as gccjit.OutputKind]], opts.outfile)
end
