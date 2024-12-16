-- Copyright (C) 2024 Amrit Bhogal
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
-- 
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

---@alias codegen.c.OptimisationLevel
---| 0
---| 1
---| 2
---| 3

local visitors = {}

---@param node tl.Node
---@param ... any
---@return any
local function visit(node, ...)
    return (visitors[node.kind] or error("no visitor for node '"..node.kind.."'"))(node, ...)
end



return {
    ---@param ast tl.Node
    ---@param out_file string
    ---@param optlevel codegen.c.OptimisationLevel?
    ---@param debug boolean?
    compile = function (ast, out_file, optlevel, debug)
        ---@type string
        local code = visit(ast)

        --we are gonna compil eand link this as C code
        local tmpfile = os.tmpname()
        local f = assert(io.open(tmpfile, "w+b"))
        f:write(code)
        f:close()

        return os.execute(string.format("clang %s -O%d %s -o %s", tmpfile, optlevel or 0, ((not not debug) and "-g" or ""), out_file))
    end
}

