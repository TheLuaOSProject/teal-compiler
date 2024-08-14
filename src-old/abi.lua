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

local export = {}

export.version = 1

---The symbol name to be used for the function, must be a valid C identifier
---@param fn_name string
---@param params {name: string, type: tl.TypeName}[]?
function export.function_name(fn_name, params)
    local symname = string.format("TEAL_V%d_NAME__%s", export.version, fn_name)

    if params then
        symname = symname.."__PARAMS"
        for _, param in ipairs(params) do
            symname = string.format("%s_NAME_%s_TYPE_%s", symname, param.name, param.type)
        end
        symname = symname.."__END"
    end

    return symname
end

return export
