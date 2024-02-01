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

---@generic T, TRet
---@param x T
---@return fun(match: { default: (fun(x: T): TRet?)?, [T] : fun(): TRet }): TRet?
function export.match(x)
    return function (match)
        local m = match[x]
        if m then return type(m) == "function" and m() or m
        else return match.default(x) end
    end
end

return export
