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
---@return fun(match: { default: (fun(x: T): TRet?)?, [T] : fun(goto: fun(x: T): TRet?): TRet }): TRet?
function export.match(x)
    return function (match)
        local m = match[x]
        local function go_to(x) return match[x] and match[x](go_to) or error("Could not goto match "..tostring(x)) end

        return m and m(go_to) or (match.default and match.default(x) or nil)
    end
end

return export
