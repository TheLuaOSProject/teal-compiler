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

function export.switch<T>(x: T): function<TRet>(match: { T: function(go_to: function(x: T): TRet): TRet }): TRet | nil
    return function<TRet>(match: { T: function(go_to: function(x: T): TRet): TRet }): TRet
        local m = match[x]
        local function go_to(x: T): TRet return match[x](go_to) or error("Could not goto match "..tostring(x)) end

        return m and m(go_to) or nil
    end
end

function export.match<T>(x: T): function<TRet>(match: { T : TRet }): TRet | nil
    return function<TRet>(match: { T : TRet }): TRet return match[x] end
end

return export
