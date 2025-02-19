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

local ffi = require("ffi")

local export = {}

---@generic T, TRet
---@param x T
---@return fun(match: { default: (fun(x: T): TRet?)?, [T] : fun(goto: fun(x: T): (TRet?)): TRet }): (TRet?)
function export.switch(x)
    return function (match)
        local m = match[x]
        local function go_to(x) return match[x] and match[x](go_to) or error("Could not goto match "..tostring(x)) end

        return m and m(go_to) or (match.default and match.default(x) or nil)
    end
end

---@generic T, TRet
---@param x T
---@return fun(match: { default?: TRet, [T] : TRet }): (TRet?)
function export.match(x)
    return function (match)
        local m = match[x]
        return m or match.default
    end
end


---@param path string
---@return boolean
function export.file_exists(path)
    local f = io.open(path, "r")
    if f then
        f:close()
        return true
    end
    return false
end

---@param cmd string
---@return boolean
function export.execute(cmd)
    print("$ "..cmd)
    local code1, _, code2 = os.execute(cmd)
    return code1 == 0 or code2 == 0
end


---@param content string
---@param dump_on_fail string?
function export.cdef(content, dump_on_fail)
    content = content:gsub("#line[^\n]*\n", ""):gsub("#include[^\n]*\n", "")


    ---@type boolean, string?
    local ok, err = pcall(ffi.cdef, content)
    if err then
        local line = tonumber(err:match("line (%d+)")) or err:match("input:(%d+)")
        if not line then error(err, 2) end

        local lines = {}
        for l in content:gmatch("([^\n]+)") do
            table.insert(lines, l)
        end

        --get the line before and after, and the line itself
        local before    = lines[line-1] or ""
        local current   = lines[line] or ""
        local after     = lines[line+1] or ""

        if dump_on_fail then
            local f = assert(io.open(dump_on_fail, "w"))
            f:write(content)
            f:close()
        end

        err = err:gsub(" at line %d+", ""):gsub("input:%d+: ", "")
        error(string.format([[
Failed to parse C definition:
%d: %s
%d: %s%s%s <- %s
%d: %s%s]], line-1, before, line, "\x1b[31m", current, "\x1b[0m", err, line+1, after, dump_on_fail and (string.format("\nSee %s:%d for more details", dump_on_fail, line)) or ""), 2)
    end
end

---@param header string
---@param flags string[]?
---@param preprocess_to string?
---@return string
function export.preprocess_header(header, flags, preprocess_to)
    ---@type string[]
    local flgs = {
        "\"-D__attribute__(...)=\"",
        "\"-D__has_feature(...)=0\"",
        "-Wno-builtin-macro-redefined",
        --make sure any clang extensions are disabled
        "-std=c11",
    }
    for k, v in ipairs(flags or {}) do
        table.insert(flgs, v)
    end

    local out_h = preprocess_to or os.tmpname()
    local cmd = string.format("%s -E -Wno-error=unused-command-line-argument -fkeep-system-includes -P -I. %s %s > %s", os.getenv("CC") or "clang", table.concat(flgs, " "), header, out_h)
    ---@type integer
    local code = os.execute(cmd)
    if code ~= 0 then
        error(string.format("Failed to preprocess C header file %s! Tried `%s`", header, cmd))
    end

    local out_f = assert(io.open(out_h, "r"))
    local content = out_f:read("*a")
    out_f:close()
    if not preprocess_to then os.remove(out_h) end

    return content
end

---@param header_path string
---@param defines { [string]: string|integer }?
---@param prefix string?
---@return ffi.namespace*
function export.load_clib(header_path, defines, prefix)
    local content = export.preprocess_header(header_path, defines, header_path..".h")
    export.cdef(content, header_path:match("([^/]+)%.h$")..".fail.h")

    local libname = header_path:match("([^/]+)%.h$")

    if not prefix then
        return ffi.load(libname)
    else
        return setmetatable({ _lib = ffi.load(libname) }, {
            __index = function(self, key)
                local ok, sym = pcall(function()
                    return self._lib[key]
                end)

                if not ok then
                    return self._lib[prefix..key]
                else
                   return sym
                end
            end,
            __newindex = function(self, key, value)
                local ok, sym = pcall(function()
                    self._lib[key] = value
                end)

                if not ok then
                    self._lib[prefix..key] = value
                end
            end
        })
    end
end

---@param T string | ffi.ctype*
---@return fun(ct: ffi.cdata*, field: string): ffi.cdata*
function export.address_of(T)
    return function(ct, field)
        ---@type integer?
        local x = ffi.offsetof(ct, field)
        if not x then
            error(string.format("Field '%s' not found in %s", field, tostring(ct)))
        end
        return ffi.cast(T, ffi.cast("uint8_t *", ct) + x)
    end
end

return export
