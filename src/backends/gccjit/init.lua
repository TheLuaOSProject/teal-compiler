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
local libgccjit = require("backends.gccjit.cdef")

ffi.cdef [[
    void free(void *ptr);
]]

---@param x string
---@return (function | integer)?
local function sym(x)
   local ok, res = pcall(function(x) return libgccjit[x] end, x)
   if ok then return res end
end

---@class gccjit
local export = {}

---@param obj gccjit.Object*
---@return string?
function export.debug_string(obj)
    local to_obj = obj.as_object
   return to_obj and ffi.string(libgccjit.gcc_jit_object_get_debug_string(to_obj(obj))) or nil
end

---@class gccjit.Object* : ffi.cdata*
---@field as_object nil | fun(self: self): gccjit.Object*

--#region Context
---@class gccjit.Context* : gccjit.Object*
local Context = {}
Context.__index = Context

---@return gccjit.Context*
function Context.acquire()
   return ffi.gc(libgccjit.gcc_jit_context_acquire(), libgccjit.gcc_jit_context_release) --[[@as gccjit.Context*]]
end

---@alias gccjit.Options.String
---| "progname" GCC_JIT_STR_OPTION_PROGNAME
---Extras:
---| "add command line option" void gcc_jit_context_add_command_line_option (gcc_jit_context *ctxt, const char *option);
---| "add driver option" void gcc_jit_context_add_driver_option (gcc_jit_context *ctxt, const char *option);

---@alias gccjit.Options.Integer
---| "optimization level" GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL

---@alias gccjit.Options.Boolean
---| "debuginfo" GCC_JIT_BOOL_OPTION_DEBUGINFO
---| "dump initial tree" GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE
---| "dump initial gimple" GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE
---| "dump generated code" GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE
---| "dump summary" GCC_JIT_BOOL_OPTION_DUMP_SUMMARY
---| "dump everything" GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING
---| "selfcheck GC" GCC_JIT_BOOL_OPTION_SELFCHECK_GC
---| "keep intermediates" GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES
---Extras:
---| "allow unreachable blocks" void gcc_jit_context_set_bool_allow_unreachable_blocks (gcc_jit_context *ctxt, int bool_value);
---| "print errors to stderr" void gcc_jit_context_set_bool_print_errors_to_stderr (gcc_jit_context *ctxt, int enabled);
---| "use external driver" void gcc_jit_context_set_bool_use_external_driver (gcc_jit_context *ctxt, int enabled);

--overload fun(self: gccjit.Context*, option: gccjit.Options.Boolean, value: boolean)
--overload fun(self: gccjit.Context*, option: gccjit.Options.Integer, value: integer)
--overload fun(self: gccjit.Context*, option: gccjit.Options.String, value: string)
---@param option gccjit.Options.Boolean | gccjit.Options.Integer | gccjit.Options.String
---@param value boolean | integer | string
function Context:set_option(option, value)
    option = option:gsub(" ", "_"):upper()
    if type(value) == "boolean" then
        local s = sym("GCC_JIT_BOOL_OPTION_"..option)
        if not s then
            s = sym["gcc_jit_context_set_bool_"..option:lower()]
            if not s then error("Unknown option: "..option) end
            s(self, value)
        else
            libgccjit.gcc_jit_context_set_bool_option(self, s, value)
        end
    elseif type(value) == "number" then
        local s = sym("GCC_JIT_INT_OPTION_"..option)
        if not s then
            s = sym["gcc_jit_context_"..option:lower()]
            if not s then error("Unknown option: "..option) end
            s(self, value)
        else
            libgccjit.gcc_jit_context_set_int_option(self, s, value)
        end
    elseif type(value) == "string" then
        local s = sym("GCC_JIT_STR_OPTION_"..option)
        if not s then
            s = sym["gcc_jit_context_"..option:lower()]
            if not s then error("Unknown option: "..option) end
            s(self, value)
        else
            libgccjit.gcc_jit_context_set_str_option(self, s, value)
        end
    else error("Unknown option type: "..type(value)) end
end

---@return gccjit.Context*
function Context:new_child_context()
    return libgccjit.gcc_jit_context_new_child_context(self) --[[@as gccjit.Context*]]
end

---@param path string
function Context:dump_reproducer_to_file(path)
    libgccjit.gcc_jit_context_dump_reproducer_to_file(self, path)
end

---@param dumpname string
---@return ffi.cdata* char *
function Context:enable_dump(dumpname)
    local ret = ffi.new("char*[1]")
    libgccjit.gcc_jit_context_enable_dump(self, dumpname, ret)
    return ffi.gc(ret[1], ffi.C.free)
end

---@param func gccjit.Function*
---@param args gccjit.RValue*[]?
---@param location gccjit.Location*?
---@return gccjit.RValue*
function Context:new_call(func, args, location)
    local numargs = args and #args or 0
    local cargs = ffi.new("gcc_jit_rvalue*[?]", numargs)
    for i = 1, numargs do
        cargs[i-1] = (args)[i]
    end
    return libgccjit.gcc_jit_context_new_call(self, location, func, numargs, cargs) --[[@as gccjit.RValue*]]
end

---@param fn_ptr gccjit.RValue*
---@param args gccjit.RValue*[]?
---@param location gccjit.Location*?
---@return gccjit.RValue*
function Context:new_call_through_ptr(fn_ptr, args, location)
    local numargs = args and #args or 0
    local cargs = ffi.new("gcc_jit_rvalue*[?]", numargs)
    for i = 1, numargs do
        cargs[i-1] = (args)[i]
    end
    return libgccjit.gcc_jit_context_new_call_through_ptr(self, location, fn_ptr, numargs, cargs) --[[@as gccjit.RValue*]]
end

---@param type gccjit.Type*
---@param value gccjit.RValue*
---@param location gccjit.Location*?
---@return gccjit.RValue*
function Context:new_cast(type, value, location)
    return libgccjit.gcc_jit_context_new_cast(self, location, value, type) --[[@as gccjit.RValue*]]
end

---@param type gccjit.Type*
---@param value gccjit.RValue*
---@param location gccjit.Location*?
---@return gccjit.RValue*
function Context:new_bitcast(type, value, location)
    return libgccjit.gcc_jit_context_new_bitcast(self, location, type, value) --[[@as gccjit.RValue*]]
end

---@alias gccjit.Type*.Type
---| "void" GCC_JIT_TYPE_VOID
---| "void *" GCC_JIT_TYPE_VOID_PTR
---| "bool" GCC_JIT_TYPE_BOOL
---| "char" GCC_JIT_TYPE_CHAR
---| "signed char" GCC_JIT_TYPE_SIGNED_CHAR
---| "unsigned char" GCC_JIT_TYPE_UNSIGNED_CHAR
---| "short" GCC_JIT_TYPE_SHORT
---| "unsigned short" GCC_JIT_TYPE_UNSIGNED_SHORT
--- "int" GCC_JIT_TYPE_INT
---| "unsigned int" GCC_JIT_TYPE_UNSIGNED_INT
---| "long" GCC_JIT_TYPE_LONG
---| "unsigned long" GCC_JIT_TYPE_UNSIGNED_LONG
---| "long long" GCC_JIT_TYPE_LONG_LONG
---| "unsigned long long" GCC_JIT_TYPE_UNSIGNED_LONG_LONG
---| "float" GCC_JIT_TYPE_FLOAT
---| "double" GCC_JIT_TYPE_DOUBLE
---| "long double" GCC_JIT_TYPE_LONG_DOUBLE
---| "const char *" GCC_JIT_TYPE_CONST_CHAR_PTR
---| "size_t" GCC_JIT_TYPE_SIZE_T
---| "FILE *" GCC_JIT_TYPE_FILE_PTR
---| "complex float" GCC_JIT_TYPE_COMPLEX_FLOAT
---| "complex double" GCC_JIT_TYPE_COMPLEX_DOUBLE
---| "complex long double" GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE
---| "uint8_t" GCC_JIT_TYPE_UINT8
---| "uint16_t" GCC_JIT_TYPE_UINT16
---| "uint32_t" GCC_JIT_TYPE_UINT32
---| "uint64_t" GCC_JIT_TYPE_UINT64
---| "uint128_t" GCC_JIT_TYPE_UINT128
---| "int8_t" GCC_JIT_TYPE_INT8
---| "int16_t" GCC_JIT_TYPE_INT16
---| "int32_t" GCC_JIT_TYPE_INT32
---| "int64_t" GCC_JIT_TYPE_INT64
---| "int128_t" GCC_JIT_TYPE_INT128

---@overload fun(self: gccjit.Context*, tname: "int", num_bytes: integer, is_signed: boolean): gccjit.Type*
---@param tname gccjit.Type*.Type
---@return gccjit.Type*
function Context:get_type(tname, num_bytes, is_signed)
    if tname == "int" then
        return libgccjit.gcc_jit_context_get_int_type(self, num_bytes, is_signed)
    end

    tname = tname:gsub(" ", "_"):gsub("*", "PTR"):upper()
    return libgccjit.gcc_jit_context_get_type(self, libgccjit["GCC_JIT_TYPE_"..tname]) --[[@as gccjit.Type*]]
end

---@param path string
---@param update_locations boolean
function Context:dump_to_file(path, update_locations)
    libgccjit.gcc_jit_context_dump_to_file(self, path, update_locations)
end

---@return string
function Context:get_first_error()
    return ffi.string(libgccjit.gcc_jit_context_get_first_error(self))
end

---@return string
function Context:get_last_error()
    return ffi.string(libgccjit.gcc_jit_context_get_last_error(self))
end

function Context:as_object()
    return self
end

function Context:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

--#endregion

---@class gccjit.Field* : gccjit.Object*
local Field = {}
Field.__index = Field

---@param element_type gccjit.Type*
---@param num_elements integer
---@param location gccjit.Location*?
---@return gccjit.Type*
function Context:new_array_type(element_type, num_elements, location)
    return libgccjit.gcc_jit_context_new_array_type(self, location, element_type, num_elements) --[[@as gccjit.Type*]]
end

---@param type gccjit.Type*
---@param name string
---@param location gccjit.Location*?
---@return gccjit.Field*
function Context:new_field(type, name, location)
    return libgccjit.gcc_jit_context_new_field(self, location, type, name) --[[@as gccjit.Field*]]
end

---@param type gccjit.Type*
---@param name string
---@param width integer
---@param location gccjit.Location*?
---@return gccjit.Field*
function Context:new_bitfield(type, name, width, location)
    return libgccjit.gcc_jit_context_new_bitfield(self, location, type, name, width) --[[@as gccjit.Field*]]
end

function Field:as_object()
    return libgccjit.gcc_jit_field_as_object(self)
end

function Field:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.Struct* : gccjit.Type*
---@field [integer] gccjit.Field*
local Struct = {}
Struct.__index = function (self, n)
    if type(n) == "number" then
        return libgccjit.gcc_jit_struct_get_field(self, n) --[[@as gccjit.Field*]]
    end
end

---@param name string
---@param fields gccjit.Field*[]
---@param location gccjit.Location*?
---@return gccjit.Struct*
function Context:new_struct_type(name, fields, location)
    local num_fields = #fields
    local cfields = ffi.new("gcc_jit_field*[?]", num_fields)
    for i = 1, num_fields do
        cfields[i-1] = (fields)[i]
    end
    return libgccjit.gcc_jit_context_new_struct_type(self, location, name, num_fields, cfields) --[[@as gccjit.Struct*]]
end

---@param name string
---@param location gccjit.Location*?
---@return gccjit.Struct*
function Context:new_opaque_struct_type(name, location)
    return libgccjit.gcc_jit_context_new_opaque_struct(self, location, name) --[[@as gccjit.Struct*]]
end

---@param fields gccjit.Field*[]
---@param location gccjit.Location*?
function Struct:set_fields(fields, location)
    local num_fields = #fields
    local cfields = ffi.new("gcc_jit_field*[?]", num_fields)
    for i = 1, num_fields do
        cfields[i-1] = (fields)[i]
    end
    libgccjit.gcc_jit_struct_set_fields(self, location, num_fields, cfields)
end

---@param idx integer
---@return gccjit.Field*
function Struct:get_field(idx)
    return libgccjit.gcc_jit_struct_get_field(self, idx) --[[@as gccjit.Field*]]
end

---@return integer
function Struct:get_field_count()
    return libgccjit.gcc_jit_struct_get_field_count(self)
end
Struct.__len = Struct.get_field_count

function Struct:as_type()
    return libgccjit.gcc_jit_struct_as_type(self)
end

---@param type gccjit.Type*
---@param fields gccjit.Field*[]
---@param values gccjit.RValue*[]
---@param loc gccjit.Location*?
---@return gccjit.RValue*
function Context:new_struct_constructor(type, fields, values, loc)
    --fields and values must have the exact same length
    local num_fields = #fields
    assert(num_fields == #values, "fields and values must have the same length")
    local cfields = ffi.new("gcc_jit_field*[?]", num_fields)
    local cvalues = ffi.new("gcc_jit_rvalue*[?]", num_fields)
    for i = 1, num_fields do
        cfields[i-1] = fields[i]
        cvalues[i-1] = values[i]
    end

    return libgccjit.gcc_jit_context_new_struct_constructor(self, loc, type, num_fields, cfields, cvalues) --[[@as gccjit.RValue*]]
end

---@param type gccjit.Type*
---@param field gccjit.Field*
---@param value gccjit.RValue*
---@param loc gccjit.Location*?
---@return gccjit.RValue*
function Context:new_union_constructor(type, field, value, loc)
    return libgccjit.gcc_jit_context_new_union_constructor(self, loc, type, field, value) --[[@as gccjit.RValue*]]
end

---@param type gccjit.Type*
---@param fields gccjit.RValue*[]
---@param loc gccjit.Location*?
---@return gccjit.RValue*
function Context:new_array_constructor(type, fields, loc)
    local num_fields = #fields
    local cfields = ffi.new("gcc_jit_rvalue*[?]", num_fields)
    for i = 1, num_fields do
        cfields[i-1] = (fields)[i]
    end
    return libgccjit.gcc_jit_context_new_array_constructor(self, loc, type, num_fields, cfields) --[[@as gccjit.RValue*]]
end

---@param ptr gccjit.RValue*
---@param index gccjit.RValue*
---@param loc gccjit.Location*?
---@return gccjit.LValue*
function Context:new_array_access(ptr, index, loc)
    return libgccjit.gcc_jit_context_new_array_access(self, loc, ptr, index) --[[@as gccjit.LValue*]]
end

function Struct:as_object()
    return libgccjit.gcc_jit_struct_as_object(self)
end

function Struct:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.Type* : ffi.cdata*
local Type = {}
Type.__index = Type

---@param name string
---@param fields gccjit.Field*[]
---@param location gccjit.Location*?
---@return gccjit.Type*
function Context:new_union_type(name, fields, location)
    local num_fields = #fields
    local cfields = ffi.new("gcc_jit_field*[?]", num_fields)
    for i = 1, num_fields do
        cfields[i-1] = (fields)[i]
    end
    return libgccjit.gcc_jit_context_new_union_type(self, location, name, num_fields, cfields) --[[@as gccjit.Type*]]
end

---@param return_type gccjit.Type*
---@param param_types gccjit.Type*[]
---@param is_variadic boolean
---@param location gccjit.Location*?
---@return gccjit.Type*
function Context:new_function_ptr_type(return_type, param_types, is_variadic, location)
    local num_params = #param_types
    local cparams = ffi.new("gcc_jit_type*[?]", num_params)
    for i = 1, num_params do
        cparams[i-1] = (param_types)[i]
    end
    return libgccjit.gcc_jit_context_new_function_ptr_type(self, location, return_type, num_params, cparams, not not is_variadic) --[[@as gccjit.Type*]]
end

---@return gccjit.Type*
function Type:pointer()
    return libgccjit.gcc_jit_type_get_pointer(self) --[[@as gccjit.Type*]]
end

---@return gccjit.Type*
function Type:const()
    return libgccjit.gcc_jit_type_get_const(self) --[[@as gccjit.Type*]]
end

---@return gccjit.Type*
function Type:volatile()
    return libgccjit.gcc_jit_type_get_volatile(self) --[[@as gccjit.Type*]]
end

---@return integer
function Type:get_size()
    return libgccjit.gcc_jit_type_get_size(self)
end

---@param t2 gccjit.Type*
---@return boolean
function Type:is_compatable_with(t2)
    return libgccjit.gcc_jit_type_is_compatible(self, t2) ~= 0
end
Type.__eq = Type.is_compatable_with

---@param bytes integer
---@return gccjit.Type*
function Type:aligned(bytes)
    return libgccjit.gcc_jit_type_get_aligned(self, bytes) --[[@as gccjit.Type*]]
end

---@param n integer
---@return gccjit.Type*
function Type:vector(n)
    return libgccjit.gcc_jit_type_get_vector(self, n) --[[@as gccjit.Type*]]
end

---@return gccjit.Type*
function Type:unqualified()
    return libgccjit.gcc_jit_type_unqualified(self) --[[@as gccjit.Type*]]
end

---@return gccjit.Type*?
function Type:dyncast_array()
    return libgccjit.gcc_jit_type_dyncast_array(self) --[[@as gccjit.Type*]]
end

---@return boolean
function Type:is_bool()
    return libgccjit.gcc_jit_type_is_boolean(self) ~= 0
end

---@return boolean
function Type:is_integral()
    return libgccjit.gcc_jit_type_is_integral(self) ~= 0
end

---@return boolean
function Type:is_pointer()
    return libgccjit.gcc_jit_type_is_pointer(self) ~= 0
end

---@return boolean
function Type:is_struct()
    return libgccjit.gcc_jit_type_is_struct(self) ~= 0
end

function Type:as_type()
    return self
end

function Type:as_object()
    return libgccjit.gcc_jit_type_as_object(self)
end

function Type:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.VectorType* : gccjit.Type*
local VectorType = {}
VectorType.__index = VectorType

---@return gccjit.VectorType*?
function Type:dyncast_vector()
    return libgccjit.gcc_jit_type_dyncast_vector(self) --[[@as gccjit.VectorType*]]
end

---@return integer
function VectorType:get_num_units()
    return libgccjit.gcc_jit_vector_type_get_num_units(self)
end

---@return gccjit.Type*
function VectorType:get_element_type()
    return libgccjit.gcc_jit_vector_type_get_element_type(self) --[[@as gccjit.Type*]]
end

---@class gccjit.FunctionType* : gccjit.Type*
local FunctionType = {}
FunctionType.__index = FunctionType

---@return gccjit.FunctionType*?
function Type:dyncast_function_ptr()
    return libgccjit.gcc_jit_type_dyncast_function_ptr(self) --[[@as gccjit.FunctionType*]]
end

---@return gccjit.Type*
function FunctionType:get_return_type()
    return libgccjit.gcc_jit_function_type_get_return_type(self) --[[@as gccjit.Type*]]
end

---@return integer
function FunctionType:get_param_count()
    return libgccjit.gcc_jit_function_type_get_param_count(self)
end

---@param idx integer
---@return gccjit.Type*
function FunctionType:get_param(idx)
    return libgccjit.gcc_jit_function_type_get_param(self, idx) --[[@as gccjit.Type*]]
end

---@param vec_t gccjit.Type*
---@param elems gccjit.RValue*[]
---@param loc gccjit.Location*?
function Context:new_rvalue_from_vector(vec_t, elems, loc)
    local num_elems = #elems
    local celems = ffi.new("gcc_jit_rvalue*[?]", num_elems)
    for i = 1, num_elems do
        celems[i-1] = (elems)[i]
    end
    return libgccjit.gcc_jit_context_new_rvalue_from_vector(self, loc, vec_t, num_elems, celems) --[[@as gccjit.RValue*]]
end

---@class gccjit.Location* : ffi.cdata*
local Location = {}
Location.__index = Location

---@param filename string
---@param line integer
---@param column integer
---@return gccjit.Location*
function Context:new_location(filename, line, column)
    return libgccjit.gcc_jit_context_new_location(self, filename, line, column) --[[@as gccjit.Location*]]
end

function Location:as_object()
    return libgccjit.gcc_jit_location_as_object(self)
end

function Location:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.RValue* : gccjit.Object*
local RValue = {}
RValue.__index = RValue

---@param field gccjit.Field*
---@param location gccjit.Location*?
---@return gccjit.RValue*
function RValue:access_field(field, location)
    return libgccjit.gcc_jit_rvalue_access_field(self, location, field)
end

---@return gccjit.Type*
function RValue:get_type()
    return libgccjit.gcc_jit_rvalue_get_type(self)
end

---@param field gccjit.Field*
---@param location gccjit.Location*?
---@return gccjit.LValue*
function RValue:dereference_field(field, location)
    return libgccjit.gcc_jit_rvalue_dereference_field(self, location, field)
end

---@param location gccjit.Location*?
---@return gccjit.LValue*
function RValue:dereference(location)
    return libgccjit.gcc_jit_rvalue_dereference(self, location)
end

---@param cond boolean
function RValue:set_require_tailcall(cond)
    libgccjit.gcc_jit_rvalue_set_bool_require_tailcall(self, cond)
end

---@param str string
---@return gccjit.RValue*
function Context:new_string_literal(str)
    return libgccjit.gcc_jit_context_new_string_literal(self, str)
end

---@param kind gccjit.GlobalKind
---@param name string
---@param type gccjit.Type*
---@param location gccjit.Location*?
---@return gccjit.LValue*
function Context:new_global(kind, type, name, location)
    return libgccjit.gcc_jit_context_new_global(self, location, libgccjit["GCC_JIT_GLOBAL_"..kind:gsub(" ", "_"):upper()], type, name)
end

function RValue:as_object()
    return libgccjit.gcc_jit_rvalue_as_object(self)
end

function RValue:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---Utility func
function RValue:as_rvalue()
    return self
end

---@class gccjit.Param* : gccjit.LValue*
local Param = {}
Param.__index = Param

---@return gccjit.RValue*
function Param:as_rvalue()
    return libgccjit.gcc_jit_param_as_rvalue(self) --[[@as gccjit.RValue*]]
end

---@param type gccjit.Type*
---@param value_type "int" | "long" | "double" | "pointer"
---@param value integer | ffi.cdata*
function Context:new_rvalue(type, value_type, value)
    if value_type == "int" then
        return libgccjit.gcc_jit_context_new_rvalue_from_int(self, type, value) --[[@as gccjit.RValue*]]
    elseif value_type == "long" then
        return libgccjit.gcc_jit_context_new_rvalue_from_long(self, type, value) --[[@as gccjit.RValue*]]
    elseif value_type == "double" then
        return libgccjit.gcc_jit_context_new_rvalue_from_double(self, type, value) --[[@as gccjit.RValue*]]
    elseif value_type == "pointer" then
        return libgccjit.gcc_jit_context_new_rvalue_from_pointer(self, type, value) --[[@as gccjit.RValue*]]
    else error("Unknown value type: "..value_type) end
end

---@param type gccjit.Type*
---@return gccjit.RValue*
function Context:zero(type)
    return libgccjit.gcc_jit_context_zero(self, type) --[[@as gccjit.RValue*]]
end

---@param type gccjit.Type*
---@return gccjit.RValue*
function Context:one(type)
    return libgccjit.gcc_jit_context_one(self, type) --[[@as gccjit.RValue*]]
end

---@param type gccjit.Type*
function Context:null(type)
    return libgccjit.gcc_jit_context_null(self, type) --[[@as gccjit.RValue*]]
end


---@alias gccjit.UnaryOperation
---| "minus"
---| "-" GCC_JIT_UNARY_OP_MINUS
---| "bitwise negate"
---| "~" GCC_JIT_UNARY_OP_BITWISE_NEGATE
---| "logical negate"
---| "!" GCC_JIT_UNARY_OP_LOGICAL_NEGATE

---@param op gccjit.UnaryOperation
---@param result_type gccjit.Type*
---@param value gccjit.RValue*
---@param location gccjit.Location*?
---@return gccjit.RValue*
function Context:new_unary_op(op, result_type, value, location)
    op = op :gsub("-", "minus")
            :gsub("~", "bitwise negate")
            :gsub("!", "logical negate")
            :gsub(" ", "_"):upper()
    return libgccjit.gcc_jit_context_new_unary_op(self, location, libgccjit["GCC_JIT_UNARY_OP_"], result_type, value) --[[@as gccjit.RValue*]]
end

---@alias gccjit.Comparison
---| "eq"
---| "==" GCC_JIT_COMPARISON_EQ
---| "ne"
---| "!=" GCC_JIT_COMPARISON_NE
---| "lt"
---| "<" GCC_JIT_COMPARISON_LT
---| "le"
---| "<=" GCC_JIT_COMPARISON_LE
---| "gt"
---| ">" GCC_JIT_COMPARISON_GT
---| "ge"
---| ">=" GCC_JIT_COMPARISON_GE

---@param op gccjit.Comparison
---@param lhs gccjit.RValue*
---@param rhs gccjit.RValue*
---@param location gccjit.Location*?
---@return gccjit.RValue*
function Context:new_comparison(op, lhs, rhs, location)
    op = op :gsub("==", "eq")
            :gsub("!=", "ne")
            :gsub("<=", "le")
            :gsub(">=", "ge")
            :gsub("<", "lt")
            :gsub(">", "gt")
            :gsub(" ", "_"):upper()
    return libgccjit.gcc_jit_context_new_comparison(self, location, libgccjit["GCC_JIT_COMPARISON_"..op], lhs, rhs) --[[@as gccjit.RValue*]]
end

---@alias gccjit.BinaryOperation
---| "plus"
---| "+" GCC_JIT_BINARY_OP_PLUS
---| "mult"
---| "*" GCC_JIT_BINARY_OP_MULT
---| "divide"
---| "/" GCC_JIT_BINARY_OP_DIVIDE
---| "modulo"
---| "%" GCC_JIT_BINARY_OP_MODULO
---| "bitwise and"
---| "&" GCC_JIT_BINARY_OP_BITWISE_AND
---| "bitwise or"
---| "|" GCC_JIT_BINARY_OP_BITWISE_OR
---| "bitwise xor"
---| "^" GCC_JIT_BINARY_OP_BITWISE_XOR
---| "left shift"
---| "<<" GCC_JIT_BINARY_OP_LEFT_SHIFT
---| "right shift"
---| ">>" GCC_JIT_BINARY_OP_RIGHT_SHIFT
---| "logical and"
---| "&&" GCC_JIT_BINARY_OP_LOGICAL_AND
---| "logical or"
---| "||" GCC_JIT_BINARY_OP_LOGICAL_OR

---@param op gccjit.BinaryOperation
---@param result_type gccjit.Type*
---@param lhs gccjit.RValue*
---@param rhs gccjit.RValue*
---@param location gccjit.Location*?
---@return gccjit.RValue*
function Context:new_binary_op(result_type, lhs, op, rhs, location)
    op = op :gsub("%+", "plus")
            :gsub("%-", "minus")
            :gsub("%*", "mult")
            :gsub("%/", "divide")
            :gsub("%%", "modulo")
            :gsub("%&", "bitwise and")
            :gsub("%|", "bitwise or")
            :gsub("%^", "bitwise xor")
            :gsub("<<", "left shift")
            :gsub(">>", "right shift")
            :gsub("%&%&", "logical and")
            :gsub("%|%|", "logical or")
            :gsub(" ", "_"):upper()
    return libgccjit.gcc_jit_context_new_binary_op(self, location, libgccjit["GCC_JIT_BINARY_OP_"..op], result_type, lhs, rhs) --[[@as gccjit.RValue*]]
end

---@param name string
---@param type gccjit.Type*
---@param location gccjit.Location*?
---@return gccjit.Param*
function Context:new_param(type, name, location)
    return libgccjit.gcc_jit_context_new_param(self, location, type, name) --[[@as gccjit.Param*]]
end

function Param:as_object()
    return libgccjit.gcc_jit_param_as_object(self)
end

function Param:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.Function* : gccjit.Object*
local Function = {}
Function.__index = Function

---@alias gccjit.Function*.Kind
---| "exported" GCC_JIT_FUNCTION_EXPORTED
---| "internal" GCC_JIT_FUNCTION_INTERNAL
---| "imported" GCC_JIT_FUNCTION_IMPORTED
---| "always inline" GCC_JIT_FUNCTION_ALWAYS_INLINE

---@alias gccjit.TLSModel
---| "none" GCC_JIT_TLS_MODEL_NONE
---| "global dynamic" GCC_JIT_TLS_MODEL_GLOBAL_DYNAMIC
---| "local dynamic" GCC_JIT_TLS_MODEL_LOCAL_DYNAMIC
---| "initial exec" GCC_JIT_TLS_MODEL_INITIAL_EXEC
---| "local exec" GCC_JIT_TLS_MODEL_LOCAL_EXEC

---@param kind gccjit.Function*.Kind
---@param name string
---@param return_type gccjit.Type*
---@param params gccjit.Param*[]?
---@param is_variadic boolean?
---@param location gccjit.Location*?
---@return gccjit.Function*
function Context:new_function(kind, name, return_type, params, is_variadic, location)
    local num_params = params and #params or 0
    local cparams = ffi.new("gcc_jit_param*[?]", num_params)
    for i = 1, num_params do
        cparams[i-1] = (params)[i]
    end
    return libgccjit.gcc_jit_context_new_function(self, location, libgccjit["GCC_JIT_FUNCTION_"..kind:upper()], return_type, name, num_params, cparams, not not is_variadic) --[[@as gccjit.Function*]]
end

---@param name string
---@return gccjit.Function*
function Context:get_builtin_function(name)
    return libgccjit.gcc_jit_context_get_builtin_function(self, name) --[[@as gccjit.Function*]]
end

---@param path string
function Function:dump_to_dot(path)
    libgccjit.gcc_jit_function_dump_to_dot(self, path)
end

---@param loc gccjit.Location*?
---@return gccjit.RValue*
function Function:get_address(loc)
    return libgccjit.gcc_jit_function_get_address(self, loc) --[[@as gccjit.RValue*]]
end

---@return gccjit.Type*
function Function:get_return_type()
    return libgccjit.gcc_jit_function_get_return_type(self) --[[@as gccjit.Type*]]
end

---@return integer
function Function:get_param_count()
    return libgccjit.gcc_jit_function_get_param_count(self)
end

function Function:as_object()
    return libgccjit.gcc_jit_function_as_object(self)
end

function Function:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.Block* : gccjit.Object*
local Block = {}
Block.__index = Block

---@param name string?
---@return gccjit.Block*
function Function:new_block(name)
    return libgccjit.gcc_jit_function_new_block(self, name) --[[@as gccjit.Block*]]
end

---@alias gccjit.GlobalKind
---| "exported" GCC_JIT_GLOBAL_EXPORTED
---| "internal" GCC_JIT_GLOBAL_INTERNAL
---| "imported" GCC_JIT_GLOBAL_IMPORTED

---@param rvalue gccjit.RValue*
---@param location gccjit.Location*?
function Block:add_eval(rvalue, location)
    libgccjit.gcc_jit_block_add_eval(self, location, rvalue)
end

---@param to gccjit.LValue*
---@param from gccjit.RValue*
---@param location gccjit.Location*?
function Block:add_assignment(to, from, location)
    libgccjit.gcc_jit_block_add_assignment(self, location, to, from)
end

---@param to gccjit.LValue*
---@param op gccjit.BinaryOperation
---@param from gccjit.RValue*
---@param location gccjit.Location*?
function Block:add_assignment_op(to, op, from, location)
    op = op :gsub("%+", "plus")
            :gsub("%-", "minus")
            :gsub("%*", "mult")
            :gsub("%/", "divide")
            :gsub("%%", "modulo")
            :gsub("%&", "bitwise and")
            :gsub("%|", "bitwise or")
            :gsub("%^", "bitwise xor")
            :gsub("<<", "left shift")
            :gsub(">>", "right shift")
            :gsub("%&%&", "logical and")
            :gsub("%|%|", "logical or")
            :gsub(" ", "_"):upper()

    libgccjit.gcc_jit_block_add_assignment_op(self, location, to, libgccjit["GCC_JIT_BINARY_OP_"..op], from)
end

---@param text string
---@param location gccjit.Location*?
function Block:add_comment(text, location)
    libgccjit.gcc_jit_block_add_comment(self, location, text)
end

---@param cond gccjit.RValue*
---@param on_true gccjit.Block*
---@param on_false gccjit.Block*
---@param location gccjit.Location*?
function Block:end_with_conditional(cond, on_true, on_false, location)
    libgccjit.gcc_jit_block_end_with_conditional(self, location, cond, on_true, on_false)
end

---@param target gccjit.Block*
---@param location gccjit.Location*?
function Block:end_with_jump(target, location)
    return libgccjit.gcc_jit_block_end_with_jump(self, location, target)
end

---@param value gccjit.RValue*
---@param location gccjit.Location*?
function Block:end_with_return(value, location)
    return libgccjit.gcc_jit_block_end_with_return(self, location, value)
end

---@param location gccjit.Location*?
function Block:end_with_void_return(location)
    return libgccjit.gcc_jit_block_end_with_void_return(self, location)
end

---@param expr gccjit.RValue*
---@param on_default gccjit.Block*
---@param cases gccjit.Case*[]
function Block:end_with_switch(expr, on_default, cases, loc)
    local num_cases = #cases
    local ccases = ffi.new("gcc_jit_case*[?]", num_cases)
    for i = 1, num_cases do
        ccases[i-1] = cases[i]
    end
    return libgccjit.gcc_jit_block_end_with_switch(self, loc, expr, on_default, num_cases, ccases)
end

function Block:as_object()
    return libgccjit.gcc_jit_block_as_object(self)
end

function Block:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.ExtendedAssembly* : gccjit.Object*
local ExtendedAssembly = {}
ExtendedAssembly.__index = ExtendedAssembly

---@param asm_templ string
---@param location gccjit.Location*?
---@return gccjit.ExtendedAssembly*
function Block:add_extended_asm(asm_templ, location)
    return libgccjit.gcc_jit_block_add_extended_asm(self, location, asm_templ) --[[@as gccjit.ExtendedAssembly*]]
end

---@param asm_templ string
---@param goto_blocks gccjit.Block*[]
---@param fallthrough_block gccjit.Block*
---@param location gccjit.Location*?
---@return gccjit.ExtendedAssembly*
function Block:end_with_extended_asm_goto(asm_templ, goto_blocks, fallthrough_block, location)
    local num_goto_blocks = #goto_blocks
    local cgoto_blocks = ffi.new("gcc_jit_block*[?]", num_goto_blocks)
    for i = 1, num_goto_blocks do
        cgoto_blocks[i-1] = (goto_blocks)[i]
    end
    return libgccjit.gcc_jit_block_end_with_extended_asm_goto(self, location, asm_templ, num_goto_blocks, cgoto_blocks, fallthrough_block) --[[@as gccjit.ExtendedAssembly*]]
end

---@param x boolean
function ExtendedAssembly:set_volatile(x)
    return libgccjit.gcc_jit_extended_asm_set_volatile(self, x)
end

---@param x boolean
function ExtendedAssembly:set_inline(x)
    return libgccjit.gcc_jit_extended_asm_set_inline(self, x)
end

---@param asmname string
---@param constraint string
---@param dest gccjit.LValue*
function ExtendedAssembly:add_output_operand(asmname, constraint, dest)
    return libgccjit.gcc_jit_extended_asm_add_output_operand(self, asmname, constraint, dest)
end

---@param asmname string
---@param constraint string
---@param src gccjit.RValue*
function ExtendedAssembly:add_input_operand(asmname, constraint, src)
    return libgccjit.gcc_jit_extended_asm_add_input_operand(self, asmname, constraint, src)
end

---@param target string
function ExtendedAssembly:add_clobber(target)
    return libgccjit.gcc_jit_extended_asm_add_clobber(self, target)
end

---@param asmname string
---@param loc gccjit.Location*?
function ExtendedAssembly:add_top_level_asm(asmname, loc)
    return libgccjit.gcc_jit_extended_asm_add_top_level_asm(self, loc, asmname)
end

function ExtendedAssembly:as_object()
    return libgccjit.gcc_jit_extended_asm_as_object(self)
end

function ExtendedAssembly:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.Case* : gccjit.Object*
local Case = {}
Case.__index = Case

---@param min gccjit.RValue*
---@param max gccjit.RValue*
---@param dest gccjit.Block*
function Context:new_case(min, max, dest)
    return libgccjit.gcc_jit_context_new_case(self, min, max, dest) --[[@as gccjit.Case*]]
end

function Case:as_object()
    return libgccjit.gcc_jit_case_as_object(self)
end

function Case:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.LValue* : gccjit.RValue*
local LValue = {}
LValue.__index = LValue

---@param type gccjit.Type*
---@param name string
---@param location gccjit.Location*?
---@return gccjit.LValue*
function Function:new_local(type, name, location)
    return libgccjit.gcc_jit_function_new_local(self, location, type, name) --[[@as gccjit.LValue*]]
end


---@param bytes integer
function LValue:set_alignment(bytes)
    return libgccjit.gcc_jit_lvalue_set_alignment(self, bytes)
end

---@return integer
function LValue:get_alignment()
    return libgccjit.gcc_jit_lvalue_get_alignment(self)
end

---@param field gccjit.Field*
---@param location gccjit.Location*?
---@return gccjit.LValue*
function LValue:access_field(field, location)
    return libgccjit.gcc_jit_lvalue_access_field(self, location, field) --[[@as gccjit.LValue*]]
end

---@param location gccjit.Location*?
---@return gccjit.RValue*
function LValue:get_address(location)
    return libgccjit.gcc_jit_lvalue_get_address(self, location) --[[@as gccjit.RValue*]]
end

---@param tls_model gccjit.TLSModel
function LValue:set_tls_model(tls_model)
    return libgccjit.gcc_jit_lvalue_set_tls_model(self, libgccjit["GCC_JIT_TLS_MODEL_"..tls_model:gsub(" ", "_"):upper()])
end

---@param section string
function LValue:set_link_section(section)
    return libgccjit.gcc_jit_lvalue_set_link_section(self, section)
end

---@param name string
function LValue:set_register_name(name)
    return libgccjit.gcc_jit_lvalue_set_register_name(self, name)
end

---@return gccjit.RValue*
function LValue:as_rvalue()
    return libgccjit.gcc_jit_lvalue_as_rvalue(self) --[[@as gccjit.RValue*]]
end

function LValue:as_object()
    return libgccjit.gcc_jit_lvalue_as_object(self)
end

function LValue:__tostring()
    return ffi.string(libgccjit.gcc_jit_object_get_debug_string(self:as_object()))
end

---@class gccjit.Result* : gccjit.Object*
local Result = {}
Result.__index = Result

---@return gccjit.Result*?
function Context:compile()
    return ffi.gc(libgccjit.gcc_jit_context_compile(self), libgccjit.gcc_jit_result_release) --[[@as gccjit.Result*]]
end

---@alias gccjit.OutputKind
---| "assembler" GCC_JIT_OUTPUT_KIND_ASSEMBLER
---| "object file" GCC_JIT_OUTPUT_KIND_OBJECT_FILE
---| "dynamic library" GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
---| "executable" GCC_JIT_OUTPUT_KIND_EXECUTABLE

---@param kind gccjit.OutputKind
---@param to string
function Context:compile_to_file(kind, to)
    libgccjit.gcc_jit_context_compile_to_file(self, libgccjit["GCC_JIT_OUTPUT_KIND_"..kind:gsub(" ", "_"):upper()], to)
end


---@param symbol string
---@param type string | ffi.ctype*
---@return ffi.cdata*?
function Result:get_code(symbol, type)
    local code = libgccjit.gcc_jit_result_get_code(self, symbol)
    if code == nil then return nil end
    local ct = ffi.typeof(type --[[@as ffi.ctype*]])
    return ffi.cast(ct, code)
end

---@param symbol string
---@param type string
---@return ffi.cdata*?
function Result:get_global(symbol, type)
    local global = libgccjit.gcc_jit_result_get_global(self, symbol)
    if global == nil then return nil end
    local ct = ffi.typeof(type --[[@as ffi.ctype*]])
    return ffi.cast(ct, global)
end

---@class gccjit.Timer* : ffi.cdata*
local Timer = {}
Timer.__index = Timer

---@return gccjit.Timer*
function Timer.new()
    return ffi.gc(libgccjit.gcc_jit_timer_new(), libgccjit.gcc_jit_timer_release) --[[@as gccjit.Timer*]]
end

---@param timer gccjit.Timer*
function Context:set_timer(timer)
    libgccjit.gcc_jit_context_set_timer(self, timer)
end

---@return gccjit.Timer*
function Context:get_timer()
    return libgccjit.gcc_jit_context_get_timer(self) --[[@as gccjit.Timer*]]
end

---@param name string
function Timer:push(name)
    libgccjit.gcc_jit_timer_push(self, name)
end

---@param name string
function Timer:pop(name)
    libgccjit.gcc_jit_timer_pop(self, name)
end

export.Type         = ffi.metatype("gcc_jit_type", Type) --[[@as gccjit.Type*]]
export.Context      = ffi.metatype("gcc_jit_context", Context) --[[@as gccjit.Context*]]
export.Field        = ffi.metatype("gcc_jit_field", Field) --[[@as gccjit.Field*]]
export.Struct   = ffi.metatype("gcc_jit_struct", Struct) --[[@as gccjit.Struct*]]
export.Location     = ffi.metatype("gcc_jit_location", Location) --[[@as gccjit.Location*]]
export.RValue       = ffi.metatype("gcc_jit_rvalue", RValue) --[[@as gccjit.RValue*]]
export.Param        = ffi.metatype("gcc_jit_param", Param) --[[@as gccjit.Param*]]
export.Function     = ffi.metatype("gcc_jit_function", Function) --[[@as gccjit.Function*]]
export.Block        = ffi.metatype("gcc_jit_block", Block) --[[@as gccjit.Block*]]
export.Case         = ffi.metatype("gcc_jit_case", Case) --[[@as gccjit.Case*]]
export.LValue       = ffi.metatype("gcc_jit_lvalue", LValue) --[[@as gccjit.LValue*]]
export.Result       = ffi.metatype("gcc_jit_result", Result) --[[@as gccjit.Result*]]
export.Timer        = ffi.metatype("gcc_jit_timer", Timer) --[[@as gccjit.Timer*]]
export.ExtendedAssembly = ffi.metatype("gcc_jit_extended_asm", ExtendedAssembly) --[[@as gccjit.ExtendedAssembly*]]
export.VectorType   = ffi.metatype("gcc_jit_vector_type", VectorType) --[[@as gccjit.VectorType*]]
export.FunctionType = ffi.metatype("gcc_jit_function_type", FunctionType) --[[@as gccjit.FunctionType*]]
export.libgccjit    = libgccjit
return export
