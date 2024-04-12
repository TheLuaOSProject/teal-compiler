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
-- along with teal-compiler. If not, see <http://www.gnu.org/licenses/>.

local gccjit = require("backends.gccjit")
local ffi = require("ffi")

local utilities = require("utilities")
local pretty = require("pl.pretty")

local ctx = gccjit.Context.acquire()

local l_type = type

---@param x any
---@return type | string
local function type(x)
    local l_t = l_type(x)
    if l_t == "cdata" then
        return tostring(ffi.typeof(x))
    else
        return l_t
    end
end

---@param node tl.Where | tl.Node
---@return gccjit.Location*?
local function loc(node)
    return ctx:new_location(node.f, node.y, node.x)
end

local COMPARISON_OPERATORS = {
    "==", "~=", "<", ">", "<=", ">="
}

---@param op string
---@return boolean
local function is_comparison(op)
    local is = false
    for _, v in ipairs(COMPARISON_OPERATORS) do
        if op == v then
            is = true
            break
        end
    end
    return is
end

---@alias PrimitiveType '"integer"' | '"boolean"' | '"number"' | '"String"' | '"nil_t"'
---@alias PrimitiveTypes.C '"int8_t"' | '"int16_t"' | '"int32_t"' | '"int64_t"' | '"uint8_t"' | '"uint16_t"' | '"uint32_t"' | '"uint64_t"' | '"const char *"' | '"cvaradict"'

local RAW_TYPES = {
    c = {
        int8_t = ctx:get_type("int8_t"),
        int16_t = ctx:get_type("int16_t"),
        int32_t = ctx:get_type("int32_t"),
        int64_t = ctx:get_type("int64_t"),

        uint8_t = ctx:get_type("uint8_t"),
        uint16_t = ctx:get_type("uint16_t"),
        uint32_t = ctx:get_type("uint32_t"),
        uint64_t = ctx:get_type("uint64_t"),

        string = ctx:get_type("const char *"),
        varadict = ctx:new_opaque_struct_type("cvaradict")
    },

    integer = ctx:get_type("int64_t"),
    boolean = ctx:get_type("bool"),
    number = ctx:get_type("double"),
    String = ctx:new_struct_type("String", {
        ctx:new_field(ctx:get_type("uint64_t"), "length"),
        ctx:new_field(ctx:get_type("uint8_t"):pointer(), "data")
    }),

    nil_t = ctx:get_type("void")
}

---@type { [PrimitiveType] : Type }
local type_cache = {}

---@alias Type.Type
---| '"function"'
---| '"tuple"'
---| '"tuple field"'
---| '"primitive"'
---| '"c"'

---@class Type
---@field id integer
---@field raw gccjit.Type*
---@field type Type.Type

---@class Type.Function.Parameter
---@field name string?
---@field type Type

---@class Type.Function : Type
---@field args Type.Function.Parameter[]
---@field return_types Type[]
---@field is_varadict boolean
---@field type "function"

---@class Type.Tuple : Type
---@field elements Type.Tuple.Field[]
---@field type "tuple"

---@class Type.Tuple.Field : Type
---@field raw gccjit.Field*
---@field backing_type Type
---@field type "tuple field"

---@param ty Type.Function.Parameter[]
---@return gccjit.Type*[]
local function get_rawargtypes(ty)
    local rawargs = {}
    for _, arg in ipairs(ty) do
        rawargs[#rawargs+1] = arg.type.raw
    end
    return rawargs
end

---@type { [integer] : Type.Tuple }
local tuple_cache = {}

---@type { [integer] : Type.Function }
local function_cache = {}

---@param type tl.Type
---@return Type
local function conv_teal_type(type)
    local function ret_prim(name)
        local dat = { id = type.typeid, raw = RAW_TYPES[name], type = "primitive" }
        type_cache[name] = dat

        return function() return dat end
    end
    return utilities.switch(type.typename) {
        ["integer"] = ret_prim "integer",
        ["boolean"] = ret_prim "boolean",
        ["number"]  = ret_prim "number",
        ["string"]  = ret_prim "String",
        ["nil"]     = ret_prim "nil_t",
        ["tuple"] = function ()
            --[[@cast type tl.TupleType]]
            if #type.tuple == 0 then return type_cache.nil_t
            elseif #type.tuple == 1 then return conv_teal_type(type.tuple[1]) end
            if tuple_cache[type.typeid] then return tuple_cache[type.typeid] end

            ---@type Type.Tuple
            local tuple = {
                id = type.typeid,
                type = "tuple",
                elements = {}
            }

            for _, tl_type in ipairs(type.tuple) do
                local t = conv_teal_type(tl_type)
                tuple.elements[#tuple.elements+1] = {
                    id = tl_type.typeid,
                    raw = ctx:new_field(t.raw, "tuple-element_"..#tuple.elements.."_"..tl_type.typeid),
                    type = "tuple field",
                    backing_type = t
                }
            end

            ---@type gccjit.Field*[]
            local tup_fields = {}
            for i, field in ipairs(tuple.elements) do
                tup_fields[i] = field.raw
            end

            tuple.raw = ctx:new_struct_type("tuple-"..type.typeid, tup_fields, loc(type))
            tuple_cache[type.typeid] = tuple
            return tuple
        end,
        ["nominal"] = function ()
            --[[@cast type tl.NominalType]]
            local names = assert(type.names)
            if names[1] == "c" then
                return utilities.switch(names[2]) {
                    default = function(x)
                        return RAW_TYPES.c[x] and {
                            id = type.typeid,
                            raw = RAW_TYPES.c[x],
                            type = "c"
                        } or error(string.format("Unsupported C type: %s", x))
                    end
                }
            else
                error(string.format("Unsupported nominal type: %s", table.concat(names, '.')))
            end
        end,
        ["function"] = function ()
            --[[@cast type tl.FunctionType]]
            if function_cache[type.typeid] then return function_cache[type.typeid] end

            ---@type Type.Function.Parameter[]
            local args = {}
            for _, arg in ipairs(type.args) do
                local a = conv_teal_type(arg)
                args[#args+1] = {
                    name = arg.name,
                    type = a
                }
            end

            ---TODO: support multi-ret
            local ret = type.rets and conv_teal_type(type.rets.tuple[1]) or type_cache.nil_t
            local ftype = ctx:new_function_ptr_type(ret.raw, get_rawargtypes(args), type.args.is_va, loc(type))
            local dat = {
                id = type.typeid,
                raw = ftype,
                type = "function",
                args = args,
                return_types = {ret}, --TODO: support multi-ret
                is_varadict = type.args.is_va
            }
            function_cache[type.typeid] = dat

            return dat
        end,
        default = function(x)
            error(string.format("Unsupported type: %s", x))
        end
    }
end

---@class TypeIs
---@field rvalue fun(x: ffi.cdata*): gccjit.RValue*
---@field lvalue fun(x: ffi.cdata*): gccjit.LValue*
---@field type fun(x: ffi.cdata*): gccjit.Type*

---@type TypeIs
local type_is = setmetatable({}, {
    __index = function (self, tname)
        local expected_t = ffi.typeof("gcc_jit_"..tname.." *")
        self[tname] = function (x)
            assert(x, "Expected a value")

            local t = ffi.typeof(x)
            if x.as_rvalue and tname == "rvalue" then
                return x:as_rvalue()
            end
            assert(t == expected_t, string.format("Expected %s, got %s", expected_t, t))
            return x --[[@as any]]
        end
        return rawget(self, tname)
    end
})

--cache the results of type_is, this will fill the table
_=type_is.rvalue
_=type_is.lvalue
_=type_is.type

---@class FunctionContext
---@field name string
---@field is_external boolean
---@field raw gccjit.Function*

---@class FunctionContext.Local.Block
---@field raw gccjit.Block*
---@field ended boolean

---@class FunctionContext.Local : FunctionContext
---@field is_external false
---@field block_stack FunctionContext.Local.Block[]
---@field variables { [string] : gccjit.LValue* }

---@class FunctionContext.External : FunctionContext
---@field is_external true

---@type { [string] : FunctionContext }
local functions = {}

---@alias Visitor.Function fun(node: tl.Node, current_func: FunctionContext.Local?, ...): ...

---@class Visitor
---@field [tl.NodeKind] Visitor.Function
local visitor = {}

---@type Visitor.Function
local function visit(node, ...)
    local vtor = visitor[node.kind]
    if not vtor then error(string.format("Unsupported node kind '%s' at %s", node.kind, tostring(loc(node)))) end
    return vtor(node, ...)
end

--#region Visitor functions

---@class VariableDeclaration
---@field name string
---@field attribute string

function visitor.identifier(node)
    return {
        name = node.tk,
        attribute = node.attribute
    }
end

---@return VariableDeclaration[]
function visitor.variable_list(node)
    local vars = {}
    for i, var in ipairs(node) do
        vars[i] = visit(var)
    end
    return vars
end

---@param node tl.Node
---@param fvar VariableDeclaration
---@return gccjit.Function*
local function extern_func_decl(node, fvar)
    local ty = conv_teal_type(node.decltuple)
    if ty.type ~= "function" then error("Expected a function type") end
    --[[@cast ty Type.Function]]

    ---@type gccjit.Type*[]
    local params = {}
    for _, arg in ipairs(ty.args) do
        params[#params+1] = arg.type.raw
    end

    local fn = ctx:new_function("imported", fvar.name, ty.return_types[1].raw, params, ty.is_varadict, loc(node))
    functions[fvar.name] = {
        raw = fn,
        is_external = true,
        name = fvar.name
    }

    return fn
end

---@alias Scope "'local'" | "'global'"

---@param node tl.Node
---@param current_func FunctionContext.Local
---@param scope Scope
local function declaration(node, current_func, scope)
    ---@type VariableDeclaration
    local var = visit(node.vars)[1]
    if var.attribute == "extern" then
        return extern_func_decl(node, var)
    end
    local fctx = assert(current_func, "Expected a function context")

    ---@type (gccjit.RValue* | gccjit.LValue*)[]?
    local val = node.exps and visit(node.exps, current_func) or nil
    ---@type gccjit.Type*?
    local ty = nil
    local tynode = conv_teal_type(node.decltuple)
    if tynode.id == type_cache.nil_t.id then
        if val and #val > 0 then
            ty = val[1]:as_rvalue():get_type()
        end
    else
        ty = tynode.raw:as_type()
    end
    if not ty then error("Variable "..var.name.." declared without a type!") end
    local lval = fctx.raw:new_local(ty, var.name, loc(node))
    current_func.variables[var.name] = lval
    return lval
end

function visitor.local_declaration(node, current_func, ...)
    return declaration(node, current_func, "local")
end

function visitor.global_declaration(node, current_func, ...)
    return declaration(node, current_func, "global")
end

---@return Type.Function.Parameter
function visitor.argument(node, ...)
    return {
        name = node.tk,
        type = conv_teal_type(node.argtype)
    }
end

---@return Type.Function.Parameter[]
function visitor.argument_list(node, ...)
    local args = {}
    for i, arg in ipairs(node) do
        args[i] = visit(arg, ...)
    end
    return args
end

---@param node tl.Node
---@param current_func FunctionContext
---@param scope Scope
---@return gccjit.Function*
local function func(node, current_func, scope)
    if current_func then error("Cannot nest functions") end

    ---@type string
    local name = visit(node.name).name

    local ret = conv_teal_type(node.rets)
    ---@type Type.Function.Parameter[]
    local args = visit(node.args)

    ---@type { [string] : gccjit.LValue* }
    local vars = {}
    ---@type gccjit.Param*[]
    local params = {}
    for i, arg in ipairs(args) do
        params[i] = ctx:new_param(arg.type.raw, arg.name, loc(node))
        vars[arg.name] = params[i]
    end

    local func = ctx:new_function(scope == "local" and "internal" or "exported", name, ret.raw, params, false, loc(node))
    local root_block = func:new_block("root")

    ---@type FunctionContext.Local
    local fctx = {
        raw = func,
        is_external = false,
        block_stack = { { raw = root_block, ended = false } },
        variables = vars,
        name = name
    }
    functions[name] = fctx

    visit(node.body, fctx)

    local block = fctx.block_stack[#fctx.block_stack]
    if not block.ended then
        if ret.id == type_cache.nil_t.id then
            block.raw:end_with_void_return(loc(node))
            block.ended = true
        else
            error("["..tostring(loc(node)).."] Function with non-nil return type must end with a return statement")
        end
    end

    func:dump_to_dot(name..".dot")
    return func
end

function visitor.local_function(node, current_func)
    return func(node, current_func, "local")
end

function visitor.global_function(node, current_func)
    return func(node, current_func, "global")
end

function visitor.statements(node, ...)
    local statements = {}
    for _, stmt in ipairs(node) do
        statements[#statements+1] = visit(stmt, ...)
    end
    return statements
end

function visitor.expression_list(node, ...)
    local expressions = {}
    for _, exp in ipairs(node) do
        expressions[#expressions+1] = visit(exp, ...)
    end
    return expressions
end

-- function visitor.if_block(node, fctx)
--     ---@type gccjit.RValue*
--     local cond =
--     visit(node.body, fctx)
--     return cond
-- end

--#region Literal-likes

function visitor.variable(node, current_func)
    return assert(current_func, "Expected a function context").variables[node.tk] or functions[node.tk]
end

function visitor.integer(node)
    return ctx:new_rvalue(RAW_TYPES.integer, "long", node.constnum)
end

function visitor.number(node)
    return ctx:new_rvalue(RAW_TYPES.number, "double", node.constnum)
end

---for now will just make a literal, in the future switch to String
function visitor.string(node)
    return ctx:new_string_literal(node.conststr)
end

--#endregion

---@param func FunctionContext
---@param args gccjit.RValue*[]
---@param loc gccjit.Location*?
---@param current_func FunctionContext.Local
---@param ... any
---@return gccjit.RValue*
local function function_call(func, args, loc, current_func, ...)
    local block = current_func.block_stack[#current_func.block_stack]

    ---@type gccjit.RValue*[]
    local rawargs = {}
    for i, v in ipairs(args) do
        rawargs[i] = v:as_rvalue()
    end

    local call = ctx:new_call(func.raw, rawargs, loc)
    block.raw:add_eval(call, loc)
    return call
end

function visitor.op(node, current_func, ...)
    assert(current_func, "Expected a function context")
    local op = assert(node.op.op, "Expected an operator")
    if op == "@funcall" then
        return function_call(visit(node.e1, current_func, ...), visit(node.e2, current_func, ...), loc(node), current_func, ...)
    end

    ---@type gccjit.RValue* | gccjit.LValue*
    local lhs = visit(node.e1, current_func, ...)--:as_rvalue()
    ---@type gccjit.RValue* | gccjit.LValue*
    local rhs = visit(node.e2, current_func, ...)--:as_rvalue()

    if is_comparison(op) then
        return ctx:new_comparison(op, lhs:as_rvalue(), rhs:as_rvalue(), loc(node))
    else
        return ctx:new_binary_op(lhs:as_rvalue():get_type(), lhs:as_rvalue(), op, rhs:as_rvalue(), loc(node))
    end
end

visitor["if"] = function (node, fctx)
    assert(fctx, "Expected a function context")
    local if_block_node = assert(node.if_blocks[1], "Expected at least one if block")
    ---@type gccjit.RValue*
    local cond = visit(if_block_node.exp, fctx):as_rvalue() --just in case its a `LValue*`
    local if_block = fctx.raw:new_block("if")
    local else_block = fctx.raw:new_block("else")
    local end_block = fctx.raw:new_block("end")

    if_block:add_comment("if block for function "..fctx.name, loc(node))
    end_block:add_comment("end block for function "..fctx.name, loc(node))

    fctx.block_stack[#fctx.block_stack+1] = { raw = if_block, ended = false }
    visit(if_block_node.body, fctx)
    if not fctx.block_stack[#fctx.block_stack].ended then
        if_block:end_with_jump(end_block, loc(node))
    end
    fctx.block_stack[#fctx.block_stack] = nil

    if node.if_blocks[2] then
        else_block:add_comment("else block for function "..fctx.name, loc(node))
        fctx.block_stack[#fctx.block_stack+1] = { raw = else_block, ended = false }
        visit(node.if_blocks[2].body, fctx)
        if not fctx.block_stack[#fctx.block_stack].ended then
            else_block:end_with_jump(end_block, loc(node))
        end
        fctx.block_stack[#fctx.block_stack] = nil
    else
        else_block:end_with_jump(end_block, loc(node))
    end

    assert(not fctx.block_stack[#fctx.block_stack].ended, "Block has already ended")
    fctx.block_stack[#fctx.block_stack].raw:end_with_conditional(cond, if_block, else_block, loc(node))
    fctx.block_stack[#fctx.block_stack] = nil
    fctx.block_stack[#fctx.block_stack+1] = { raw = end_block, ended = false }
end

visitor["return"] = function (node, current_func, ...)
    local fctx = assert(current_func, "Expected a function context")
    local ret = visit(node.exps[1], fctx)
    fctx.block_stack[#fctx.block_stack].raw:end_with_return(ret:as_rvalue(), loc(node))
    fctx.block_stack[#fctx.block_stack].ended = true
end
--#endregion

function visitor.cast(node, current_func, ...)
    local to = conv_teal_type(node.casttype)
end

return {
    compiler_context = ctx,
    visitor = visitor,
    -- compile = visit --[=[@as fun(node: tl.Node): gccjit.Object*[]]=]
    ---@param node tl.Node
    ---@return any?
    compile = function (node, ...)
        return visit(node, ...)
    end
}
