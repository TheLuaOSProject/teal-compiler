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

---Turn stuff like \\n into \n
---@param str string
function string.unescape(str)
    return (str:gsub("\\(.)", function (c)
        return ({
            ["n"] = "\n",
            ["t"] = "\t",
            ["r"] = "\r",
            ["\\"] = "\\"
        })[c] or c
    end))
end

local is_comparison = {
    ["=="]  = true,
    ["~="]  = true,
    ["<"]   = true,
    [">"]   = true,
    ["<="]  = true,
    [">="]  = true
}

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

        float = ctx:get_type("float"),
        double = ctx:get_type("double"),

        string = ctx:get_type("const char *"),
        varadict = ctx:new_opaque_struct_type("cvaradict"),
        void = ctx:get_type("void"),
        bool = ctx:get_type("bool")
    },
}

RAW_TYPES.integer = RAW_TYPES.c.int64_t
RAW_TYPES.boolean = RAW_TYPES.c.bool
RAW_TYPES.number = RAW_TYPES.c.double
RAW_TYPES.nil_t = RAW_TYPES.c.void

RAW_TYPES.String = ctx:new_struct_type("String", {
    ctx:new_field(RAW_TYPES.c.uint64_t, "length"),
    ctx:new_field(RAW_TYPES.c.int8_t:pointer(), "data")
})

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
---@field name string

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

---@type { [string] : Type, [integer] : Type }
local type_cache = setmetatable({}, {
    __index = function (self, k)
        if type(k) == "number" then
            for _, v in pairs(self) do
                if v.id == k then return v end
            end
        end
    end
})

---@type { [integer] : Type.Tuple }
local tuple_type_cache = {}

---@type { [integer] : Type.Function }
local function_type_cache = {}

---@param type tl.Type
---@return Type
local function conv_teal_type(type)
    local function ret_prim(name)
        -- if type_cache[name] then return function() return type_cache[name] end end

        -- local dat = {
        --     id = type.typeid,
        --     raw = RAW_TYPES[name],
        --     type = "primitive",
        --     name = name
        -- }
        -- type_cache[name] = dat
        -- pretty(dat)

        -- return function() return dat end
        return function ()
            if type_cache[name] then return type_cache[name] end

            local dat = {
                id = type.typeid,
                raw = RAW_TYPES[name],
                type = "primitive",
                name = name
            }
            type_cache[name] = dat
            return dat
        end
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
            if tuple_type_cache[type.typeid] then return tuple_type_cache[type.typeid] end

            local tuplename = "tuple-"..type.typeid

            ---@type Type.Tuple
            local tuple = {
                id = type.typeid,
                type = "tuple",
                elements = {},
            }

            for _, tl_type in ipairs(type.tuple) do
                local t = conv_teal_type(tl_type)
                local n = "tuple-element_"..#tuple.elements.."_"..tl_type.typeid
                tuple.elements[#tuple.elements+1] = {
                    id = tl_type.typeid,
                    raw = ctx:new_field(t.raw, n),
                    type = "tuple field",
                    backing_type = t,
                    name = n
                }
            end
            local friendly_name = "tuple"..type.typeid.." { "
            for i, field in ipairs(tuple.elements) do
                friendly_name = friendly_name..field.backing_type.name..(i == #tuple.elements and "" or ", ")
            end
            friendly_name = friendly_name.." }"
            tuple.name = friendly_name

            ---@type gccjit.Field*[]
            local tup_fields = {}
            for i, field in ipairs(tuple.elements) do
                tup_fields[i] = field.raw
            end

            tuple.raw = ctx:new_struct_type(tuplename, tup_fields, loc(type))
            tuple_type_cache[tuplename] = tuple
            return tuple
        end,
        ["nominal"] = function ()
            --[[@cast type tl.NominalType]]
            local names = assert(type.names)
            if names[1] == "c" then
                return utilities.switch(names[2]) {
                    pointer = function ()
                        local pt = conv_teal_type(type.typevals[1])

                        return {
                            id = type.typeid,
                            raw = pt.raw:pointer(),
                            type = "c",
                            name = pt.name..'*'
                        }
                    end,
                    const = function ()
                        local ct = conv_teal_type(type.typevals[1])

                        return {
                            id = type.typeid,
                            raw = ct.raw:const(),
                            type = "c",
                            name = "const "..ct.name
                        }
                    end,
                    default = function(x)
                        return RAW_TYPES.c[x] and {
                            id = type.typeid,
                            raw = RAW_TYPES.c[x],
                            type = "c",
                            name = x
                        } or error(string.format("Unsupported C type: %s", x))
                    end
                }
            else
                --search through the cache
                local t = type_cache[type.typeid]
                if t then return t end

                error(string.format("Unsupported nominal type \"%s\". Did you declare it?", table.concat(names, '.')))
            end
        end,
        ["function"] = function ()
            --[[@cast type tl.FunctionType]]
            if function_type_cache[type.typeid] then return function_type_cache[type.typeid] end

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
                is_varadict = type.args.is_va,
                name = string.format("function (%s): %s", table.concat(args, ", "), ret.name)
            }
            function_type_cache[type.typeid] = dat

            return dat
        end,
        ["typealias"] = function ()
            --[[@cast type tl.TypeAliasType]]
            type_cache[type.alias_to.typeid] = conv_teal_type(type.alias_to)
            return type_cache[type.typeid]
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

---@class FunctionContext.Local : FunctionContext
---@field is_external false
---@field block_stack gccjit.Block*[]
---@field variables { [string] : gccjit.LValue* }

---@class FunctionContext.External : FunctionContext
---@field is_external true

---@type { [string] : FunctionContext }
local functions = {}

---@alias Visitor.Function fun(node: tl.Node, fctx: FunctionContext.Local): ...

---@class Visitor
---@field [tl.NodeKind] Visitor.Function
local visitor = {}

---This needs to be 3 lines so its not inlined properly, making it easier to debug!
---@type Visitor.Function
local function visit(node, fctx)
    -- return (visitor[node.kind] or error(string.format("Unsupported node kind '%s' at %s", node.kind, tostring(loc(node)))))(node)
    local vtor = visitor[node.kind]
    if not vtor then
        error(string.format("Unsupported node kind '%s' at %s", node.kind, tostring(loc(node))))
    end

    --don't TCO so debug looks nicer
    return vtor(node, fctx)
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
function visitor.variable_list(node, func)
    local vars = {}
    for i, var in ipairs(node) do
        vars[i] = visit(var, func)
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
---@param fctx FunctionContext.Local
---@param scope Scope
local function declaration(node, fctx, scope)
    ---@type VariableDeclaration
    local var = visit(node.vars, fctx)[1]
    if var.attribute == "extern" then
        return extern_func_decl(node, var)
    end

    ---@type (gccjit.RValue* | gccjit.LValue*)[]?
    local val = node.exps and visit(node.exps, fctx) or nil
    ---@type gccjit.Type*?
    local ty = nil
    local tynode = conv_teal_type(node.decltuple)
    if tynode.id == type_cache.nil_t.id then
        if val then
            ty = val[1]:as_rvalue():get_type()
        end
    else
        ty = tynode.raw:as_type()
    end
    if not ty then error("Variable "..var.name.." declared without a type!") end
    local lval = fctx.raw:new_local(ty, var.name, loc(node))
    fctx.variables[var.name] = lval

    if val then
        fctx.block_stack[#fctx.block_stack]:add_assignment(lval, val[1]:as_rvalue(), loc(node))
    end

    return lval
end

function visitor.local_declaration(node, fctx)
    return declaration(node, fctx, "local")
end

function visitor.global_declaration(node, fctx)
    return declaration(node, fctx, "global")
end

function visitor.assignment(node, fctx)
    local lval = visit(node.vars, fctx)
    local rval = visit(node.exps, fctx)

    assert(ffi.typeof(lval[1]) == ffi.typeof("gcc_jit_lvalue *"), "Expected an lvalue")

    fctx.block_stack[#fctx.block_stack]:add_assignment(lval[1], rval[1]:as_rvalue(), loc(node))
end

function visitor.paren(node, fctx)
    return visit(node.e1, fctx)
end

---@return Type.Function.Parameter
function visitor.argument(node)
    return {
        name = node.tk,
        type = conv_teal_type(node.argtype)
    }
end

---@return Type.Function.Parameter[]
function visitor.argument_list(node, fctx)
    local args = {}
    for i, arg in ipairs(node) do
        args[i] = visit(arg, fctx)
    end
    return args
end

---@param node tl.Node
---@param fctx FunctionContext
---@param scope Scope
---@return gccjit.Function*
local function func(node, fctx, scope)
    if fctx then error("Cannot nest functions") end

    ---@type string
    local name = visit(node.name, fctx).name

    local ret = conv_teal_type(node.rets)
    ---@type Type.Function.Parameter[]
    local args = visit(node.args, fctx)

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
        block_stack = { root_block },
        variables = vars,
        name = name
    }
    functions[name] = fctx

    visit(node.body, fctx)

    local block = fctx.block_stack[#fctx.block_stack]
    if block then
        if ret.id == type_cache.nil_t.id then
            block:end_with_void_return(loc(node))
            fctx.block_stack[#fctx.block_stack] = nil
        else
            error("["..tostring(loc(node)).."] Function with non-nil return type must end with a return statement")
        end
    end

    func:dump_to_dot(name..".dot")
    return func
end

function visitor.local_function(node, fctx)
    return func(node, fctx, "local")
end

function visitor.global_function(node, fctx)
    return func(node, fctx, "global")
end

function visitor.statements(node, fctx)
    local statements = {}
    for _, stmt in ipairs(node) do
        local var, is_func_call = visit(stmt, fctx)
        if is_func_call then fctx.block_stack[#fctx.block_stack]:add_eval(var, loc(node)) end
        statements[#statements+1] = var
    end
    return statements
end

function visitor.expression_list(node, fctx)
    local expressions = {}
    for _, exp in ipairs(node) do
        expressions[#expressions+1] = visit(exp, fctx)
    end
    return expressions
end
--#region literals
function visitor.variable(node, fctx)
    return fctx.variables[node.tk] or functions[node.tk]
end

function visitor.integer(node)
    return ctx:new_rvalue(RAW_TYPES.integer, "long", node.constnum)
end

function visitor.number(node)
    return ctx:new_rvalue(RAW_TYPES.number, "double", node.constnum)
end

---for now will just make a literal, in the future switch to String
function visitor.string(node)
    return ctx:new_string_literal(node.conststr:unescape())
end

--#endregion

---@param func gccjit.Function*
---@param args gccjit.RValue*[]
---@param loc gccjit.Location*?
---@param fctx FunctionContext.Local
---@return gccjit.RValue*
local function function_call(func, args, loc, fctx)
    ---@type gccjit.RValue*[]
    local rawargs = {}
    for i, v in ipairs(args) do
        rawargs[i] = v:as_rvalue()
    end

    return ctx:new_call(func, rawargs, loc)
end

--this just gets the type to cast to
function visitor.cast(node)
    return conv_teal_type(node.casttype)
end

---@param node tl.Node
---@param fctx FunctionContext.Local
---@param scope Scope
---@return Type
local function typedef(node, fctx, scope)
    ---@type string
    local name = visit(node.var, fctx).name
    assert(node.value.kind == "newtype", "Expected a newtype node")
    return conv_teal_type(node.value.newtype)
end

function visitor.local_type(node, fctx)
    return typedef(node, fctx, "local")
end

function visitor.global_type(node, fctx)
    return typedef(node, fctx, "global")
end

local special_operators = {}

---@param node tl.Node
---@param fctx FunctionContext.Local
---@return gccjit.RValue*, true
function special_operators.funcall(node, fctx)
    ---@type FunctionContext.External
    local func = visit(node.e1, fctx)
    ---@type (gccjit.RValue* | gccjit.Param*)[]
    local args = visit(node.e2, fctx)
    --the `true` indicates its a function call, so the statement can be added to the block. This is a godawful hack but I dont care
    return function_call(func.raw, args, loc(node), fctx), true
end

---@param node tl.Node
---@param fctx FunctionContext.Local
---@return gccjit.RValue*
function special_operators.as(node, fctx)
    local ty = visit(node.e2, fctx)
    local val = visit(node.e1, fctx)
    return ctx:new_cast(ty.raw, val:as_rvalue(), loc(node))
end

---TODO: When tables are implemented make this also work with tables
---@param node tl.Node
---@param fctx FunctionContext.Local
function special_operators.index(node, fctx)
    ---@type gccjit.RValue*
    local tbl = visit(node.e1, fctx):as_rvalue()
    ---@type gccjit.RValue*
    local idx = visit(node.e2, fctx):as_rvalue()

    return ctx:new_array_access(tbl, idx, loc(node)) --gccjit should catch any type errors :)
end

function visitor.op(node, fctx)
    local op = assert(node.op.op, "Expected an operator")

    if op:sub(1, 1) == "@" or op == "as" then
        local specop = special_operators[op == "as" and "as" or op:sub(2)]
        if not specop then
            error("Unsupported special operator: "..op)
        end
        return specop(node, fctx)
    elseif is_comparison[op] then
        local lhs = visit(node.e1, fctx)
        local rhs = visit(node.e2, fctx)
        return ctx:new_comparison(op, lhs:as_rvalue(), rhs:as_rvalue(), loc(node))
    else
        local lhs = visit(node.e1, fctx)
        local rhs = visit(node.e2, fctx)
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

    local newidx = #fctx.block_stack+1
    fctx.block_stack[newidx] = if_block
    visit(if_block_node.body, fctx) --fill the block

    --if the block is still on the stack, then it didn't end, so it can continue to the end block
    if fctx.block_stack[newidx] then
        if_block:end_with_jump(end_block, loc(node))
        fctx.block_stack[newidx] = nil
    end

    if node.if_blocks[2] then
        else_block:add_comment("else block for function "..fctx.name, loc(node))
        newidx = #fctx.block_stack+1
        fctx.block_stack[newidx] = else_block
        visit(node.if_blocks[2].body, fctx)
        if fctx.block_stack[newidx] then
            else_block:end_with_jump(end_block, loc(node))
            fctx.block_stack[newidx] = nil
        end
    else
        else_block:end_with_jump(end_block, loc(node))
    end

    assert(fctx.block_stack[#fctx.block_stack], "Block has already ended")
    fctx.block_stack[#fctx.block_stack]:end_with_conditional(cond, if_block, else_block, loc(node))
    fctx.block_stack[#fctx.block_stack] = nil
    fctx.block_stack[#fctx.block_stack+1] = end_block
end

visitor["return"] = function (node, fctx)
    local ret = visit(node.exps[1], fctx)
    fctx.block_stack[#fctx.block_stack]:end_with_return(ret:as_rvalue(), loc(node))
    fctx.block_stack[#fctx.block_stack] = nil
end
--#endregion

return {
    compiler_context = ctx,
    visitor = visitor,
    -- compile = visit --[=[@as fun(node: tl.Node): gccjit.Object*[]]=]
    ---@param node tl.Node
    ---@return any?
    compile = function (node)
---@diagnostic disable-next-line: param-type-mismatch
        return visit(node, nil)
    end
}
