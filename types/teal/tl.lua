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

---@meta

---@class tl
local tl = {}

---@class tl.Where
---@field f? string
---@field y integer
---@field x integer

---@class tl.Errors
---@field filename? string
---@field errors? tl.Error[]
---@field warnings? tl.Error[]
---@field unknown_dots? { [string] : boolean }

---@alias tl.WarningKind
---| "unknown"
---| "unused"
---| "redeclaration"
---| "branch"
---| "hint"
---| "debug"

---@alias tl.GenCompat
---| "off"
---| "optional"
---| "required"

---@alias tl.GenTarget
---| "5.1"
---| "5.3"
---| "5.4"

---@class tl.Error
---@field y? integer
---@field x? integer
---@field msg? string
---@field filename? string
---@field tag? tl.WarningKind

---@alias tl.TypeName
---| "typedecl"
---| "typealias"
---| "typevar"
---| "typearg"
---| "function"
---| "array"
---| "map"
---| "tupletable"
---| "record"
---| "interface"
---| "enum"
---| "boolean"
---| "string"
---| "nil"
---| "thread"
---| "number"
---| "integer"
---| "union"
---| "nominal"
---| "emptytable"
---| "literal_table_item"
---| "unresolved_emptytable_value"
---| "unresolved_typearg"
---| "unresolvable_typearg"
---| "circular_require"
---| "tuple"
---| "poly" intersection types, currently restricted to polymorphic functions defined inside records
---| "any"
---| "unknown" to be used in lax mode only
---| "invalid" producing a new value of this type (not propagating) must always produce a type error
---| "none"
---| "*"

---@class tl.Type : tl.Where
---@field typename? tl.TypeName
---@field typeid? integer
---@field inferred_at? tl.Where
---@field needs_compat? boolean

---@class tl.StringType : tl.Type
---@field typename? "string"
---@field literal? string

---@class tl.NumericType : tl.Type

---@class tl.IntegerType : tl.NumericType
---@field typename? "integer"

---@class tl.BooleanType : tl.Type
---@field typename? "boolean"

---@class tl.TypeDeclType : tl.Type
---@field typename? "typedecl"
---@field def? tl.Type
---@field closed? boolean
---@field typeargs? tl.TypeArgType[]

---@class tl.TypeAliasType : tl.Type
---@field typename? "typealias"
---@field alias_to? tl.NominalType
---@field is_nested_alias? boolean

---@alias tl.TypeType tl.TypeDeclType | tl.TypeAliasType

---@class tl.LiteralTableItemType : tl.Type
---@field typename? "literal_table_item"
---@field kname? string
---@field ktype? tl.Type
---@field vtype? tl.Type

---@class tl.Scope
---@field vars? { [string] : tl.Variable }
---@field labels? { [string] : tl.Node }
---@field pending_labels? { [string] : tl.Node[] }
---@field pending_nominals? { [string] : tl.NominalType[] }
---@field pending_global_types? { [string] : boolean }
---@field narrows? { [string] : boolean }

---@class tl.HasTypeArgs : tl.Type
---@field typeargs? tl.TypeArgType[]

---@class tl.HasDeclName
---@field declname? string

---@class tl.NominalType : tl.Type
---@field typename? "nominal"
---@field names? string[]
---@field typevals? tl.Type[]
---@field found? tl.TypeType
---@field resolved? tl.Type

---@class tl.ArrayLikeType : tl.Type
---@field elements? tl.Type
---@field consttypes? tl.Type[]
---@field inferred_len? integer

---@class tl.RecordLikeType : tl.Type, tl.HasTypeArgs, tl.HasDeclName, tl.ArrayLikeType
---@field interface_list? (tl.ArrayType | tl.NominalType)[]
---@field interfaces_expanded? boolean
---@field fields? { [string] : tl.Type }
---@field field_order? string[]
---@field meta_fields? { [string] : tl.Type }
---@field meta_field_order? string[]
---@field is_userdata? boolean

---@class tl.ArrayType : tl.ArrayLikeType
---@field typename? "array"

---@class tl.RecordType : tl.RecordLikeType
---@field typename? "record"

---@class tl.InterfaceType : tl.RecordLikeType
---@field typename? "interface"

---@class tl.InvalidType : tl.Type
---@field typename? "invalid"

---@class tl.UnknownType : tl.Type
---@field typename? "unknown"

---@class tl.TupleType : tl.Type
---@field typename? "tuple"
---@field is_va? boolean
---@field tuple? tl.Type[]

---@class tl.TypeArgType : tl.Type
---@field typename? "typearg"
---@field typearg? string
---@field constraint? tl.Type

---@class tl.UnresolvedTypeArgType : tl.Type
---@field typename? "unresolved_typearg"
---@field typearg? string
---@field constraint? tl.Type

---@class tl.UnresolvableTypeArgType : tl.Type
---@field typename? "unresolvable_typearg"
---@field typearg? string

---@class tl.TypeVarType : tl.Type
---@field typename? "typevar"
---@field typevar? string
---@field constraint? tl.Type

---@class tl.MapType : tl.Type
---@field typename? "map"
---@field keys? tl.Type
---@field values? tl.Type

---@class tl.EmptyTableType : tl.Type
---@field typename? "emptytable"
---@field declared_at? tl.Node
---@field assigned_to? string
---@field keys? tl.Type

---@class tl.UnresolvedEmptyTableValueType : tl.Type
---@field typename? "unresolved_emptytable_value"
---@field emptytable_type? tl.EmptyTableType

---@class tl.FunctionType : tl.Type, tl.HasTypeArgs
---@field typename? "function"
---@field is_method? boolean
---@field min_arity? integer
---@field args? tl.TupleType
---@field rets? tl.TupleType
---@field macroexp? tl.Node
---@field is_record_function? boolean

---@class tl.AggregateType : tl.Type
---@field types? tl.Type[]

---@class tl.UnionType : tl.AggregateType
---@field typename? "union"

---@class tl.TupleTableType : tl.AggregateType
---@field typename? "tupletable"

---@class tl.PolyType : tl.AggregateType
---@field typename? "poly"
---@field types? tl.FunctionType[]

---@class tl.EnumType : tl.Type, tl.HasDeclName
---@field typename? "enum"
---@field enumset? { [string] : boolean }

---@class tl.Operator
---@field y? integer
---@field x? integer
---@field arity? integer
---@field op? string
---@field prec? integer

---@alias tl.NodeKind
---| "op"
---| "nil"
---| "string"
---| "number"
---| "integer"
---| "boolean"
---| "literal_table"
---| "literal_table_item"
---| "function"
---| "expression_list"
---| "enum_item"
---| "if"
---| "if_block"
---| "while"
---| "fornum"
---| "forin"
---| "goto"
---| "label"
---| "repeat"
---| "do"
---| "break"
---| "return"
---| "newtype"
---| "argument"
---| "type_identifier"
---| "variable"
---| "variable_list"
---| "statements"
---| "assignment"
---| "argument_list"
---| "local_function"
---| "global_function"
---| "local_type"
---| "global_type"
---| "record_function"
---| "local_declaration"
---| "global_declaration"
---| "identifier"
---| "cast"
---| "..."
---| "paren"
---| "macroexp"
---| "local_macroexp"
---| "interface"
---| "error_node"

---@alias tl.FactType
---| "is"     type-based type judgement (its negation implies the subtracted type)
---| "=="     value-based type judgement (its negation does not imply a subtracted type negated)
---| "not"    negation: type-based judgements subtract, value-based judgements prove nothing
---| "and"    conjunction: type-based judgements intersect, any value-based judgement downgrades all
---| "or"     disjunction: type-based judgements unite, any value-based judgement downgrades all
---| "truthy" expression that is either truthy or a runtime error

---@class tl.Fact
---@field fact? tl.FactType
---@field w? tl.Where
---@field no_infer? boolean

---@class tl.TruthyFact : tl.Fact
---@overload fun(f1: tl.Fact, f2: tl.Fact): tl.TruthyFact
---@field fact? "truthy"

---@class tl.NotFact : tl.Fact
---@overload fun(f1: tl.Fact, f2: tl.Fact): tl.NotFact
---@field fact? "not"
---@field f1? tl.Fact

---@class tl.AndFact : tl.Fact
---@overload fun(f1: tl.Fact, f2: tl.Fact): tl.AndFact
---@field fact? "and"
---@field f1? tl.Fact
---@field f2? tl.Fact

---@class tl.OrFact : tl.Fact
---@overload fun(f1: tl.Fact, f2: tl.Fact): tl.OrFact
---@field fact? "or"
---@field f1? tl.Fact
---@field f2? tl.Fact

---@class tl.EqFact : tl.Fact
---@overload fun(f1: tl.Fact, f2: tl.Fact): tl.EqFact
---@field fact? "=="
---@field var? string
---@field typ? tl.Type

---@class tl.IsFact : tl.Fact
---@overload fun(f1: tl.Fact, f2: tl.Fact): tl.IsFact
---@field fact? "is"
---@field var? string
---@field typ? tl.Type

---@alias tl.KeyParsed
---| "short"
---| "long"
---| "implicit"

---@alias tl.Attribute string

---@alias tl.Narrow
---| "narrow"
---| "narrowed_declaration"
---| "declaration"

---@class tl.Variable
---@field t? tl.Type
---@field attribute? tl.Attribute
---@field needs_compat? boolean
---@field narrowed_from? tl.Type
---@field is_narrowed? tl.Narrow
---@field declared_at? tl.Node
---@field is_func_arg? boolean
---@field used? boolean
---@field used_as_type? boolean
---@field aliasing? tl.Variable
---@field implemented? { [string] : boolean }

---@class tl.Node.ExpectedContext
---@field kind? tl.NodeKind
---@field name? string

---@class tl.Node : tl.Where
---@field [integer] tl.Node
---@field tk? string
---@field kind tl.NodeKind
---@field symbol_list_slot? integer
---@field semicolon? boolean
---@field hashbang? string
---@field is_longstring? boolean
---@field yend? integer
---@field xend? integer
---@field known? tl.Fact
---@field expected? tl.Type
---@field expected_context? tl.Node.ExpectedContext
---@field key? tl.Node
---@field value? tl.Node
---@field key_parsed? tl.KeyParsed
---@field typeargs? tl.TypeArgType[]
---@field min_arity? integer
---@field args? tl.Node
---@field rets? tl.TupleType
---@field body? tl.Node
---@field implicit_global_function? boolean
---@field is_predeclared_local_function? boolean
---@field name? tl.Node
---@field attribute? tl.Attribute
---@field fn_owner? tl.Node
---@field is_method? boolean
---@field exp? tl.Node
---@field if_parent? tl.Node
---@field if_block_n? integer
---@field if_blocks? tl.Node[]
---@field block_returns? boolean
---@field var? tl.Node
---@field from? tl.Node
---@field to? tl.Node
---@field step? tl.Node
---@field vars? tl.Node
---@field exps? tl.Node
---@field newtype? tl.TypeType
---@field elide_type? boolean
---@field op? tl.Operator
---@field e1? tl.Node
---@field e2? tl.Node
---@field constnum? number
---@field conststr? string
---@field failstore? boolean
---@field discarded_tuple? boolean
---@field receiver? tl.Type
---@field array_len? integer
---@field is_total? boolean
---@field missing? string[]
---@field label? string
---@field used_label? boolean
---@field casttype? tl.Type
---@field is_lvalue? boolean
---@field macrodef? tl.Node
---@field expanded? tl.Node
---@field argtype? tl.Type
---@field itemtype? tl.Type
---@field decltuple? tl.TupleType
---@field opt? boolean
---@field debug_type? tl.Type

---@param contents string
---@param filename string
---@return tl.Node ast, tl.Error[] errors, string[] modules
function tl.parse(contents, filename) end

return tl
