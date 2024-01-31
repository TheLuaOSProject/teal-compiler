// Copyright (C) 2024 Amrit Bhogal
//
// This file is part of teal-compiler.
//
// teal-compiler is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// teal-compiler is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with teal-compiler.  If not, see <http://www.gnu.org/licenses/>.

///
/// Raw 1-1 port of the Teal types to C++
///

///Teal synopsis
/*

local enum TypeName
   "typedecl"
   "typealias"
   "typevar"
   "typearg"
   "function"
   "array"
   "map"
   "tupletable"
   "record"
   "interface"
   "enum"
   "boolean"
   "string"
   "nil"
   "thread"
   "number"
   "integer"
   "union"
   "nominal"
   "emptytable"
   "literal_table_item"
   "unresolved_emptytable_value"
   "unresolved_typearg"
   "unresolvable_typearg"
   "circular_require"
   "tuple"
   "poly" -- intersection types, currently restricted to polymorphic functions defined inside records
   "any"
   "unknown" -- to be used in lax mode only
   "invalid" -- producing a new value of this type (not propagating) must always produce a type error
   "none"
   "*"
end

local table_types <total>: {TypeName:boolean} = {
   ["array"] = true,
   ["map"] = true,
   ["record"] = true,
   ["interface"] = true,
   ["emptytable"] = true,
   ["tupletable"] = true,

   ["typedecl"] = false,
   ["typealias"] = false,
   ["typevar"] = false,
   ["typearg"] = false,
   ["function"] = false,
   ["enum"] = false,
   ["boolean"] = false,
   ["string"] = false,
   ["nil"] = false,
   ["thread"] = false,
   ["number"] = false,
   ["integer"] = false,
   ["union"] = false,
   ["nominal"] = false,
   ["literal_table_item"] = false,
   ["unresolved_emptytable_value"] = false,
   ["unresolved_typearg"] = false,
   ["unresolvable_typearg"] = false,
   ["circular_require"] = false,
   ["tuple"] = false,
   ["poly"] = false,
   ["any"] = false,
   ["unknown"] = false,
   ["invalid"] = false,
   ["none"] = false,
   ["*"] = false,
}

local interface Type
   is Where
   where self.typename

   y: integer
   x: integer

   typename: TypeName    -- discriminator
   typeid: integer       -- unique identifier
   inferred_at: Where    -- for error messages
   needs_compat: boolean -- for Lua compatibilty
end

local record StringType
   is Type
   where self.typename == "string"

   literal: string
end

local function is_numeric_type(t:Type): boolean
   return t.typename == "number" or t.typename == "integer"
end

local interface NumericType
   is Type
   where is_numeric_type(self)
end

local record IntegerType
   is NumericType
   where self.typename == "integer"
end

local record BooleanType
   is Type
   where self.typename == "boolean"
end

local record TypeDeclType
   is Type
   where self.typename == "typedecl"

   def: Type
   closed: boolean
end

local record TypeAliasType
   is Type
   where self.typename == "typealias"

   alias_to: NominalType
   is_nested_alias: boolean
end

local type TypeType = TypeDeclType | TypeAliasType

local record LiteralTableItemType
   is Type
   where self.typename == "literal_table_item"

   -- table items
   kname: string
   ktype: Type
   vtype: Type
end

local record Scope
   vars: {string:Variable}
   labels: {string:Node}
   pending_labels: {string:{Node}}
   pending_nominals: {string:{NominalType}}
   pending_global_types: {string:boolean}
   narrows: {string:boolean}
end

local interface HasTypeArgs
   is Type
   where self.typeargs

   typeargs: {TypeArgType}
end

local interface HasDeclName
   declname: string
end

local record NominalType
   is Type
   where self.typename == "nominal"

   names: {string}
   typevals: {Type}
   found: TypeType      -- type is found but typeargs are not resolved
   resolved: Type       -- type is found and typeargs are resolved
end

local interface ArrayLikeType
   is Type
   where self.elements

   elements: Type
   consttypes: {Type}
   inferred_len: integer
end

local interface RecordLikeType
   is Type, HasTypeArgs, HasDeclName, ArrayLikeType
   where self.fields

   interface_list: {ArrayType | NominalType}
   interfaces_expanded: boolean
   fields: {string: Type}
   field_order: {string}
   meta_fields: {string: Type}
   meta_field_order: {string}
   is_userdata: boolean
end

local record ArrayType
   is ArrayLikeType
   where self.typename == "array"
end

local record RecordType
   is RecordLikeType
   where self.typename == "record"
end

local record InterfaceType
   is RecordLikeType
   where self.typename == "interface"
end

local record InvalidType
   is Type
   where self.typename == "invalid"
end

local record UnknownType
   is Type
   where self.typename == "unknown"
end

local record TupleType
   is Type
   where self.typename == "tuple"

   is_va: boolean
   tuple: {Type}
end

local record TypeArgType
   is Type
   where self.typename == "typearg"

   typearg: string
   constraint: Type
end

local record UnresolvedTypeArgType
   is Type
   where self.typename == "unresolved_typearg"

   typearg: string
   constraint: Type
end

local record UnresolvableTypeArgType
   is Type
   where self.typename == "unresolvable_typearg"

   typearg: string
end

local record TypeVarType
   is Type
   where self.typename == "typevar"

   typevar: string
   constraint: Type
end

local record MapType
   is Type
   where self.typename == "map"

   keys: Type
   values: Type
end

local record EmptyTableType
   is Type
   where self.typename == "emptytable"

   declared_at: Node
   assigned_to: string
   keys: Type
end

local record UnresolvedEmptyTableValueType
   is Type
   where self.typename == "unresolved_emptytable_value"

   emptytable_type: EmptyTableType
end

local record FunctionType
   is Type, HasTypeArgs
   where self.typename == "function"

   is_method: boolean
   min_arity: integer
   args: TupleType
   rets: TupleType
   macroexp: Node
end

local interface AggregateType
   is Type
   where self.types

   types: {Type}
end

local record UnionType
   is AggregateType
   where self.typename == "union"
end

local record TupleTableType
   is AggregateType
   where self.typename == "tupletable"
end

local record PolyType
   is AggregateType
   where self.typename == "poly"

   types: {FunctionType}
end

local record EnumType
   is Type, HasDeclName
   where self.typename == "enum"

   enumset: {string:boolean}
end

local record Operator
   y: integer
   x: integer
   arity: integer
   op: string
   prec: integer
end

local enum NodeKind
   "op"
   "nil"
   "string"
   "number"
   "integer"
   "boolean"
   "literal_table"
   "literal_table_item"
   "function"
   "expression_list"
   "enum_item"
   "if"
   "if_block"
   "while"
   "fornum"
   "forin"
   "goto"
   "label"
   "repeat"
   "do"
   "break"
   "return"
   "newtype"
   "argument"
   "type_identifier"
   "variable"
   "variable_list"
   "statements"
   "assignment"
   "argument_list"
   "local_function"
   "global_function"
   "local_type"
   "global_type"
   "record_function"
   "local_declaration"
   "global_declaration"
   "identifier"
   "cast"
   "..."
   "paren"
   "macroexp"
   "local_macroexp"
   "interface"
   "error_node"
end

local enum FactType
   "is"     -- type-based type judgement (its negation implies the subtracted type)
   "=="     -- value-based type judgement (its negation does not imply a subtracted type negated)
   "not"    -- negation: type-based judgements subtract, value-based judgements prove nothing
   "and"    -- conjunction: type-based judgements intersect, any value-based judgement downgrades all
   "or"     -- disjunction: type-based judgements unite, any value-based judgement downgrades all
   "truthy" -- expression that is either truthy or a runtime error
end

local interface Fact
   where self.fact

   fact: FactType
   w: Where
   no_infer: boolean
end

local record TruthyFact
   is Fact
   where self.fact == "truthy"

   metamethod __call: function(Fact, Fact): TruthyFact
end

local record NotFact
   is Fact
   where self.fact == "not"

   f1: Fact

   metamethod __call: function(Fact, Fact): NotFact
end

local record AndFact
   is Fact
   where self.fact == "and"

   f1: Fact
   f2: Fact

   metamethod __call: function(Fact, Fact): AndFact
end

local record OrFact
   is Fact
   where self.fact == "or"

   f1: Fact
   f2: Fact

   metamethod __call: function(Fact, Fact): OrFact
end

local record EqFact
   is Fact
   where self.fact == "=="

   var: string
   typ: Type

   metamethod __call: function(Fact, Fact): EqFact
end

local record IsFact
   is Fact
   where self.fact == "is"

   var: string
   typ: Type

   metamethod __call: function(Fact, Fact): IsFact
end

local enum KeyParsed
   "short"
   "long"
   "implicit"
end

local enum Attribute
   "const"
   "close"
   "total"
end

local attributes <total>: {Attribute: boolean} = {
   ["const"] = true,
   ["close"] = true,
   ["total"] = true,
}
local is_attribute <const>: {string:boolean} = attributes as {string:boolean}

local record Node
   is {Node}, Where
   where self.kind ~= nil

   record ExpectedContext
      kind: NodeKind
      name: string
   end

   tk: string
   kind: NodeKind
   symbol_list_slot: integer
   semicolon: boolean
   hashbang: string

   is_longstring: boolean

   yend: integer
   xend: integer

   known: Fact

   -- bidirectional inference
   expected: Type
   expected_context: Node.ExpectedContext

   key: Node
   value: Node
   key_parsed: KeyParsed

   typeargs: {TypeArgType}
   min_arity: integer
   args: Node
   rets: TupleType
   body: Node
   implicit_global_function: boolean
   is_predeclared_local_function: boolean

   name: Node

   -- statements list in a `repeat`, delay closing scope
   is_repeat: boolean

   -- var declaration
   attribute: Attribute

   fn_owner: Node
   is_method: boolean

   exp: Node
   if_parent: Node
   if_block_n: integer
   if_blocks: {Node}
   block_returns: boolean

   -- fornum
   var: Node
   from: Node
   to: Node
   step: Node

   -- forin
   vars: Node
   exps: Node

   -- newtype
   newtype: TypeType
   elide_type: boolean

   -- expressions
   op: Operator
   e1: Node
   e2: Node
   constnum: number
   conststr: string
   failstore: boolean
   discarded_tuple: boolean
   receiver: Type

   -- table literal
   array_len: integer
   is_total: boolean
   missing: {string}

   -- goto
   label: string

   -- label
   used_label: boolean

   casttype: Type

   -- variable
   is_lvalue: boolean

   -- macroexp
   macrodef: Node
   expanded: Node

   argtype: Type
   itemtype: Type
   decltuple: TupleType

   opt: boolean

   debug_type: Type
end

*/

#pragma once

namespace teal::raw
{

    static struct Teal {
        sol::state lua_state;
        sol::table teal;

        Teal()
        {
            lua_state.open_libraries();
            teal = lua_state.do_file("teal/tl.lua");
        }

        std::tuple<sol::table, sol::table, sol::table> parse(const std::string &input, const std::string &filename)
        { return teal["parse"](input, filename); }
    } TEAL;

    //Every teal type can be nillable (because fuck you), so every field also must be optional here (because fuck you)
    template<typename T>
    using Optional = std::optional<T>;

    // teal type aliases
    using integer = Optional<int64_t>;
    using number = Optional<double>;
    using boolean = Optional<bool>;
    using string = Optional<std::string>;


    //No value can be represented as `nullptr`
    template<typename T>
    using Pointer = DxPtr::omni_ptr<T>;
    // using Pointer = std::shared_ptr<T>; //must be shared because of weird casting stuff!
    // using Pointer = std::unique_ptr<T>;
    template<typename T>
    using Array = Optional<std::vector<T>>;
    template<typename ...T>
    using Union = Optional<std::variant<T...>>;
    template<typename TKey, typename TValue>
    using Map = Optional<std::unordered_map<TKey, TValue>>;

    using Table = Optional<sol::table>;

    struct Where {
        integer y;
        integer x;
        string f;

        // virtual ~Where() = default; // technically it is an interface but it doesn't make sense to be
    };

    struct Error {
        integer y;
        integer x;
        string msg;
        string filename;

        string tag;

        //ignore
        integer i;

        inline std::string to_string() const
        { return std::format("{}:{}:{} - ({}): {}", *filename, *y, *x, *tag, *msg); }
    };

    struct Errors {
        string filename;
        Array<Error> errors;
        Array<Error> warnings;
        Map<string, boolean> unknown_dots;
    };

    enum class Narrow {
        NARROW,
        NARROWED_DECLARATION,
        DECLARATION,
    };

    enum class Attribute {
        CONST,
        CLOSE,
        TOTAL
    };

    struct Node;
    struct Type;
    struct Variable {
        Pointer<Type> t;
        Optional<Attribute> attribute;
        boolean needs_compat;
        Pointer<Type> narrowed_from;
        boolean is_narrowed;
        Pointer<Node> declared_at;
        boolean is_func_arg;
        boolean used;
        boolean used_as_type;
        Pointer<Variable> aliasing;
        Map<string, boolean> implemented;
    };

#pragma region Types
    enum class TypeName {
        TYPEDECL,
        TYPEALIAS,
        TYPEVAR,
        TYPEARG,
        FUNCTION,
        ARRAY,
        MAP,
        TUPLETABLE,
        RECORD,
        INTERFACE,
        ENUM,
        BOOLEAN,
        STRING,
        NIL,
        THREAD,
        NUMBER,
        INTEGER,
        UNION,
        NOMINAL,
        EMPTYTABLE,
        LITERAL_TABLE_ITEM,
        UNRESOLVED_EMPTYTABLE_VALUE,
        UNRESOLVED_TYPEARG,
        UNRESOLVABLE_TYPEARG,
        CIRCULAR_REQUIRE,
        TUPLE,
        POLY,
        ANY,
        UNKNOWN,
        INVALID,
        NONE,
        STAR
    };

    struct Type : Where {
        TypeName type_name;
        integer type_id;
        Optional<Where> inferred_at;
        boolean needs_compat;

        virtual ~Type() = default;
    };

    struct StringType final : Type {
        TypeName type_name = TypeName::STRING;
        string literal;
    };

    struct NumericType : Type {
        virtual ~NumericType() = default;
    };

    struct IntegerType final : NumericType {
        TypeName type_name = TypeName::INTEGER;
    };

    struct BooleanType final : Type {
        TypeName type_name = TypeName::BOOLEAN;
    };

    struct TypeDeclType : Type {
        TypeName type_name = TypeName::TYPEDECL;
        Pointer<Type> def;
        boolean closed;

        ~TypeDeclType() = default;
    };

    struct NominalType final : Type {
        Array<std::string> names;
        Array<Pointer<Type>> typevals;
        Pointer<Type> found;
        Pointer<Type> resolved;
    };

    struct ArrayLikeType : Type {
        Pointer<Type> elements;
        Array<Pointer<Type>> consttypes;
        integer inferred_len;

        virtual ~ArrayLikeType() = default;
    };

    struct TypeAliasType : Type {
        TypeName type_name = TypeName::TYPEALIAS;

        Pointer<NominalType> alias_to;
        boolean is_nested_alias;

        ~TypeAliasType() = default;
    };

    struct LiteralTableItemType final : Type {
        TypeName type_name = TypeName::LITERAL_TABLE_ITEM;

        string kname;
        Pointer<Type> ktype;
        Pointer<Type> vtype;
    };
    struct [[gnu::unused]] Scope {
        Map<string, Variable> vars;
        Map<string, Pointer<Node>> labels;
        Map<string, Array<Pointer<Node>>> pending_labels;
        Map<string, Array<Pointer<NominalType>>> pending_nominals;
        Map<string, boolean> pending_global_types;
        Map<string, boolean> narrows;
    };

    struct HasTypeArgs {
        Array<Pointer<Type>> typeargs;

        virtual ~HasTypeArgs() = default;
    };

    struct HasDeclName {
        string declname;

        virtual ~HasDeclName() = default;
    };

    struct HasIsTotal {
        boolean is_total;
        Array<string> missing;

        virtual ~HasIsTotal() = default;
    };

    struct ArrayType final : ArrayLikeType {
        TypeName type_name = TypeName::ARRAY;
    };

    struct RecordLikeType : HasTypeArgs, HasDeclName, ArrayLikeType {
        //This is really an Array<Union<ArrayType, NominalType>> but it is faster and easier to do it like this
        Array<Pointer<Type>> interface_list;
        boolean interfaces_expanded;
        Map<string, Pointer<Type>> fields;
        Array<string> field_order;
        Map<string, Pointer<Type>> meta_fields;
        Array<string> meta_field_order;
        boolean is_userdata;

        virtual ~RecordLikeType() = default;
    };

    struct RecordType final : RecordLikeType, HasIsTotal {
        TypeName type_name = TypeName::RECORD;
    };

    struct InterfaceType final : RecordLikeType {
        TypeName type_name = TypeName::INTERFACE;
    };

    struct InvalidType final : Type {
        TypeName type_name = TypeName::INVALID;
    };

    struct UnknownType final : Type {
        TypeName type_name = TypeName::UNKNOWN;
    };

    struct TupleType final : Type {
        TypeName type_name = TypeName::TUPLE;

        boolean is_va;
        Array<Pointer<Type>> tuple;
    };

    struct TypeArgType final : Type {
        TypeName type_name = TypeName::TYPEARG;

        string typearg;
        Pointer<Type> constraint;
    };

    struct UnresolvedTypeArgType final : Type {
        TypeName type_name = TypeName::UNRESOLVED_TYPEARG;

        string typearg;
        Pointer<Type> constraint;
    };

    struct UnresolvableTypeArgType final : Type {
        TypeName type_name = TypeName::UNRESOLVABLE_TYPEARG;

        string typearg;
    };

    struct TypeVarType final : Type {
        TypeName type_name = TypeName::TYPEVAR;

        string typevar;
        Pointer<Type> constraint;
    };

    struct MapType final : Type, HasIsTotal {
        TypeName type_name = TypeName::MAP;

        Pointer<Type> keys;
        Pointer<Type> values;
    };

    struct EmptyTableType final : Type {
        TypeName type_name = TypeName::EMPTYTABLE;

        Pointer<Node> declared_at;
        string assigned_to;
        Pointer<Type> keys;
    };

    struct UnresolvedEmptyTableValueType final : Type {
        TypeName type_name = TypeName::UNRESOLVED_EMPTYTABLE_VALUE;

        Pointer<EmptyTableType> emptytable_type;
    };

    struct FunctionType final : Type, HasTypeArgs {
        TypeName type_name = TypeName::FUNCTION;

        boolean is_method;
        integer min_arity;
        Pointer<TupleType> args;
        Pointer<TupleType> rets;
        Pointer<Node> macroexp;
    };

    struct AggregateType : Type {
        Array<Pointer<Type>> types;

        virtual ~AggregateType() = default;
    };

    struct UnionType final : AggregateType {
        TypeName type_name = TypeName::UNION;
    };

    struct TupleTableType final : AggregateType {
        TypeName type_name = TypeName::TUPLETABLE;
    };

    struct PolyType final : AggregateType {
        TypeName type_name = TypeName::POLY;

        Array<Pointer<FunctionType>> types;
    };

    struct EnumType final : Type, HasDeclName {
        TypeName type_name = TypeName::ENUM;

        Map<string, boolean> enumset;
    };

#pragma endregion

    struct Operator {
        integer y;
        integer x;
        integer arity;
        string op;
        integer prec;
    };

#pragma region Nodes
    enum class NodeKind {
        OP,
        NIL,
        STRING,
        NUMBER,
        INTEGER,
        BOOLEAN,
        LITERAL_TABLE,
        LITERAL_TABLE_ITEM,
        FUNCTION,
        EXPRESSION_LIST,
        ENUM_ITEM,
        IF,
        IF_BLOCK,
        WHILE,
        FORNUM,
        FORIN,
        GOTO,
        LABEL,
        REPEAT,
        DO,
        BREAK,
        RETURN,
        NEWTYPE,
        ARGUMENT,
        TYPE_IDENTIFIER,
        VARIABLE,
        VARIABLE_LIST,
        STATEMENTS,
        ASSIGNMENT,
        ARGUMENT_LIST,
        LOCAL_FUNCTION,
        GLOBAL_FUNCTION,
        LOCAL_TYPE,
        GLOBAL_TYPE,
        RECORD_FUNCTION,
        LOCAL_DECLARATION,
        GLOBAL_DECLARATION,
        IDENTIFIER,
        CAST,
        VARARGS, //...
        PAREN,
        MACROEXP,
        LOCAL_MACROEXP,
        INTERFACE,
        ERROR_NODE,
    };

    enum class FactType {
        IS,
        EQ, //==
        NOT,
        AND,
        OR,
        TRUTHY
    };

    struct Fact {
        FactType fact;
        Where w;
        boolean no_infer;

        virtual ~Fact() = default;
    };

    struct TruthyFact : Fact {
        FactType fact = FactType::TRUTHY;
    };

    struct NotFact : Fact {
        FactType fact = FactType::NOT;
        Pointer<Fact> f1;
    };

    struct AndFact : Fact {
        FactType fact = FactType::AND;
        Pointer<Fact> f1;
        Pointer<Fact> f2;
    };

    struct OrFact : Fact {
        FactType fact = FactType::OR;
        Pointer<Fact> f1;
        Pointer<Fact> f2;
    };

    struct EqFact : Fact {
        FactType fact = FactType::EQ;
        string var;
        Pointer<Type> typ;
    };

    struct IsFact : Fact {
        FactType fact = FactType::IS;
        string var;
        Pointer<Type> typ;
    };

    enum class KeyParsed {
        SHORT,
        LONG,
        IMPLICIT
    };

    struct Node {
        struct ExpectedContext {
            Optional<NodeKind> kind;
            string name;
        };

        Array<Pointer<Node>> children;

        string tk;
        Optional<NodeKind> kind;
        integer symbol_list_slot;
        boolean semicolon;
        string hashbang;
        boolean is_longstring;
        integer yend;
        integer xend;

        Pointer<Fact> known;
        Pointer<Type> expected;
        Optional<ExpectedContext> expected_context;

        Pointer<Node> key;
        Pointer<Node> value;
        Optional<KeyParsed> key_parsed;

        Array<TypeArgType> typeargs;
        integer min_arity;
        Pointer<Node> args;
        Pointer<TupleType> rets;
        Pointer<Node> body;
        boolean implicit_global_function;
        boolean is_predeclared_local_function;

        Pointer<Node> name;

        boolean is_repeat;

        Optional<Attribute> attribute;

        Pointer<Node> fn_owner;
        boolean is_method;

        Pointer<Node> exp;
        Pointer<Node> if_parent;
        integer if_block_n;
        Array<Pointer<Node>> if_blocks;
        boolean block_returns;

        Pointer<Node> var;
        Pointer<Node> from;
        Pointer<Node> to;
        Pointer<Node> step;

        Pointer<Node> vars;
        Pointer<Node> exps;

        // Pointer<Type> newtype;
        // Union<TypeAliasType, TypeDeclType> newtype;
        // Union<TypeAliasType, TypeDeclType, std::nullopt_t> newtype;
        Optional<Union<Pointer<TypeAliasType>, Pointer<TypeDeclType>>> newtype; //Pointer to union so `std::move` can be done. Yes, this is a bad idea.
        boolean elide_type;

        Optional<Operator> op;
        Pointer<Node> e1;
        Pointer<Node> e2;

        number constnum;
        string conststr;
        boolean failstore;
        boolean discarded_tuple;
        Pointer<Type> receiver;

        integer array_len;

        string label;
        boolean used_label;

        Pointer<Type> casttype;

        boolean is_lvalue;

        Pointer<Node> macrodef;
        Pointer<Node> expanded;

        Pointer<Type> argtype;
        Pointer<Type> itemtype;
        Pointer<TupleType> decltuple;

        boolean opt;

        Pointer<Type> debug_type;

        constexpr Node &operator[](size_t index) const
        { return *children.value().at(index).get(); }

        Node() = default;
        Node(Node &&) = default;

        static std::tuple<Pointer<Node>, sol::table> convert_from_lua(const std::string &input, const std::string &filename = "");
    };

    class ConvertException : public std::exception {
    private:
        std::vector<Error> _errors;
        std::string _what;

    public:
        explicit ConvertException(std::vector<Error> errors) : _errors(std::move(errors))
        {
            std::stringstream ss;
            ss << "Parse Exception: " << std::endl;
            for (const auto &error : _errors) {
                ss << "    " << error.to_string() << std::endl;
            }
            _what = ss.str();
        }

        [[nodiscard]] const char *what() const noexcept override {
            return _what.c_str();
        }

        [[nodiscard]] const std::vector<Error> &errors() const noexcept {
            return _errors;
        }
    };

#pragma endregion
}
