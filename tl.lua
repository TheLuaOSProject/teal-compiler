---@meta
---@class teal
local tl = {}

---@alias teal.Node.Kind
---|>"op"
---|>"nil"
---|>"string"
---|>"number"
---|>"integer"
---|>"boolean"
---|>"literal_table"
---|>"literal_table_item"
---|>"function"
---|>"expression_list"
---|>"enum_item"
---|>"if"
---|>"if_block"
---|>"while"
---|>"fornum"
---|>"forin"
---|>"goto"
---|>"label"
---|>"repeat"
---|>"do"
---|>"break"
---|>"return"
---|>"newtype"
---|>"argument"
---|>"type_identifier"
---|>"variable"
---|>"variable_list"
---|>"statements"
---|>"assignment"
---|>"argument_list"
---|>"local_function"
---|>"global_function"
---|>"local_type"
---|>"global_type"
---|>"record_function"
---|>"local_declaration"
---|>"global_declaration"
---|>"identifier"
---|>"cast"
---|>"..."
---|>"paren"
---|>"macroexp"
---|>"local_macroexp"
---|>"interface"
---|>"error_node"


---@alias teal.Fact.Type
---|> "is"     -- type-based type judgement (its negation implies the subtracted type)
---|> "=="     -- value-based type judgement (its negation does not imply a subtracted type negated)
---|> "not"    -- negation: type-based judgements subtract, value-based judgements prove nothing
---|> "and"    -- conjunction: type-based judgements intersect, any value-based judgement downgrades all
---|> "or"     -- disjunction: type-based judgements unite, any value-based judgement downgrades all
---|> "truthy" -- expression that is either truthy or a runtime error

---@class teal.Typename
---|> "typedecl"
---|> "typealias"
---|> "typevar"
---|> "typearg"
---|> "function"
---|> "array"
---|> "map"
---|> "tupletable"
---|> "record"
---|> "interface"
---|> "enum"
---|> "boolean"
---|> "string"
---|> "nil"
---|> "thread"
---|> "number"
---|> "integer"
---|> "union"
---|> "nominal"
---|> "emptytable"
---|> "literal_table_item"
---|> "unresolved_emptytable_value"
---|> "unresolved_typearg"
---|> "unresolvable_typearg"
---|> "circular_require"
---|> "tuple"
---|> "poly" -- intersection types, currently restricted to polymorphic functions defined inside records
---|> "any"
---|> "unknown" -- to be used in lax mode only
---|> "invalid" -- producing a new value of this type (not propagating) must always produce a type error
---|> "unresolved"
---|> "none"
---|> "*"

---@class teal.Where
---@field x integer
---@field y integer
---@field filename string?

---@class teal.Type : teal.Where
---@field typename teal.Typename
---@field typeid integer
---@field inferred_at teal.Where?
---@field needs_compat boolean?

---@class teal.StringType : teal.Type
---@field typename "string"
---@field literal string

---@class teal.Fact
---@field fact teal.Fact.Type
---@field where teal.Where

---@class teal.TruthyFact : teal.Fact
---@field fact "truthy"

---@class teal.NotFact : teal.Fact
---@field fact "not"
---@field f1 teal.Fact

---@class teal.AndFact : teal.Fact
---@field fact "and"
---@field f1 teal.Fact
---@field f2 teal.Fact

---@class teal.OrFact : teal.Fact
---@field fact "or"
---@field f1 teal.Fact
---@field f2 teal.Fact

---@class teal.EqFact : teal.Fact
---@field fact "=="
---@field var string?
---@field typ teal.Type?

---@class teal.IsFact : teal.Fact
---@field fact "is"
---@field var string?
---@field typ teal.Type?

---@class teal.Node.ExpectedContext
---@field kind teal.Node.Kind?
---@field name string?

---@alias teal.KeyParsed
---|> "short"
---|> "long"
---|> "implicit"

---@class teal.TypeArgType : teal.Type
---@field typename "typearg"
---@field typearg string?
---@field constraint teal.Type?

---@class teal.TupleType : teal.Type
---@field is_va boolean
---@field tuple teal.Type[]

---@class teal.NominalType : teal.Type
---@field typename "nominal"
---@field names string[]
---@field typevals teal.Type[]
---@field found teal.Type?
---@field resolved teal.Type?

---@class teal.TypeAliasType : teal.Type
---@field typename "typealias"
---@field alias_to teal.NominalType
---@field is_nested_alias boolean

---@class teal.TypeDeclType : teal.Type
---@field typename "typedecl"
---@field def teal.Type
---@field closed boolean

---@class teal.Operator
---@field y integer
---@field x integer
---@field airity integer
---@field op string
---@field prec integer

---@alias teal.Attribute
---|> "const"
---|> "close"
---|> "total"

---@class teal.Node : teal.Where
---@field [integer] teal.Node
---@field tk string
---@field kind teal.Node.Kind
---@field symbol_list_slot integer?
---@field semicolon boolean?
---@field hashbang string?
---@field is_longstring boolean?
---@field yend integer
---@field xend integer
---@field known teal.Fact?
---@field expected teal.Type?
---@field expected_context teal.Node.ExpectedContext?
---@field key teal.Node?
---@field value teal.Node?
---@field key_parsed teal.KeyParsed?
---@field typeargs teal.TypeArgType[]?
---@field min_arity integer?
---@field args teal.Node?
---@field rets teal.TupleType?
---@field body teal.Node?
---@field implicit_global_function boolean?
---@field is_predeclared_local_function boolean?
---@field name teal.Node
---@field is_repeat boolean?
---@field attribute teal.Attribute?
---@field fn_owner teal.Node?
---@field is_method boolean?
---@field exp teal.Node?
---@field if_parent teal.Node?
---@field if_block_n integer?
---@field if_blocks teal.Node[]?
---@field block_returns boolean?
---@field var teal.Node?
---@field from teal.Node?
---@field to teal.Node?
---@field step teal.Node?
---@field vars teal.Node?
---@field exps teal.Node?
---@field newtype teal.TypeAliasType | teal.TypeDeclType?
---@field elide_type boolean?
---@field op teal.Operator?
---@field e1 teal.Node?
---@field e2 teal.Node?
---@field constnum number?
---@field conststr string?
---@field failstore boolean?
---@field discarded_tuple boolean?
---@field receiver teal.Type?
---@field array_len integer?
---@field label string?
---@field casttype teal.Type?
---@field is_lvalue boolean?
---@field macrodef teal.Node?
---@field expanded teal.Node?
---@field argtype teal.Type?
---@field itemtype teal.Type?
---@field decltuple teal.TupleType?
---@field opt boolean?
---@field debug_type teal.Type?

---@alias teal.WarningKind
---|> "unknown"
---|> "unused"
---|> "redeclaration"
---|> "branch"
---|> "hint"
---|> "debug"

---@class teal.Error
---@field y integer
---@field x integer
---@field msg string
---@field filename string?
---@field tag teal.WarningKind
---@field private i integer

---@param input string
---@param filename string?
---@return teal.Node AST, teal.Error[] Errors, string[] Modules
function tl.parse(input, filename) end

return tl
