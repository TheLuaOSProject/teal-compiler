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

#include "RawAST.hpp"
#include "utilities.hpp"

#include <algorithm>

using std::nullopt;
using namespace std::string_literals;
using namespace teal::raw;

template<typename T>
[[nodiscard]]
static T convert(const Table &node)
{ static_assert(sizeof(T) == 0, "Use specialisations of this function"); }


template<>
[[nodiscard]]
Pointer<Node> convert<Pointer<Node>>(const Table &node);

// [[nodiscard]]
// static Error convert_error(const sol::table &error)
template<>
[[nodiscard]]
Optional<Error> convert<Optional<Error>>(const Table &error)
{
    if (not error.has_value() or not error->valid() or error->get_type() != sol::type::table) return nullopt;
    Error result;
    result.msg = error->get<string>("msg");
    result.tag = error->get<string>("tag");
    result.filename = error->get<string>("filename");
    result.y = error->get<integer>("y");
    result.x = error->get<integer>("x");
    return result;
}

template<typename T>
static inline Optional<T> convert_enum(const string &s)
{
    if (not s.has_value()) return nullopt;

    std::string kind = *s; //copy so `std::transform` can work
    //Match the enum kind to the real C++ enum
    std::transform(kind.begin(), kind.end(), kind.begin(), ::toupper);

    return magic_enum::enum_cast<T>(kind);
}

template<>
[[nodiscard]]
Optional<Where> convert<Optional<Where>>(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table) return nullopt;

    return Where {
        .y = node->get<integer>("y"),
        .x = node->get<integer>("x"),
        .f = node->get<string>("filename")
    };
}

template<>
[[nodiscard]]
string convert<string>(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::string) return "";

    return node->as<string>();
}


template<typename T>
[[nodiscard]]
Array<T> convert_array(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table) return nullopt;

    auto result = std::vector<T>(node->size());
    for (size_t i = 0; i < node->size(); i++) {
        auto x = (*node)[i + 1];
        if (x.get_type() == sol::type::table)
            result.emplace_back(convert<T>(x));
    }
    return result;
}

template<typename TKey, typename TValue>
[[nodiscard]]
Map<TKey, TValue> convert_map(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table) return nullopt;

    auto result = std::unordered_map<TKey, TValue>();
    for (auto &pair : *node) {
        auto key = pair.first;
        auto value = pair.second;

        result.emplace(convert<TKey>(key), convert<TValue>(value));
    }
    return result;
}

template<>
[[nodiscard]]
Array<std::string> convert_array<std::string>(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table) return nullopt;

    auto result = std::vector<std::string>(node->size());
    for (size_t i = 0; i < node->size(); i++) {
        auto x = (*node)[i + 1];
        if (x.get_type() == sol::type::string)
            result.emplace_back(x);
    }
    return result;
}

template<>
[[nodiscard]]
Pointer<Type> convert<Pointer<Type>>(const Table &node)
{
    if (not node.has_value()
     or not node->valid()
     or node->get_type() != sol::type::table
     or not node->get<string>("type_name").has_value()) return nullptr;

    Pointer<Type> result = nullptr;

    auto type_name = convert_enum<TypeName>(*node->get<string>("type_name"));
    if (not type_name.has_value()) return nullptr;

    const auto convert_array_like_type = [](const Table &node, ArrayLikeType *to) {
        to->elements = convert<Pointer<Type>>(node->get<Table>("elements"));
        to->consttypes = convert_array<Pointer<Type>>(node->get<Table>("consttypes"));
        to->inferred_len = node->get<integer>("inferred_len");
    };

    const auto parse_record_like_type = [](const Table &node, RecordLikeType *to) {
        to->interface_list = ({ // I cant care enough to make a proper specialisation
            auto tbl = node->get<Table>("interface_list").value();
            auto result = std::vector<Pointer<Type>>(tbl.size());

            for (size_t i = 0; i < tbl.size(); i++) {
                auto x = tbl.get<sol::table>(i+1);
                if (x.get_type() == sol::type::table) {
                    result.emplace_back(convert<Pointer<Type>>(x));
                }
            }

            std::move(result);
        });
        to->interfaces_expanded = node->get<boolean>("interfaces_expanded");
        to->fields              = convert_map<string, Pointer<Type>>(node->get<Table>("fields"));
        to->field_order         = convert_array<string>(node->get<Table>("field_order"));
        to->meta_fields         = convert_map<string, Pointer<Type>>(node->get<Table>("meta_fields"));
        to->meta_field_order    = convert_array<string>(node->get<Table>("meta_field_order"));
        to->is_userdata         = node->get<boolean>("is_userdata");
    };

    switch (*type_name) {
    case TypeName::STRING: {
        auto s = DxPtr::make_omni<StringType>();
        s->literal = node->get<string>("literal");
        result = std::move(s);
        break;
    }

    case TypeName::INTEGER: {
        result = DxPtr::make_omni<IntegerType>();
        break;
    }

    case TypeName::BOOLEAN: {
        result = DxPtr::make_omni<BooleanType>();
        break;
    }

    case TypeName::TYPEDECL: {
        auto s = DxPtr::make_omni<TypeDeclType>();
        s->def = convert<Pointer<Type>>(node->get<Table>("def"));
        s->closed = node->get<boolean>("closed");
        result = std::move(s);
        break;
    }

    case TypeName::NOMINAL: {
        auto s = DxPtr::make_omni<NominalType>();
        s->names = convert_array<std::string>(node->get<Table>("names"));
        s->typevals = convert_array<Pointer<Type>>(node->get<Table>("typevals"));
        s->found = convert<Pointer<Type>>(node->get<Table>("found"));
        s->resolved = convert<Pointer<Type>>(node->get<Table>("resolved"));
        result = std::move(s);
        break;
    }

    case TypeName::LITERAL_TABLE_ITEM: {
        auto s = DxPtr::make_omni<LiteralTableItemType>();
        s->kname = node->get<string>("kname");
        s->ktype = convert<Pointer<Type>>(node->get<Table>("ktype"));
        s->vtype = convert<Pointer<Type>>(node->get<Table>("vtype"));
        result = std::move(s);
        break;
    }

    case TypeName::ARRAY: {
        auto s = DxPtr::make_omni<ArrayType>();
        convert_array_like_type(node, s.get());
        result = std::move(s);
        break;
    }

    case TypeName::RECORD: {
        auto s = DxPtr::make_omni<RecordType>();
        parse_record_like_type(node, s.get());
        result = std::move(s);
        break;
    }

    case TypeName::INTERFACE: {
        auto s = DxPtr::make_omni<InterfaceType>();
        parse_record_like_type(node, s.get());
        result = std::move(s);
        break;
    }

    case TypeName::INVALID: {
        auto s = DxPtr::make_omni<InvalidType>();
        result = std::move(s);
        break;
    }

    case TypeName::UNKNOWN: {
        auto s = DxPtr::make_omni<UnknownType>();
        result = std::move(s);
        break;
    }

    case TypeName::TUPLE: {
        auto s = DxPtr::make_omni<TupleType>();
        s->is_va = node->get<boolean>("is_va");
        s->tuple = convert_array<Pointer<Type>>(node->get<Table>("tuple"));
        result = std::move(s);
        break;
    }

    case TypeName::TYPEARG: {
        auto s = DxPtr::make_omni<TypeArgType>();
        s->typearg = node->get<string>("typearg");
        s->constraint = convert<Pointer<Type>>(node->get<Table>("constraint"));
        result = std::move(s);
        break;
    }

    case TypeName::UNRESOLVED_TYPEARG: {
        auto s = DxPtr::make_omni<UnresolvedTypeArgType>();
        s->typearg = node->get<string>("typearg");
        s->constraint = convert<Pointer<Type>>(node->get<Table>("constraint"));
        result = std::move(s);
        break;
    }

    case TypeName::UNRESOLVABLE_TYPEARG: {
        auto s = DxPtr::make_omni<UnresolvableTypeArgType>();
        s->typearg = node->get<string>("typearg");
        result = std::move(s);
        break;
    }

    case TypeName::TYPEVAR: {
        auto s = DxPtr::make_omni<TypeVarType>();
        s->typevar = node->get<string>("typevar");
        s->constraint = convert<Pointer<Type>>(node->get<Table>("constraint"));
        result = std::move(s);
        break;
    }

    case TypeName::MAP: {
        auto s = DxPtr::make_omni<MapType>();
        s->is_total = node->get<boolean>("is_total");
        s->missing = convert_array<string>(node->get<Table>("missing"));
        s->keys = convert<Pointer<Type>>(node->get<Table>("keys"));
        s->values = convert<Pointer<Type>>(node->get<Table>("values"));
        result = std::move(s);
        break;
    }

    case TypeName::EMPTYTABLE: {
        auto s = DxPtr::make_omni<EmptyTableType>();
        s->declared_at  = convert<Pointer<Node>>(node->get<Table>("declared_at"));
        s->assigned_to  = node->get<string>("assigned_to");
        s->keys         = convert<Pointer<Type>>(node->get<Table>("keys"));
        result = std::move(s);
        break;
    }

    case TypeName::UNRESOLVED_EMPTYTABLE_VALUE: {
        auto s = DxPtr::make_omni<UnresolvedEmptyTableValueType>();
        s->emptytable_type = derived_ptr_cast<EmptyTableType>(convert<Pointer<Type>>(node->get<Table>("emptytable_type")));
        result = std::move(s);
        break;
    }

    case TypeName::FUNCTION: {
        auto s = DxPtr::make_omni<FunctionType>();
        s->is_method = node->get<boolean>("is_method");
        s->min_arity = node->get<integer>("min_arity");
        s->args = derived_ptr_cast<TupleType>(convert<Pointer<Type>>(node->get<Table>("args")));
    }

    default:
        throw std::runtime_error("Unknown type name: "s + *node->get<string>("type_name"));
    };

    result->x = node->get<integer>("x");
    result->y = node->get<integer>("y");
    result->type_name = *type_name;
    result->type_id = node->get<integer>("type_id");
    result->inferred_at = convert<Optional<Where>>(node->get<Table>("inferred_at"));
    result->needs_compat = node->get<boolean>("needs_compat");

    return result;
}

[[nodiscard]]
static Pointer<Fact> convert_fact(const Table &node)
{
    return nullptr;
}

template<>
[[nodiscard]]
Optional<NodeKind> convert_enum<NodeKind>(const string &kind)
{
    if (not kind.has_value()) return nullopt;

    Optional<NodeKind> result;

    //Match the enum kind to the real C++ enum
    if (kind == "...") result = NodeKind::VARARGS;
    else {
        result = magic_enum::enum_cast<NodeKind>(*kind, [](char l, char r) { return ::tolower(l) == ::tolower(r); });
    }

    return result;
}

template<>
[[nodiscard]]
Array<TypeArgType> convert_array<TypeArgType>(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table) return nullopt;

    auto result = std::vector<TypeArgType>(node->size());
    for (size_t i = 0; i < node->size(); i++) {
        auto x = (*node)[i + 1];
        if (x.get_type() == sol::type::table)
            result.emplace_back(std::move(*derived_ptr_cast<TypeArgType>(convert<Pointer<Type>>(x))));
    }

    return result;
}

template<>
[[nodiscard]]
Array<Pointer<Node>> convert_array<Pointer<Node>>(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table)
        return nullopt;

    auto result = std::vector<Pointer<Node>>(node->size());
    for (size_t i = 1; i <= node->size(); i++) {
        Table x = (*node)[i];
        if (not x.has_value())
            throw std::runtime_error(std::format("Node at index {} is not a valid table!", i));
        auto ptr = convert<Pointer<Node>>(x);
        if (ptr == nullptr)
            throw std::runtime_error(std::format("Node ({})[{}] is not a valid node!", static_cast<const void *>(&node), i));
        result.emplace_back(std::move(ptr));
    }
    return result;
}

template<>
[[nodiscard]]
Pointer<Node> convert<Pointer<Node>>(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table)
        return nullptr;

    auto result = DxPtr::make_omni<Node>();

    result->children = convert_array<Pointer<Node>>(node);
    //verify all children are valid
    if (result->children.has_value()) {
        for (size_t i = 0; i < result->children->size(); i++) {
            if (not result->children->at(i)) {
                throw std::runtime_error(std::format("Node at index {} is not a valid node!", i));
            }
        }
    }

    result->tk = node->get<string>("tk");
    result->kind = convert_enum<NodeKind>(node->get<string>("kind"));
    result->symbol_list_slot = node->get<integer>("symbol_list_slot");
    result->semicolon = node->get<boolean>("semicolon");
    result->hashbang = node->get<string>("hashbang");
    result->is_longstring = node->get<boolean>("is_longstring");
    result->yend = node->get<integer>("yend");
    result->xend = node->get<integer>("xend");

    result->known = convert_fact(node->get<Table>("known"));
    result->expected = convert<Pointer<Type>>(node->get<Table>("expected"));
    result->expected_context = ({
        Optional<Node::ExpectedContext> match = std::nullopt;
        if (auto tbl = node->get<Table>("expected_context"); tbl.has_value() and tbl->valid()) {
            match = Node::ExpectedContext {
                .kind = convert_enum<NodeKind>(tbl->get<string>("kind")),
                .name = tbl->get<string>("name")
            };
        }
        match;
    });

    result->key = convert<Pointer<Node>>(node->get<Table>("key"));
    result->value = convert<Pointer<Node>>(node->get<Table>("value"));
    result->key_parsed = convert_enum<KeyParsed>(node->get<string>("key_parsed"));

    result->typeargs = convert_array<TypeArgType>(node->get<Table>("typeargs"));
    result->min_arity = node->get<integer>("min_arity");
    result->args = convert<Pointer<Node>>(node->get<Table>("args"));
    result->body = convert<Pointer<Node>>(node->get<Table>("body"));
    result->implicit_global_function = node->get<boolean>("implicit_global_function");
    result->is_predeclared_local_function = node->get<boolean>("is_predeclared_local_function");

    result->name = convert<Pointer<Node>>(node->get<Table>("name"));

    result->is_repeat = node->get<boolean>("is_repeat");

    result->attribute = convert_enum<Attribute>(node->get<string>("attribute"));

    result->fn_owner = convert<Pointer<Node>>(node->get<Table>("fn_owner"));
    result->is_method = node->get<boolean>("is_method");

    result->exp = convert<Pointer<Node>>(node->get<Table>("exp"));
    result->if_parent = convert<Pointer<Node>>(node->get<Table>("if_parent"));
    result->if_block_n = node->get<integer>("if_block_n");
    result->block_returns = node->get<boolean>("block_returns");

    result->var = convert<Pointer<Node>>(node->get<Table>("var"));
    result->from = convert<Pointer<Node>>(node->get<Table>("from"));
    result->to = convert<Pointer<Node>>(node->get<Table>("to"));
    result->step = convert<Pointer<Node>>(node->get<Table>("step"));

    result->vars = convert<Pointer<Node>>(node->get<Table>("vars"));
    result->exps = convert<Pointer<Node>>(node->get<Table>("exps"));

    //fuck this
    result->newtype = ({
        //fuck you
        decltype(result->newtype) res = nullopt;
        auto ty = convert<Pointer<Type>>(node->get<Table>("newtype"));
        if (ty) {
            if (ty->type_name == TypeName::TYPEALIAS)
                res = derived_ptr_cast<TypeAliasType>(std::move(ty));
            else if (ty->type_name == TypeName::TYPEDECL)
                res = derived_ptr_cast<TypeDeclType>(std::move(ty));
        }

        std::move(res);
    });
    result->elide_type = node->get<boolean>("elide_type");

    result->op = ({
        auto tbl = node->get<Table>("op");
        Optional<Operator> op = nullopt;

        if (tbl.has_value()) {
            op->y = tbl->get<integer>("y");
            op->x = tbl->get<integer>("x");
            op->arity = tbl->get<integer>("arity");
            auto opv = tbl->get<string>("op");
            if (opv.has_value()) (void)*opv; //this prevents UBSan from triggering. No, I have no fucking clue why or how
            op->op = tbl->get<string>("op");
            op->prec = tbl->get<integer>("prec");
        }

        op;
    });
    result->e1 = convert<Pointer<Node>>(node->get<Table>("e1"));
    result->e2 = convert<Pointer<Node>>(node->get<Table>("e2"));

    result->constnum = node->get<number>("constnum");
    result->conststr = node->get<string>("conststr");
    result->failstore = node->get<boolean>("failstore");
    result->discarded_tuple = node->get<boolean>("discarded_tuple");
    result->receiver = convert<Pointer<Type>>(node->get<Table>("receiver"));

    result->array_len = node->get<integer>("array_len");

    result->label = node->get<string>("label");
    result->used_label = node->get<boolean>("used_label");

    result->casttype = convert<Pointer<Type>>(node->get<Table>("casttype"));

    result->is_lvalue = node->get<boolean>("is_lvalue");

    result->macrodef = convert<Pointer<Node>>(node->get<Table>("macrodef"));
    result->expanded = convert<Pointer<Node>>(node->get<Table>("expanded"));

    result->argtype = convert<Pointer<Type>>(node->get<Table>("argtype"));
    result->itemtype = convert<Pointer<Type>>(node->get<Table>("itemtype"));
    result->decltuple = derived_ptr_cast<TupleType>(convert<Pointer<Type>>(node->get<Table>("decltuple")));

    result->opt = node->get<boolean>("opt");

    result->debug_type = convert<Pointer<Type>>(node->get<Table>("debug_type"));

    return result;
}

std::tuple<Pointer<Node>, sol::table> Node::convert_from_lua(const std::string &input, const std::string &filename)
{
    auto [ast, errors, _] = TEAL.parse(input, filename);
    if (errors.size() > 0) {
        auto errs = std::vector<Error>(errors.size());
        for (size_t i = 0; i < errors.size(); i++) {
            errs[i] = *convert<Optional<Error>>(errors[i + 1]);
        }

        throw ConvertException(errs);
    }

    return { convert<Pointer<Node>>(ast), ast };
}
