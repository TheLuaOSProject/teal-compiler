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

#include <algorithm>

using std::nullopt;
using namespace std::string_literals;
using namespace teal::raw;

static struct Teal {
    sol::state lua;
    sol::table teal;

    Teal()
    {
        lua.open_libraries();
        teal = lua.do_file("teal/tl.lua");
    }

    std::tuple<sol::table, sol::table, sol::table> parse(const std::string &input, const std::string &filename)
    { return teal["parse"](input, filename); }
} TEAL;

template<typename T>
using ConvertResult = Union<Optional<T>, Pointer<T>>;

template<typename T>
[[nodiscard]]
static ConvertResult<T> convert(const Table &node)
{ static_assert(sizeof(T) == 0, "Use specialisations of this function"); }

// [[nodiscard]]
// static Error convert_error(const sol::table &error)
template<>
[[nodiscard]]
ConvertResult<Error> convert<Error>(const Table &error)
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
ConvertResult<Where> convert<Where>(const Table &node)
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
ConvertResult<std::vector<std::string>> convert<std::vector<std::string>>(const Table &node)
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
ConvertResult<Type> convert<Type>(const Table &node)
{
    if (not node.has_value()
     or not node->valid()
     or node->get_type() != sol::type::table
     or not node->get<string>("type_name").has_value()) return nullptr;

    Pointer<Type> result = nullptr;

    auto type_name = convert_enum<TypeName>(*node->get<string>("type_name"));
    if (not type_name.has_value()) return nullptr;

    switch (*type_name) {
    case TypeName::STRING: {
        auto s = std::make_unique<StringType>();
        s->literal = node->get<string>("literal");
        result = std::move(s);
        break;
    }

    case TypeName::INTEGER: {
        result = std::make_unique<IntegerType>();
        break;
    }

    case TypeName::BOOLEAN: {
        result = std::make_unique<BooleanType>();
        break;
    }

    case TypeName::TYPEDECL: {
        auto s = std::make_unique<TypeDeclType>();
        s->def = convert_type(node->get<Table>("def"));
        s->closed = node->get<boolean>("closed");
        result = std::move(s);
        break;
    }

    case TypeName::NOMINAL: {
        auto s = std::make_unique<NominalType>();
        s->names = convert_array<std::string>(node->get<Table>("names"));
        s->typevals = convert_array<Pointer<Type>>(node->get<Table>("typevals"));
        s->found = convert_type(node->get<Table>("found"));
        s->resolved = convert_type(node->get<Table>("resolved"));
        result = std::move(s);
        break;
    }

    case TypeName::LITERAL_TABLE_ITEM: {
        auto s = std::make_unique<LiteralTableItemType>();
        s->kname = node->get<string>("kname");
        s->ktype = convert_type(node->get<Table>("ktype"));
        s->vtype = convert_type(node->get<Table>("vtype"));
        result = std::move(s);
        break;
    }

    default:
        throw std::runtime_error("Unknown type name: "s + *node->get<string>("type_name"));
    };

    result->type_id = node->get<integer>("type_id");
    result->inferred_at = convert_where(node->get<Table>("inferred_at"));
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
        result = magic_enum::enum_cast<NodeKind>(*kind, [](auto l, auto r) { return ::tolower(l) == ::tolower(r); });
    }

    return result;
}

[[nodiscard]]
static Pointer<Node> convert_node(const Table &node);

template<>
[[nodiscard]]
Array<TypeArgType> convert_array<TypeArgType>(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table) return nullopt;

    auto result = std::vector<TypeArgType>(node->size());
    for (size_t i = 0; i < node->size(); i++) {
        auto x = (*node)[i + 1];
        if (x.get_type() == sol::type::table)
            result.emplace_back(std::move(*dynamic_cast<TypeArgType *>(convert_type(x).get())));
    }

    return result;
}

template<>
[[nodiscard]]
Array<Pointer<Node>> convert_array<Pointer<Node>>(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table) return nullopt;

    auto result = std::vector<Pointer<Node>>(node->size());
    for (size_t i = 0; i < node->size(); i++) {
        auto x = (*node)[i + 1];
        if (x.get_type() == sol::type::table)
            result.emplace_back(convert_node(x));
    }
    return result;
}

[[nodiscard]]
static Pointer<Node> convert_node(const Table &node)
{
    if (not node.has_value() or not node->valid() or node->get_type() != sol::type::table) return nullptr;

    auto result = std::make_unique<Node>();

    result->children = convert_array<Pointer<Node>>(node);
    result->tk = node->get<std::string>("tk");
    result->kind = convert_enum<NodeKind>(node->get<string>("kind"));
    result->symbol_list_slot = node->get<integer>("symbol_list_slot");
    result->semicolon = node->get<boolean>("semicolon");
    result->hashbang = node->get<string>("hashbang");
    result->is_longstring = node->get<boolean>("is_longstring");
    result->yend = node->get<integer>("yend");
    result->xend = node->get<integer>("xend");

    result->known = convert_fact(node->get<Table>("known"));
    result->expected = convert_type(node->get<Table>("expected"));
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

    result->key = convert_node(node->get<Table>("key"));
    result->value = convert_node(node->get<Table>("value"));
    result->key_parsed = convert_enum<KeyParsed>(node->get<string>("key_parsed"));

    result->typeargs = convert_array<TypeArgType>(node->get<Table>("typeargs"));
    result->min_arity = node->get<integer>("min_arity");
    result->args = convert_node(node->get<Table>("args"));
    result->body = convert_node(node->get<Table>("body"));
    result->implicit_global_function = node->get<boolean>("implicit_global_function");
    result->is_predeclared_local_function = node->get<boolean>("is_predeclared_local_function");

    result->name = convert_node(node->get<Table>("name"));

    result->is_repeat = node->get<boolean>("is_repeat");

    result->attribute = convert_enum<Attribute>(node->get<string>("attribute"));

    result->fn_owner = convert_node(node->get<Table>("fn_owner"));
    result->is_method = node->get<boolean>("is_method");

    result->exp = convert_node(node->get<Table>("exp"));
    result->if_parent = convert_node(node->get<Table>("if_parent"));
    result->if_block_n = node->get<integer>("if_block_n");
    result->block_returns = node->get<boolean>("block_returns");

    result->var = convert_node(node->get<Table>("var"));
    result->from = convert_node(node->get<Table>("from"));
    result->to = convert_node(node->get<Table>("to"));
    result->step = convert_node(node->get<Table>("step"));

    result->vars = convert_node(node->get<Table>("vars"));
    result->exps = convert_node(node->get<Table>("exps"));

    //fuck this
    result->newtype = ({
        //fuck you
        decltype(result->newtype) res;
        auto ty = convert_type(node->get<Table>("newtype"));

        if (ty->type_name == TypeName::TYPEALIAS)
            res = Pointer<TypeAliasType>(dynamic_cast<TypeAliasType *>(ty.release()));
        else if (ty->type_name == TypeName::TYPEDECL)
            res = Pointer<TypeDeclType>(dynamic_cast<TypeDeclType *>(ty.release()));

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
            op->op = tbl->get<string>("op");
            op->prec = tbl->get<integer>("prec");
        }
        op;
    });
    result->e1 = convert_node(node->get<Table>("e1"));
    result->e2 = convert_node(node->get<Table>("e2"));

    result->constnum = node->get<number>("constnum");
    result->conststr = node->get<string>("conststr");
    result->failstore = node->get<boolean>("failstore");
    result->discarded_tuple = node->get<boolean>("discarded_tuple");
    result->receiver = convert_type(node->get<Table>("receiver"));

    result->array_len = node->get<integer>("array_len");

    result->label = node->get<string>("label");
    result->used_label = node->get<boolean>("used_label");

    result->casttype = convert_type(node->get<Table>("casttype"));

    result->is_lvalue = node->get<boolean>("is_lvalue");

    result->macrodef = convert_node(node->get<Table>("macrodef"));
    result->expanded = convert_node(node->get<Table>("expanded"));

    result->argtype = convert_type(node->get<Table>("argtype"));
    result->itemtype = convert_type(node->get<Table>("itemtype"));
    result->decltuple = Pointer<TupleType>(dynamic_cast<TupleType *>(convert_type(node->get<Table>("decltuple")).release()));

    result->opt = node->get<boolean>("opt");

    result->debug_type = convert_type(node->get<Table>("debug_type"));

    return result;
}

Node &&Node::convert_from_lua(const std::string &input, const std::string &filename)
{
    auto [ast, errors, _] = TEAL.parse(input, filename);
    if (errors.size() > 0) {
        auto errs = std::vector<Error>(errors.size());
        for (size_t i = 0; i < errors.size(); i++) {
            errs[i] = convert_error(errors[i + 1]);
        }

        throw ConvertException(errs);
    }

    return std::move(*convert_node(ast));
}
