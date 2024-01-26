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

[[nodiscard]]
static Error convert_error(const sol::table &error)
{
    Error result;
    result.msg = error["msg"];
    result.tag = error["tag"];
    result.filename = error.get_or("filename", ""s);
    result.y = error["y"];
    result.x = error["x"];
    return result;
}

[[nodiscard]]
static Pointer<Type> convert_type(const Table &node)
{
    return nullptr;
}

[[nodiscard]]
static Pointer<Fact> convert_fact(const Table &node)
{
    return nullptr;
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

template<typename T>
[[nodiscard]]
static Array<T> convert_array(const Table &node)
{ static_assert(sizeof(T) == 0, "Use specialisations of this function"); }

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

    return result;
}

Node Node::convert_from_lua(const std::string &input, const std::string &filename)
{
    auto [ast, errors, _] = TEAL.parse(input, filename);
    if (errors.size() > 0) {
        auto errs = std::vector<Error>(errors.size());
        for (size_t i = 0; i < errors.size(); i++) {
            errs[i] = convert_error(errors[i + 1]);
        }

        throw ConvertException(errs);
    }

    return convert_node(ast);
}
