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

#define SOL_ALL_SAFETIES_ON 1
#include <sol/sol.hpp>

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
static Pointer<Type> convert_type(const sol::table &node)
{
    return nullptr;
}

[[nodiscard]]
static Pointer<Fact> convert_fact(const sol::table &node)
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
    else result = magic_enum::enum_cast<NodeKind>(*kind, [](auto l, auto r) { return ::tolower(l) == ::tolower(r); });

    return result;
}

[[nodiscard]]
static Node convert_node(const sol::table &node);

template<typename T>
[[nodiscard]]
static Array<T> convert_array(const sol::table &node)
{ static_assert(sizeof(T) == 0, "Use specialisations of this function"); }

template<>
[[nodiscard]]
Array<Node> convert_array<Node>(const sol::table &node)
{
    if (not node.valid() or node.get_type() != sol::type::table) return nullopt;

    auto result = std::vector<Node>(node.size());
    for (size_t i = 0; i < node.size(); i++) {
        auto x = node[i + 1];
        if (x.get_type() == sol::type::table)
            result.emplace_back(convert_node(x));
    }
    return result;
}

template<typename T>
[[nodiscard]] T convert_if_exists(const sol::table &node)
{ static_assert(sizeof(T) == 0, "Use specialisations of this function"); }

template<>
[[nodiscard]]
Pointer<Node> convert_if_exists<Pointer<Node>>(const sol::table &node)
{ return node.valid() ? std::make_unique<Node>(convert_node(node)) : nullptr; }

[[nodiscard]]
static Node convert_node(const sol::table &node)
{
    Node result;

    result.tk = node.get<std::string>("tk");
    result.kind = convert_enum<NodeKind>(node.get<string>("kind"));
    result.symbol_list_slot = node.get<integer>("symbol_list_slot");
    result.semicolon = node.get<boolean>("semicolon");
    result.hashbang = node.get<string>("hashbang");
    result.is_longstring = node.get<boolean>("is_longstring");
    result.yend = node.get<integer>("yend");
    result.xend = node.get<integer>("xend");

    result.known = convert_fact(node.get<sol::table>("known"));
    result.expected = convert_type(node.get<sol::table>("expected"));
    result.expected_context = ({
        Optional<Node::ExpectedContext> match = std::nullopt;
        if (auto tbl = node.get<sol::table>("expected_context"); tbl.valid()) {
            match = Node::ExpectedContext {
                .kind = convert_enum<NodeKind>(tbl.get<string>("kind")),
                .name = tbl.get<string>("name")
            };
        }
        match;
    });

    result.key = convert_if_exists<Pointer<Node>>(node.get<sol::table>("key"));
    result.value = convert_if_exists<Pointer<Node>>(node.get<sol::table>("value"));
    result.key_parsed = convert_enum<KeyParsed>(node.get<string>("key_parsed"));

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
