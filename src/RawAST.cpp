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

static Error parse_error(const sol::table &error)
{
    Error result;
    result.msg = error["msg"];
    result.tag = error["tag"];
    result.filename = error.get_or("filename", ""s);
    result.y = error["y"];
    result.x = error["x"];
    return result;
}

static Node parse_node(const sol::table &node)
{
    Node result;

    result.tk = node["tk"];
    result.kind = ({
        NodeKind match = NodeKind::ERROR_NODE;
        string kind = node["kind"];

        if (kind == "...") {
            match = NodeKind::VARARGS;
        } else {
            //Match the enum kind to the real C++ enum
            std::transform(kind.begin(), kind.end(), kind.begin(), ::toupper);
            match = magic_enum::enum_cast<NodeKind>(kind).value_or(NodeKind::UNKNOWN);
        }

        match;
    });
    result.symbol_list_slot = node.get<Optional<integer>>("symbol_list_slot");

    return result;
}

Node Node::parse(const string &input, const string &filename)
{
    auto [ast, errors, comments] = TEAL.parse(input, filename);
    if (errors.size() > 0) {
        auto errs = Array<Error>(errors.size());
        for (size_t i = 0; i < errors.size(); i++) {
            errs[i] = parse_error(errors[i + 1]);
        }

        throw ParseException(errs);
    }

    return parse_node(ast);
}
