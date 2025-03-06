#include <typeinfo>
#include <sol/sol.hpp>
#include <cxxabi.h>

#include "teal-parser/AST.hpp"
#include "teal-parser/Parser.hpp"

template<class ...Ts>
struct overload : Ts... {
    using Ts::operator()...;
};

template<typename ...Variant>
static constexpr auto match(Variant &&...var)
{
    return [&var...]<typename... T>(T &&...matchers) constexpr -> decltype(auto) {
        return std::visit(overload {std::forward<T>(matchers)...}, std::forward<Variant>(var)...);
    };
}

namespace lua
{
    template<typename T> requires std::is_base_of_v<teal::ASTNode, T>
    static std::string node_name()
    {
        char buf[1024];
        size_t siz = sizeof(buf);
        int ok = 0;
        abi::__cxa_demangle(typeid(T).name(), buf, &siz, &ok);
        return buf;
    }

    template<typename T> requires std::is_base_of_v<teal::ASTNode, T>
    sol::table serialise_node(sol::state_view lua, const T *node);

    template<>
    sol::table serialise_node<teal::NameExpression>(sol::state_view lua, const teal::NameExpression *node)
    {
        return lua.create_table_with(

            "name", node->name
        );
    }

    constexpr sol::table token_to_table(sol::state_view lua, const teal::Token &token)
    {
        auto tbl = lua.create_table();
        tbl["type"] = teal::Token::type_to_string(token.type);
        tbl["text"] = token.text;
        tbl["line"] = token.line;
        tbl["column"] = token.col;
        return tbl;
    }

    constexpr sol::table lexer_error_to_table(sol::state_view lua, const teal::Lexer::Error &err)
    {
        auto tbl = lua.create_table();

        tbl["error"] = err.to_string();
        tbl["line"] = err.line;
        tbl["column"] = err.column;

        return tbl;
    }

    sol::table open_bindings(sol::this_state raw)
    {
        auto lua = sol::state_view(raw);

        auto module = lua.create_table();
        {
            module.set_function("parse", [raw](const std::string_view &src) -> std::tuple<sol::table, sol::table> {
                auto lua = sol::state_view(raw);
                auto lexer = teal::Lexer(std::string(src));
                auto [tks, errs] = lexer.tokenize();

                if (errs.size() > 0) {
                    auto ltks = lua.create_table();
                    for (const auto &tk : tks) {
                        ltks.add(token_to_table(lua, tk));
                    }

                    auto lerrs = lua.create_table();
                    for (const auto &err : errs) {
                        lerrs.add(err);
                    }

                    return std::make_tuple(ltks, lerrs);
                }

                auto parser = teal::Parser(tks);

            });
        }
        return module;
    }
}

int luaopen_teal_parser(lua_State *lua)
{
	return sol::stack::call_lua(lua, 1, lua::open_bindings);
}
