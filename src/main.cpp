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

#include "utilities.hpp"

#include "RawAST.hpp"

static void debug_ast_traverse(const DxPtr::omni_ptr<teal::raw::Node> &node, int depth = 2)
{
    std::println("{}Node at {} ({}:{})", std::string(static_cast<size_t>(depth-2), ' '), static_cast<void *>(node.get()), *node->yend, *node->xend);
    std::println("{}tk: {}, kind: {}, children.size(): {}", std::string(static_cast<size_t>(depth), ' '), *node->tk, magic_enum::enum_name(*node->kind), node->children.has_value() ? node->children->size() : 0);

    if (node->children.has_value() and not node->children->empty()) {
        for (auto &child : *node->children) {
            debug_ast_traverse(child, depth + 4);
        }
    }
}

int main(int argc, const char *argv[])
{
    std::filesystem::path path = argc > 1 ? argv[1] : "test.tl";
    auto file = std::ifstream(path);
    std::string contents;
    file.seekg(0, std::ios::end);
    contents.reserve(static_cast<size_t>(file.tellg()));
    file.seekg(0, std::ios::beg);
    contents.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());


    auto [ast, raw] = teal::raw::Node::convert_from_lua(contents, "test.teal");
    debug_ast_traverse(ast);
}
