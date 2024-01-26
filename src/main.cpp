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

int main()
{
    std::filesystem::path path = "test.tl";
    auto contents = ({
        auto file = std::ifstream(path);
        std::string contents;
        file.seekg(0, std::ios::end);
        contents.reserve(file.tellg());
        file.seekg(0, std::ios::beg);
        contents.assign(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
        contents;
    });


    auto ast = teal::raw::Node::convert_from_lua(contents, "test.teal");
    std::println("tk: {}, kind: {}, children.size(): {}", ast.tk.value(), magic_enum::enum_name(ast.kind.value()),  ast.children->size());
}
