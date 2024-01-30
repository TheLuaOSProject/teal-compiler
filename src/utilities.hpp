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

#pragma once

#include <variant>
#include <string>
#include <vector>
#include <print>
#include <memory>
#include <optional>
#include <unordered_map>
#include <exception>
#include <sstream>
#include <filesystem>
#include <fstream>
#include <magic_enum.hpp>

#define SOL_ALL_SAFETIES_ON 1
#include <sol/sol.hpp>

template<typename TTo, typename TFrom> requires std::is_base_of_v<TFrom, TTo>
constexpr inline std::unique_ptr<TTo> unique_ptr_cast(std::unique_ptr<TFrom> &&from)
{
    //make sure the types are ACTUALLY related
    if (not dynamic_cast<TTo *>(from.get()))
        throw std::bad_cast();

    return std::unique_ptr<TTo>(static_cast<TTo *>(from.release()));
}
