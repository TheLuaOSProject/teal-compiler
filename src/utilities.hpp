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

#include "DxPtr.hpp"

template<typename TTo, typename TFrom> requires std::is_base_of_v<TFrom, TTo>
constexpr inline DxPtr::omni_ptr<TTo> derived_ptr_cast(DxPtr::omni_ptr<TFrom> &&from)
{
    if (not from) return nullptr; // nullptr -> nullptr, all good
    auto newptr = DxPtr::dynamic_pointer_cast<TTo>(std::move(from));
    if (not newptr) throw std::bad_cast();
    return newptr;
}
