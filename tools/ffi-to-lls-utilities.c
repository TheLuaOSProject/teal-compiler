/**
 * Copyright (C) 2024 Amrit Bhogal
 *
 * This file is part of ffi-to-lls.
 *
 * ffi-to-lls is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * ffi-to-lls is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ffi-to-lls.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <clang-c/Index.h>


 //luajit can't handle the CXCursors being passed as values, so we pass them as pointers
static enum CXChildVisitResult cursor_visitor(CXCursor cursor, CXCursor parent, CXClientData data)
{
    return (*(int (*)(CXCursor *, CXCursor *))data)(&cursor, &parent);
}

__attribute__((used, noinline))
__attribute__((visibility("default")))
unsigned int visit_cursor(CXCursor cursor, int (*vis)(CXCursor *cursor, CXCursor *parent))
{
    return clang_visitChildren(cursor, cursor_visitor, vis);
}
