#pragma once

#include <string>
#include <cxxabi.h>

namespace teal::compiler::utilities
{
    static std::string demangle(const std::string_view &ident)
    {
        char buf[1024];
        size_t s = sizeof(buf);
        int ok = 0;
        abi::__cxa_demangle(ident.data(), buf, &s, &ok);
        return std::string(buf, s);
    }
}
