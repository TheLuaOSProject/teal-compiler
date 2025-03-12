#pragma once

#include <string>
#include <any>
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

    template<typename T, typename TBase> requires std::is_base_of_v<TBase, T>
    constexpr T *polymorphic_any_cast(const std::any &val)
    {
        try {
            return std::any_cast<T *>(val);
        } catch (const std::bad_any_cast &) {
            return std::any_cast<TBase *>(val);
        }
    }

    class Any : public std::any {
        public:
            using std::any::any;
    
            template<typename T>
            constexpr inline T to(this Any &&self)
            { return std::any_cast<T &&>(std::move(self)); }
    
            template<typename T>
            constexpr inline const T &as(this const Any &self)
            { return std::any_cast<const T &>(self); }
    
            template<typename T>
            constexpr inline T &as(this Any &self)
            { return std::any_cast<T &>(self); }
    };
}
