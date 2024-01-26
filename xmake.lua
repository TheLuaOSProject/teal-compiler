--Config:
local packages = {
    "sol2",
    "magic_enum"
}

local sanitizers = { "address", "leak", "undefined" }

local cxxflags = {
    release = {
        "-Weverything",
        "-Wno-c++98-compat", "-Wno-c++20-extensions",
        "-Wno-shadow-field-in-constructor",
        "-Wno-unsafe-buffer-usage",
        "-Wno-weak-vtables",
        "-Wno-padded",
        "-Wno-gnu-designator"
    },
    debug = {
        "-Wno-unused-function", "-Wno-unused-parameter", "-Wno-unused-variable"
    },
    regular = {
        "-Wall", "-Wextra", "-Werror",
        "-stdlib=libc++",
        "-Wno-c99-designator"
    }
}

local ldflags = {
    release = {},
    debug = {},
    regular = {}
}

set_languages("gnuxxlatest", "gnulatest")

add_rules("mode.debug", "mode.release")

add_requires(packages)

target("TealCompiler")
do
    set_kind("binary")
    add_packages(packages)

    add_files("src/**.cpp")
    add_headerfiles("src/**.hpp")
    --precompile utilities.hpp
    set_pcxxheader("src/utilities.hpp")

    add_includedirs("src/")

    add_cxxflags(cxxflags.regular)
    add_ldflags(ldflags.regular)

    if is_mode("debug") then
        add_cxxflags(cxxflags.debug)
        add_ldflags(ldflags.debug)

        for _, v in ipairs(sanitizers) do
            add_cxflags("-fsanitize=" .. v)
            add_ldflags("-fsanitize=" .. v)
        end

        add_defines("PROJECT_DEBUG")
    elseif is_mode("release") then
        add_cxxflags(cxxflags.release)
        add_ldflags(ldflags.release)
    end

    on_config(function (target)
        import("lib.detect.find_tool")
        local llvm_config = find_tool("llvm-config")
        if not llvm_config then raise("llvm-config not found!") end

        local cxxflags_str = os.iorunv(llvm_config.program, {"--cxxflags"})
        local ldflags_str = os.iorunv(llvm_config.program, {"--ldflags" })

        local link_with = os.iorunv(llvm_config.program, {"--libs"}):gsub("-l(%S+)", "%1")

        local cxxflags = {}
        local ldflags = {}

        table.join2(cxxflags, cxxflags_str:gsub("-std=%S+", ""):split("%s"))
        table.join2(ldflags, ldflags_str:split("%s"))

        target:add("cxxflags", cxxflags)
        target:add("ldflags", ldflags)
        target:add("links", link_with)
    end)
end
target_end()
