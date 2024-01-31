--Config:
local packages = {
    "sol2",
    "magic_enum"
}

local sanitizers = { "address", "leak", "undefined" }

local function no(x) return "-Wno-"..x end

local cxxflags = {
    release = {
        "-Weverything",
        no"c++98-compat",
        no"c++20-extensions",
        no"shadow-field-in-constructor",
        no"unsafe-buffer-usage",
        no"weak-vtables",
        no"padded",
        no"gnu-designator",
        no"pre-c++20-compat-pedantic",
        no"exit-time-destructors",
        no"c++98-compat-pedantic",
        no"global-constructors",
        no"gnu-statement-expression",
        no"shadow-uncaptured-local",
        no"switch-enum",
        no"deprecated-copy-with-dtor"
    },
    debug = {
        no"unused-function", no"unused-parameter", no"unused-variable"
    },
    regular = {
        "-Wall", "-Wextra", "-Werror",
        "-stdlib=libc++",
        no"c99-designator",
        no"shadow-field"
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
