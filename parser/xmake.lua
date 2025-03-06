add_rules("mode.debug", "mode.release")

set_languages("gnuxx23")

includes("teal-parser")

add_requires("sol2")

target("teal-parser-bindings")
    set_kind("lua.module")
    add_files("src/**.cpp")
    add_cxxflags("-fexperimental-library")

    add_packages("sol2")
    add_deps("teal-parser")

    add_cxxflags (
        "-Wall", "-Wextra", "-Werror",
        "-Wno-c23-extensions",
        "-stdlib=libc++"
    )
