---@diagnostic disable: undefined-global, undefined-field
add_rules("mode.debug", "mode.release")

package("libllvm")
do
    on_install(function (package)
        import("lib.detect.find_tool")
        local llvmcfg = find_tool("llvm-config")

        local cflags, ldflags = {}, {}
        table.join2(cflags, os.iorunv(llvmcfg.program, {"--cflags"}):split("%s+"))
        table.join2(ldflags, os.iorunv(llvmcfg.program, {"--ldflags"}):split("%s+"))
        table.join2(ldflags, os.iorunv(llvmcfg.program, {"--libs"}):split("%s+"))

        print({
            cflags = cflags,
            ldflags = ldflags
        })
        package:add("cxflags", cflags)
        package:add("ldflags", ldflags)
    end)
end
package_end()

add_requires("libllvm", "libc++", "argparse")
includes("parser")

set_languages("gnuxx23")

target("teal-compiler")
    add_files("src/**.cpp")
    add_cxxflags("-Wall", "-Wextra", "-Werror", "-Wno-c23-extensions", "-stdlib=libc++", "-fexperimental-library")
    add_deps("teal-parser")
    add_cxxflags(
        "-Wno-unused-parameter",
        "-Wno-error=deprecated-declarations" --because LLVM uses std::aligned_union for some reason :)
    )
    add_includedirs("src")
    add_packages("libllvm", "libc++", "argparse")

