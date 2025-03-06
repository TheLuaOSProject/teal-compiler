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
        package:add("cflags", cflags)
        package:add("ldflags", ldflags)
    end)
end
package_end()

add_requires("libllvm")
includes("parser")

set_languages("gnu++23")

target("teal-compiler")
    add_files("src/**.cpp")
    add_cxxflags("-Wall", "-Wextra", "-Werror", "-Wno-c23-extensions", "-std=libc++")
    add_deps("teal-parser")
    add_packages("libllvm")

