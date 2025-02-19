---@diagnostic disable: undefined-global, undefined-field
add_rules("mode.debug", "mode.release")

package("libllvm")
do
    on_install(function (package)
        import("lib.detect.find_tool")
        local llvmcfg = find_tool("llvm-config")
        local cflags_str = os.iorunv(llvmcfg.program, {"--cflags"})
        local ldflags_str = os.iorunv(llvmcfg.program, {"--ldflags"})

        local cflags, ldflags = {}, {}
        table.join2(cflags, cflags_str:split("%s+"))
        table.join2(ldflags, ldflags_str:split("%s+"))

        package:add("cflags", cflags)
        package:add("ldflags", ldflags)
        package:add("links", {os.iorunv(llvmcfg.program, {"--libs"})})
    end)
end
package_end()

add_requires("libllvm")

includes("tests/llvm")

