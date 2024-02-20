package = "teal-compiler"
version = "dev-1"
source = {
   url = "git+https://github.com/Frityet/teal-compiler.git"
}
description = {
   homepage = "https://github.com/Frityet/teal-compiler",
   license = "GPLv3"
}
dependencies = {
   "lua ~> 5.1",
   "penlight",
   "argparse",
   "tl"
}
build = {
   type = "builtin",
   install = {
      bin = {
         ["teal-compiler"] = "src/main.lua"
      }
   },
   modules = {
      ["utilities"] = "src/utilities.lua",
      ["abi"] = "src/abi.lua",
      ["codegen.gccjit"] = "src/codegen/gccjit.lua",
      ["backends.gccjit"] = "src/backends/gccjit/init.lua",
      ["backends.gccjit.cdef"] = "src/backends/gccjit/cdef.lua",
   }
}
