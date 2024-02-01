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
}
build = {
   type = "make",
   -- install = {
   --    -- bin = {
   --    --    ["teal-compiler"] = "build/main.lua"
   --    --    ["print-ast"] = "build/print-ast.lua"
   --    -- }
   -- },
   -- modules = {
   --    -- ["backends.gccjit"] = "src/backends/gccjit.lua",
   --    -- ["backends.gccjit.cdef"] = "src/backends/gccjit/cdef.lua",
   --    -- ["backends.gccjit.init"] = "src/backends/gccjit/init.lua",
   --    -- ["teal.tl"] = "teal/tl.lua",
   --    -- ["tl"] = "teal/tl.lua",
   -- }
}
