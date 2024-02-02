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
   "argparse"
}
build = {
   type = "make",
   build_variables = {
      CFLAGS = "$(CFLAGS)",
      LIBFLAG = "$(LIBFLAG)",
      LUA_LIBDIR = "$(LUA_LIBDIR)",
      LUA_BINDIR = "$(LUA_BINDIR)",
      LUA_INCDIR = "$(LUA_INCDIR)",
      LUA = "$(LUA)",
   },
   install_variables = {
      INST_PREFIX = "$(PREFIX)",
      INST_BINDIR = "$(BINDIR)",
      INST_LIBDIR = "$(LIBDIR)",
      INST_LUADIR = "$(LUADIR)",
      INST_CONFDIR = "$(CONFDIR)",
   },
}
