os.rename("lua_modules/share/lua/5.1/tl.tl", "lua_modules/share/lua/5.1/tl.tl-do-not-use")
os.rename("lua_modules/share/lua/5.1/tl.lua", "lua_modules/share/lua/5.1/tl.lua-do-not-use")
os.execute("cp teal/tl.lua lua_modules/share/lua/5.1/tl.lua")
