local type cstring = c.pointer<c.const<c.int8_t>>
local printf<extern>: function(s: cstring, ...: c.varadict): c.int32_t

global function main(argc: c.int32_t, argv: c.pointer<cstring>): c.int32_t
    printf("Argc: %d, argv: %p\n", argc, argv)
    printf("progname: %s\n", argv[0])

    return 0 as c.int32_t
end
