local function test(): integer
    return 42
end

global function main(argc: integer): integer
    return test()
end
