local printf<extern>: function(s: string, ...: c.varadict): c.int32_t
global puts<extern>: function(s: string): nil

local function fib(n: integer): integer
    if n <= 1 then
        return n
    end

    return fib(n - 1) + fib(n - 2)
end

global function main(): c.int32_t
    printf "Hello, World!\n"

    local i = 10
    local result = fib(i)
    printf("fib(%d) = %d\n", i, result)

    i = (result / 10) as integer

    return 0 as c.int32_t
end
