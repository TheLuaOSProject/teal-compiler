local printf<extern>: function(s: string, ...: c.varadict): c.int32_t

local function fib(n: integer): integer
    if n <= 1 then
        return n
    else
        return fib(n - 1) + fib(n - 2)
    end
end

global function main(): c.int32_t
    local i = 47
    local result = fib(i)
    printf("Final: fib(%lld) = %lld\n", i, result)

    return 0 as c.int32_t
end
