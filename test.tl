local printf<extern>: function(s: string, ...: c.varadict): integer
global puts<extern>: function(s: string): nil

local function fib(n: integer): integer
    if n <= 1 then
        return n
    else
        return fib(n - 1) + fib(n - 2)
    end
end

global function main(): integer
    printf "Hello, World!\n"

    local i = 10
    local result = fib(i)
    printf("fib(%d) = %d\n", i, result)

    return 0
end
