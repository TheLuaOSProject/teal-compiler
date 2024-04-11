global function add(x: integer, y: integer): integer
    local result: integer = x + y

    return result
end

global function my_func(x: integer): integer
    local var = x * 12
    var = var + 1
    return var + x
end
