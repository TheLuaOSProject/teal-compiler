---@diagnostic disable: undefined-global
target("llvm-c-tests", function ()
    set_languages("gnu23")
    add_packages("libllvm")
    add_files("src/**.c")
end)
