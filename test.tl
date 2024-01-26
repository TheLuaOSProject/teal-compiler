global function main(cli_args: {string})
    print("Hello World!")
    print("CLI args:")
    for i, v in ipairs(cli_args) do
        print(i, v)
    end
end
