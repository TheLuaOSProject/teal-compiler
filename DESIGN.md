# Teal compiler language design document

The language used for the teal compiler is a soft superset of [teal](https://github.com/teal-language/tl) with some additional features and restrictions. Read the [teal documentation](https://github.com/teal-language/tl/tree/master/docs) first.

## Key ideas

- Lua interop:
    - You should be able to easily run lua functions from teal and vice versa.
    - One of the main motivations for this compiler was to make it easier and faster to write Lua modules
- C interop
    - You should be able to easily call C functions from teal and vice versa.
    - This adds on to making it easier to write Lua modules, as you can write C functions to be called from Lua.

## Modules

Perhaps the biggest difference is with modules:

```lua
local export = {}

function export.say_hello()
    print("Hello, world!")
end

return export
```

### Lua interop

This is an example of a basic module, it is the same as regular teal. You may return either a table or a function. This module can only be accessed from other native teal code. To make it accessible from Lua, you must use the `#lua module` pragma:

```lua
--#lua module "hello"
local export = {}

--This is now implicitly a "lua" function.
--The first argument to the function is the lua_State *.
function export.say_hello()
    print("Hello, world!")
end

--you can also declare a function to not be a lua function within a lua module with
--#lua off
function export.my_function()
    print("This is a regular, compiled teal function")
end
--this will not be accessible from Lua but is accessible from other teal code, including teal functions which are accessible from Lua

return export
```

This module can now be accessed from Lua:

```lua
local hello = require("hello")
hello.say_hello()
```

### C interop

You can also call C functions from teal:

```lua
--#extern c
local function printf(format: c.string, ...: c.varadict): c.int32 end

local function f()
    printf("Hello, world!\n")
end
```

This will call the C function `printf` from the standard library. You can access any symbol that is linked into the executable this way.

## Exporting symbols

Any `global` variables in the top scope (`global` variables in any other scope is prohibited) will be accessable from outside the object file. This is also how to define a `main` function.

```lua

--a symbol `x` will be exported
global x: integer = 5

global function main(argc: integer, argv: c.pointer<c.string>): integer
    print("Argc: "..tostring(argc))

    return 0
end
```

## Type representation

More primitive types are defined in order to make it easier to interface with C. This table

| Teal type           | C type                      | Description                                                                                               | Lua equivalent type |
| :------------------ | :-------------------------- | :-------------------------------------------------------------------------------------------------------- | :------------------ |
| `nil`               | `void`                      | no value                                                                                                  | `nil`               |
| `byte`              | `uint8_t`                   | 8-bit unsigned integer                                                                                    | `number`            |
| `integer8`          | `int8_t`                    | 8-bit signed integer                                                                                      | `number`            |
| `integer16`         | `int16_t`                   | 16-bit signed integer                                                                                     | `number`            |
| `integer32`         | `int32_t`                   | 32-bit signed integer                                                                                     | `number`            |
| `integer64`         | `int64_t`                   | 64-bit signed integer                                                                                     | `number`            |
| `float32`           | `float`                     | single-precision floating point number                                                                    | `number`            |
| `float64`           | `double`                    | double-precision floating point number                                                                    | `number`            |
| `float128`          | `long double`               | quadruple-precision floating point number (available on supporting architectures)                         |                     |
| `record T`          | `struct T`                  | Standard product type                                                                                     | `table`             |
| `interface T`       | `struct T`                  | Interface (new in teal `next`)                                                                            | `table`             |
| `{T}`               | `T[]`                       | Array                                                                                                     | `table`             |
| `{T1, T2}`          | `struct {T1; T2} []`        | Tuple                                                                                                     | `table`             |
| `enum`              | `const char *`              | Enum, in teal this is represented as strings, so the same must be done in C.                              | `string`            |
| `T1 \| T2`          | `struct {T1\|T2; enum tag}` | Sum type, in C this would be a tagged union, where in Lua it would be any value                           | `any`               |
| `any`               | `void *`                    | Any value, can be any type                                                                                | `lightuserdata`     |
|                     |                             |                                                                                                           |                     |
| `function(T...): T` | `T(T...)`                   | Function                                                                                                  | `function`          |
| `unsigned<T>`       | `unsigned T`                | Marks an integral type to be unsigned                                                                     | `number`            |
| `value<T>`          |                             | By default, any `record` type is a reference on the heap. `value<T>` forces it to be located on the stack |                     |
| `integer`           | `int64_t`                   | 64-bit signed integer                                                                                     | `number`            |
| `number`            | `double`                    | double-precision floating point number                                                                    | `number`            |
| `boolean`           | `bool`                      | boolean                                                                                                   | `boolean`           |
| `string`            |                             | Proper string type, not null-terminated                                                                   | `string`            |
|                     |                             |                                                                                                           |                     |
| `c.pointer<T>`      | `T *`                       | Pointer to value. This is a raw pointer, so be careful!                                                   | `lightuserdata`     |
| `c.string`          | `const char *`              | C string type, must be null terminated                                                                    | `string`            |
| `c.varadict`        | `...`                       | C varadict, only valid in arguments for C funcitons                                                       |                     |
| `c.const<T>`        | `const T`                   | Constant type specifier                                                                                   |                     |
|                     |                             |                                                                                                           |                     |
