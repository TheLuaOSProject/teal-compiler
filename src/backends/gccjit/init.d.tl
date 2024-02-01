local ffi = require("ffi")

local record libgccjit
    interface Object end
    record Context is Object
        enum StringOption
            "progname" -- GCC_JIT_STR_OPTION_PROGNAME
            "add command line option" -- void gcc_jit_context_add_command_line_option (gcc_jit_context *ctxt, const char *option);
            "add driver option" -- void gcc_jit_context_add_driver_option (gcc_jit_context *ctxt, const char *option);

        end

        enum IntegerOption
            "optimization level" -- GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL
        end

        enum BooleanOption
            "debuginfo" -- GCC_JIT_BOOL_OPTION_DEBUGINFO
            "dump initial tree" -- GCC_JIT_BOOL_OPTION_DUMP_INITIAL_TREE
            "dump initial gimple" -- GCC_JIT_BOOL_OPTION_DUMP_INITIAL_GIMPLE
            "dump generated code" -- GCC_JIT_BOOL_OPTION_DUMP_GENERATED_CODE
            "dump summary" -- GCC_JIT_BOOL_OPTION_DUMP_SUMMARY
            "dump everything" -- GCC_JIT_BOOL_OPTION_DUMP_EVERYTHING
            "selfcheck GC" -- GCC_JIT_BOOL_OPTION_SELFCHECK_GC
            "keep intermediates" -- GCC_JIT_BOOL_OPTION_KEEP_INTERMEDIATES
            "allow unreachable blocks" -- void gcc_jit_context_set_bool_allow_unreachable_blocks (gcc_jit_context *ctxt, int bool_value);
            "print errors to stderr" -- void gcc_jit_context_set_bool_print_errors_to_stderr (gcc_jit_context *ctxt, int enabled);
            "use external driver" -- void gcc_jit_context_set_bool_use_external_driver (gcc_jit_context *ctxt, int enabled);
        end

        enum CompilationKind
            "assembler" -- GCC_JIT_OUTPUT_KIND_ASSEMBLER
            "object file" -- GCC_JIT_OUTPUT_KIND_OBJECT_FILE
            "dynamic library" -- GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
            "executable" -- GCC_JIT_OUTPUT_KIND_EXECUTABLE
            "assembler" -- GCC_JIT_OUTPUT_KIND_ASSEMBLER
            "object file" -- GCC_JIT_OUTPUT_KIND_OBJECT_FILE
            "dynamic library" -- GCC_JIT_OUTPUT_KIND_DYNAMIC_LIBRARY
            "executable" -- GCC_JIT_OUTPUT_KIND_EXECUTABLE
        end

        acquire: function(): Context

        set_option: function(self: Context, option: StringOption, value: string)
        set_option: function(self: Context, option: IntegerOption, value: integer)
        set_option: function(self: Context, option: BooleanOption, value: boolean)

        compile: function(self: Context): Result | nil
        compile_to_file: function(self: Context, kind: CompilationKind, filename: string)

        dump_reproducer_to_file: function(self: Context, filename: string)
        dump_to_file: function(self: Context, filename: string, update_locations: boolean)

        enable_dump: function(self: Context, name: string): ffi.CData

        get_builtin_function: function(self: Context, name: string): Function

        get_first_error: function(self: Context): string
        get_last_error: function(self: Context): string

        get_timer: function(self: Context): Timer
        get_type: function(self: Context, name: Type.Name): Type
        get_type: function(self: Context, name: Type.IntTypeName, num_bytes: integer, is_signed: boolean): Type

        new_array_access: function(self: Context, array: RValue, index: RValue, loc?: Location): LValue
        new_array_constructor: function(self: Context, element_type: Type, fields: {RValue}): RValue
        new_array_type: function(self: Context, element_type: Type, num_elements: integer, loc?: Location): Type
        new_binary_op: function(self: Context, result: Type, lhs: RValue, op: BinaryOperation, rhs: RValue, loc?: Location)
        new_bitcast: function(self: Context, target_type: Type, value: RValue, loc?: Location): RValue
        new_bitfield: function(self: Context, type: Type, name: string, width: integer, loc?: Location): Field
        new_call: function(self: Context, func: Function, result: Type, args?: {RValue}, loc?: Location): RValue
        new_call_through_ptr: function(self: Context, func_ptr: RValue, args?: {RValue}, loc?: Location): RValue
        new_case: function(self: Context, min: RValue, max: RValue, body: Block): Case
        new_cast: function(self: Context, target_type: Type, value: RValue, loc?: Location): RValue
        new_child_context: function(self: Context): Context
        new_comparison: function(self: Context, result: Type, lhs: RValue, op: ComparisonOperation, rhs: RValue, loc?: Location): RValue
        new_field: function(self: Context, type: Type, name: string, loc?: Location): Field
        new_function: function(self: Context, kind: Function.Kind, name: string, return_type: Type, params: {Param}, is_variadic?: boolean, loc?: Location): Function
        new_function_ptr_type: function(self: Context, return_type: Type, params: {Type}, is_variadic?: boolean, loc?: Location): Type
        new_global: function(self: Context, kind: GlobalKind, type: Type, name: string, loc?: Location): LValue
        new_location: function(self: Context, filename: string, line: integer, column: integer): Location
        new_opaque_struct_type: function(self: Context, name: string, loc?: Location): Struct
        new_param: function(self: Context, type: Type, name: string, loc?: Location): Param
        new_rvalue_from_int: function(self: Context, type: Type, value_type: RValue.InitalizerValueType, value: integer | ffi.CData): RValue
        new_rvalue_from_vector: function(self: Context, type: Type, values: {RValue}, loc?: Location): RValue
        new_string_literal: function(self: Context, value: string): RValue
        new_struct_constructor: function(self: Context, type: Type, fields: {Field}, values: {RValue}, loc?: Location): RValue
        new_struct_type: function(self: Context, name: string, fields: {Field}, loc?: Location): Struct
        new_unary_op: function(self: Context, result: Type, op: UnaryOperation, operand: RValue, loc?: Location): RValue
        new_union_constructor: function(self: Context, field: Field, value: RValue, loc?: Location): RValue
        new_union_type: function(self: Context, name: string, fields: {Field}, loc?: Location): Struct
        null: function(self: Context, type: Type): RValue
        zero: function(self: Context, type: Type): RValue
        one: function(self: Context, type: Type): RValue
        set_timer: function(self: Context, timer: Timer)
    end

    record ExtendedAssembly is Object
        add_clobber: function(self: ExtendedAssembly, clobber: string)
        add_input_operand: function(self: ExtendedAssembly, name: string, constraint: string, value: RValue)
        add_output_operand: function(self: ExtendedAssembly, name: string, constraint: string, dest: LValue)
        add_top_level_asm: function(self: ExtendedAssembly, asm_string: string)

        set_inline: function(self: ExtendedAssembly, is_inline: boolean)
        set_volatile: function(self: ExtendedAssembly, is_volatile: boolean)
    end

    record Function is Object
        enum Kind
            "exported" -- GCC_JIT_FUNCTION_EXPORTED
            "internal" -- GCC_JIT_FUNCTION_INTERNAL
            "imported" -- GCC_JIT_FUNCTION_IMPORTED
            "always inline" -- GCC_JIT_FUNCTION_ALWAYS_INLINE
        end

        dump_to_dot: function(self: Function, filename: string)

        get_address: function(self: Function, loc?: Location): RValue
        get_param_count: function(self: Function): integer
        get_return_type: function(self: Function): Type

        new_block: function(self: Function, name?: string): Block
        new_local: function(self: Function, type: Type, name: string, loc?: Location): LValue
    end

    record Field is Object end

    record Block is Object
        add_assignment: function(self: Block, lhs: LValue, rhs: RValue, loc?: Location)
        add_assignment_op: function(self: Block, lhs: LValue, op: BinaryOperation, rhs: RValue, loc?: Location)
        add_comment: function(self: Block, comment: string, loc?: Location)
        add_eval: function(self: Block, expr: RValue, loc?: Location)
        add_extended_asm: function(self: Block, asm: string, loc?: Location): ExtendedAssembly

        end_with_conditional: function(self: Block, condition: RValue, on_true: Block, on_false: Block, loc?: Location)
        end_with_extended_asm_goto: function(self: Block, asm: string, goto_blocks: {Block}, fallthrough_block: Block, loc?: Location): ExtendedAssembly
        end_with_jump: function(self: Block, target: Block, loc?: Location)
        end_with_return: function(self: Block, value: RValue, loc?: Location)
        end_with_switch: function(self: Block, value: RValue, on_default: Block, cases: {Case}, loc?: Location)
        end_with_void_return: function(self: Block, loc?: Location)
    end
    record Case is Object end
    record Location is Object end


    interface RValue is Object
        enum InitalizerValueType
            "int"
            "long"
            "double"
            "pointer"
        end

        dereference: function(self: RValue, loc?: Location): LValue
        dereference_field: function(self: RValue, field: Field, loc?: Location): LValue
    end
    interface LValue is Object
        enum TLSModel
            "none" -- GCC_JIT_TLS_MODEL_NONE
            "global dynamic" -- GCC_JIT_TLS_MODEL_GLOBAL_DYNAMIC
            "local dynamic" -- GCC_JIT_TLS_MODEL_LOCAL_DYNAMIC
            "initial exec" -- GCC_JIT_TLS_MODEL_INITIAL_EXEC
            "local exec" -- GCC_JIT_TLS_MODEL_LOCAL_EXEC
        end

        access_field: function(self: LValue, field: Field, loc?: Location): LValue
        as_rvalue: function(self: LValue): RValue
        get_address: function(self: LValue, loc?: Location): RValue
        get_alignment: function(self: LValue): integer
        set_alignment: function(self: LValue, alignment: integer)
        set_intaliser: function(self: LValue, initializer: RValue)
        set_link_section: function(self: LValue, section: string)
        set_register_name: function(self: LValue, name: string)
        set_require_tailcall: function(self: LValue, require_tailcall: boolean)
        set_tls_model: function(self: LValue, model: TLSModel)
    end
    record Param is LValue end

    interface Type is Object
        enum Name
            "void" -- GCC_JIT_TYPE_VOID
            "void *" -- GCC_JIT_TYPE_VOID_PTR
            "bool" -- GCC_JIT_TYPE_BOOL
            "char" -- GCC_JIT_TYPE_CHAR
            "signed char" -- GCC_JIT_TYPE_SIGNED_CHAR
            "unsigned char" -- GCC_JIT_TYPE_UNSIGNED_CHAR
            "short" -- GCC_JIT_TYPE_SHORT
            "unsigned short" -- GCC_JIT_TYPE_UNSIGNED_SHORT
            "unsigned int" --  "int" GCC_JIT_TYPE_INT
            "long" -- GCC_JIT_TYPE_LONG
            "unsigned long" -- GCC_JIT_TYPE_UNSIGNED_LONG
            "long long" -- GCC_JIT_TYPE_LONG_LONG
            "unsigned long long" -- GCC_JIT_TYPE_UNSIGNED_LONG_LONG
            "float" -- GCC_JIT_TYPE_FLOAT
            "double" -- GCC_JIT_TYPE_DOUBLE
            "long double" -- GCC_JIT_TYPE_LONG_DOUBLE
            "const char *" -- GCC_JIT_TYPE_CONST_CHAR_PTR
            "size_t" -- GCC_JIT_TYPE_SIZE_T
            "FILE *" -- GCC_JIT_TYPE_FILE_PTR
            "complex float" -- GCC_JIT_TYPE_COMPLEX_FLOAT
            "complex double" -- GCC_JIT_TYPE_COMPLEX_DOUBLE
            "complex long double" -- GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE
            "uint8_t" -- GCC_JIT_TYPE_UINT8
            "uint16_t" -- GCC_JIT_TYPE_UINT16
            "uint32_t" -- GCC_JIT_TYPE_UINT32
            "uint64_t" -- GCC_JIT_TYPE_UINT64
            "uint128_t" -- GCC_JIT_TYPE_UINT128
            "int8_t" -- GCC_JIT_TYPE_INT8
            "int16_t" -- GCC_JIT_TYPE_INT16
            "int32_t" -- GCC_JIT_TYPE_INT32
            "int64_t" -- GCC_JIT_TYPE_INT64
            "int128_t" -- GCC_JIT_TYPE_INT128
            "void" -- GCC_JIT_TYPE_VOID
            "void *" -- GCC_JIT_TYPE_VOID_PTR
            "bool" -- GCC_JIT_TYPE_BOOL
            "char" -- GCC_JIT_TYPE_CHAR
            "signed char" -- GCC_JIT_TYPE_SIGNED_CHAR
            "unsigned char" -- GCC_JIT_TYPE_UNSIGNED_CHAR
            "short" -- GCC_JIT_TYPE_SHORT
            "unsigned short" -- GCC_JIT_TYPE_UNSIGNED_SHORT
            "unsigned int" --  "int" GCC_JIT_TYPE_INT
            "long" -- GCC_JIT_TYPE_LONG
            "unsigned long" -- GCC_JIT_TYPE_UNSIGNED_LONG
            "long long" -- GCC_JIT_TYPE_LONG_LONG
            "unsigned long long" -- GCC_JIT_TYPE_UNSIGNED_LONG_LONG
            "float" -- GCC_JIT_TYPE_FLOAT
            "double" -- GCC_JIT_TYPE_DOUBLE
            "long double" -- GCC_JIT_TYPE_LONG_DOUBLE
            "const char *" -- GCC_JIT_TYPE_CONST_CHAR_PTR
            "size_t" -- GCC_JIT_TYPE_SIZE_T
            "FILE *" -- GCC_JIT_TYPE_FILE_PTR
            "complex float" -- GCC_JIT_TYPE_COMPLEX_FLOAT
            "complex double" -- GCC_JIT_TYPE_COMPLEX_DOUBLE
            "complex long double" -- GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE
            "uint8_t" -- GCC_JIT_TYPE_UINT8
            "uint16_t" -- GCC_JIT_TYPE_UINT16
            "uint32_t" -- GCC_JIT_TYPE_UINT32
            "uint64_t" -- GCC_JIT_TYPE_UINT64
            "uint128_t" -- GCC_JIT_TYPE_UINT128
            "int8_t" -- GCC_JIT_TYPE_INT8
            "int16_t" -- GCC_JIT_TYPE_INT16
            "int32_t" -- GCC_JIT_TYPE_INT32
            "int64_t" -- GCC_JIT_TYPE_INT64
            "int128_t" -- GCC_JIT_TYPE_INT128
        end
        enum IntTypeName
            "int"
        end

        aligned: function(self: Type, alignment: integer): Type
        const: function(self: Type): Type
        dyncast_array: function(self: Type): Type
        dyncast_function_ptr: function(self: Type): Type
        dyncast_vector: function(self: Type): VectorType
        get_size: function(self: Type): integer
        is_bool: function(self: Type): boolean
        is_compatable_with: function(self: Type, other: Type): boolean
        is_integral: function(self: Type): boolean
        is_pointer: function(self: Type): boolean
        is_struct: function(self: Type): boolean
        pointer: function(self: Type): Type
        unqualified: function(self: Type): Type
        vector: function(self: Type, num_elements: integer): VectorType
        volatile: function(self: Type): Type
    end
    record FunctionType is Type
        get_param: function(self: FunctionType, index: integer): Type
        get_param_count: function(self: FunctionType): integer
        get_return_type: function(self: FunctionType): Type
    end
    record Struct is {Field}, Type
        get_field: function(self: Struct, idx: integer): Field
        get_field_count: function(self: Struct): integer
        set_fields: function(self: Struct, fields: {Field}, loc?: Location)
    end
    record VectorType is Type end

    record Result is Object
        get_code: function(self: Result, symbol: string, type: string | ffi.CType): ffi.CData | nil
        get_global: function(self: Result, name: string, type: string | ffi.CType): ffi.CData | nil
    end
    record Timer is Object end

    enum UnaryOperation
        "minus"
        "-" -- GCC_JIT_UNARY_OP_MINUS
        "bitwise negate"
        "~" -- GCC_JIT_UNARY_OP_BITWISE_NEGATE
        "logical negate"
        "!" -- GCC_JIT_UNARY_OP_LOGICAL_NEGATE
        "minus"
        "-" -- GCC_JIT_UNARY_OP_MINUS
        "bitwise negate"
        "~" -- GCC_JIT_UNARY_OP_BITWISE_NEGATE
        "logical negate"
        "!" -- GCC_JIT_UNARY_OP_LOGICAL_NEGATE
    end

    enum BinaryOperation
        "plus"
        "+" -- GCC_JIT_BINARY_OP_PLUS
        "mult"
        "*" -- GCC_JIT_BINARY_OP_MULT
        "divide"
        "/" -- GCC_JIT_BINARY_OP_DIVIDE
        "modulo"
        "%" -- GCC_JIT_BINARY_OP_MODULO
        "bitwise and"
        "&" -- GCC_JIT_BINARY_OP_BITWISE_AND
        "bitwise or"
        "|" -- GCC_JIT_BINARY_OP_BITWISE_OR
        "bitwise xor"
        "^" -- GCC_JIT_BINARY_OP_BITWISE_XOR
        "left shift"
        "<<" -- GCC_JIT_BINARY_OP_LEFT_SHIFT
        "right shift"
        ">>" -- GCC_JIT_BINARY_OP_RIGHT_SHIFT
        "logical and"
        "&&" -- GCC_JIT_BINARY_OP_LOGICAL_AND
        "logical or"
        "||" -- GCC_JIT_BINARY_OP_LOGICAL_OR
        "plus"
        "+" -- GCC_JIT_BINARY_OP_PLUS
        "mult"
        "*" -- GCC_JIT_BINARY_OP_MULT
        "divide"
        "/" -- GCC_JIT_BINARY_OP_DIVIDE
        "modulo"
        "%" -- GCC_JIT_BINARY_OP_MODULO
        "bitwise and"
        "&" -- GCC_JIT_BINARY_OP_BITWISE_AND
        "bitwise or"
        "|" -- GCC_JIT_BINARY_OP_BITWISE_OR
        "bitwise xor"
        "^" -- GCC_JIT_BINARY_OP_BITWISE_XOR
        "left shift"
        "<<" -- GCC_JIT_BINARY_OP_LEFT_SHIFT
        "right shift"
        ">>" -- GCC_JIT_BINARY_OP_RIGHT_SHIFT
        "logical and"
        "&&" -- GCC_JIT_BINARY_OP_LOGICAL_AND
        "logical or"
        "||" -- GCC_JIT_BINARY_OP_LOGICAL_OR
    end

    enum ComparisonOperation
        "eq"
        "==" -- GCC_JIT_COMPARISON_EQ
        "ne"
        "!=" -- GCC_JIT_COMPARISON_NE
        "lt"
        "<" -- GCC_JIT_COMPARISON_LT
        "le"
        "<=" -- GCC_JIT_COMPARISON_LE
        "gt"
        ">" -- GCC_JIT_COMPARISON_GT
        "ge"
        ">=" -- GCC_JIT_COMPARISON_GE
        "eq"
        "==" -- GCC_JIT_COMPARISON_EQ
        "ne"
        "!=" -- GCC_JIT_COMPARISON_NE
        "lt"
        "<" -- GCC_JIT_COMPARISON_LT
        "le"
        "<=" -- GCC_JIT_COMPARISON_LE
        "gt"
        ">" -- GCC_JIT_COMPARISON_GT
        "ge"
        ">=" -- GCC_JIT_COMPARISON_GE
    end

    enum GlobalKind
        "exported" -- GCC_JIT_GLOBAL_EXPORTED
        "internal" -- GCC_JIT_GLOBAL_INTERNAL
        "imported" -- GCC_JIT_GLOBAL_IMPORTED
    end
end

return libgccjit
