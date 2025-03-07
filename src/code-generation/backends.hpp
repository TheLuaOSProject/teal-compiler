#pragma once
#include <exception>
#include <filesystem>
#include <functional>
#include <any>

#include "utilities.hpp"
#include "teal-parser/AST.hpp"

#define $statment_nodes\
    $(ReturnStatement);\
    $(BreakStatement);\
    $(GotoStatement);\
    $(LabelStatement);\
    \
    $(DoStatement);\
    $(IfStatement);\
    $(WhileStatement);\
    $(RepeatStatement);\
    $(ForNumericStatement);\
    $(ForInStatement);\
    \
    $(FunctionDeclarationStatement);\
    $(VariableDeclarationStatement);\
    $(RecordDeclarationStatement);\
    $(EnumDeclarationStatement);\
    $(TypeAliasStatement);\
\
    $(AssignmentStatement);\
    $(CallStatement);

#define $expression_nodes\
    $(NameExpression);\
    $(NumberExpression);\
    $(StringExpression);\
    $(BooleanExpression);\
    $(NilExpression);\
    $(VarargExpression);\
    $(FunctionCallExpression);\
    $(IndexExpression);\
    $(FieldExpression);\
    \
    $(BinaryOperationExpression);\
    $(UnaryOperationExpression);\
    $(FunctionDefinitionExpression);\
    $(CastExpression);\
    $(IsTypeExpression);\
    $(TableConstructorExpression);\

#define $type_nodes\
    $(BasicTypeNode);\
    $(NominalTypeNode);\
    $(TableTypeNode);\
    $(UnionTypeNode);\
    $(TypeRecordNode);\
    $(TypeEnumNode);\
    $(RequireTypeNode);

#define $ast_nodes\
    $(Block);\
    $(FunctionBody);\
    $(EnumBody);\
    $(RecordBody);\
    $type_nodes\
    $expression_nodes\
    $statment_nodes


namespace teal::compiler::codegen
{
    class Backend {
    public:
        class BaseCompilationOptions {
        public:
            std::filesystem::path source, compile_to;

            virtual ~BaseCompilationOptions() = default;
        };

        class CompilationException : public std::exception {
        };

        class NodeNotImplementedException : CompilationException {
        public:
            std::string message;

            NodeNotImplementedException(const std::string_view &src, const parser::ast::ASTNode &node):
                message(std::format("`{}` at {}:{}:{} is not implemented yet!", utilities::demangle(typeid(node).name()), src, node.line, node.column))
            {}

            const char *what() const noexcept override
            {
                return message.c_str();
            }
        };

        Backend(std::unique_ptr<teal::parser::ast::ASTNode> &&root, const BaseCompilationOptions &opts):
            _ast_root(std::move(root)),
            _options(opts)
        {}

        virtual void compile()
        { visit(*_ast_root.get()); }

        virtual ~Backend() = default;
    protected:
        std::any visit(const teal::parser::ast::ASTNode &ptr)
        {
#define $(T) if (auto *raw = dynamic_cast<const teal::parser::ast::T *>(&ptr)) return visit(*raw)
            $ast_nodes
#undef $
            throw NodeNotImplementedException(_options.source.string(), ptr);
        }

        std::unique_ptr<teal::parser::ast::ASTNode> _ast_root;
        const BaseCompilationOptions &_options;


#define $visitor_decl(T, ...) std::any __VA_OPT__(__VA_ARGS__::)visit(const teal::parser::ast::T &node)
#define $undefined_visitor(...)\
        $visitor_decl(__VA_ARGS__)\
        { throw NodeNotImplementedException(_options.compile_to.string(), node); }


#define $(T) virtual $visitor_decl(T);
        $ast_nodes
#undef $


    };
}
