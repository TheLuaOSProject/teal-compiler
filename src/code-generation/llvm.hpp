#pragma once

#include <map>
#include <stack>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include "backends.hpp"


namespace teal::compiler::codegen
{
    class LLVMCodeGenerator : public Backend {
    public:
        class NestedFunctionsNotSupportedException : CompilationException {};
        class FunctionNotFoundException : CompilationException {};

        class CompilationOptions : public Backend::BaseCompilationOptions {
        public:
            ~CompilationOptions() {}
        };


        LLVMCodeGenerator(std::unique_ptr<teal::parser::ast::ASTNode> &&root, const CompilationOptions &opts):
            Backend(std::move(root), opts),
            _ctx(),
            _builder(_ctx),
            _module("net.friyet.teal-compiler", _ctx),
            _func_ctx(nullptr),
            _type_converter(&_ctx)
        {}

    ~LLVMCodeGenerator() = default;
    protected:
        using Backend::visit;
        $visitor_decl(NumberExpression) override;
        $visitor_decl(Block) override;
        $visitor_decl(FunctionDeclarationStatement) override;
        $visitor_decl(BasicTypeNode) override;
        $visitor_decl(FunctionBody) override;
        $visitor_decl(ReturnStatement) override;
        $visitor_decl(FunctionCallExpression) override;
    private:
        llvm::LLVMContext _ctx;
        llvm::IRBuilder<> _builder;
        llvm::Module _module;
        
        struct FunctionContext {
            std::string name;
            parser::ast::Visibility visibility;
            std::map<std::string, llvm::Value *> named_values;
            llvm::FunctionType *type;
            llvm::Function *func;
        } *_func_ctx;

        class TypeConverter {
            public:
                TypeConverter(llvm::LLVMContext *ctx)
                    : _ctx(ctx)
                {}

                llvm::Type *operator()(const teal::parser::ast::BasicTypeNode &node);
            private:
                ::llvm::LLVMContext *_ctx;
        } _type_converter;
    };
}

