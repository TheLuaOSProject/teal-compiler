#pragma once

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include "backends.hpp"


namespace teal::compiler::codegen::llvm
{
    class CompilationOptions : public Backend::BaseCompilationOptions {
    public:
        ~CompilationOptions() {}
    };

    class CodeGenerator : public Backend {
    public:
        CodeGenerator(std::unique_ptr<teal::parser::ast::ASTNode> &&root, const CompilationOptions &opts):
            Backend(std::move(root), opts),
            _ctx(),
            _builder(_ctx),
            _module("net.friyet.teal-compiler", _ctx)
        {}

    ~CodeGenerator() = default;
    protected:
        using Backend::visit;
        $visitor_decl(NumberExpression) override;
        $visitor_decl(Block) override;
    private:
        ::llvm::LLVMContext _ctx;
        ::llvm::IRBuilder<> _builder;
        ::llvm::Module _module;

    };
}

