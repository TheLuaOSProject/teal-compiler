#pragma once

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include "backends.hpp"


namespace teal::compiler::codegen
{
    class LLVMCompilationOptions : public Backend::BaseCompilationOptions {
    public:
        ~LLVMCompilationOptions() {}
    };

    class LLVMCodeGenerator : public Backend {
    public:
        LLVMCodeGenerator(std::unique_ptr<teal::parser::ast::ASTNode> &&root, const LLVMCompilationOptions &opts):
            Backend(std::move(root), opts),
            _ctx(),
            _builder(_ctx),
            _module("net.friyet.teal-compiler", _ctx)
        {}

    ~LLVMCodeGenerator() = default;
    protected:
        using Backend::visit;
        $visitor_decl(NumberExpression) override;
        $visitor_decl(Block) override;
        $visitor_decl(FunctionDeclarationStatement) override;
        $visitor_decl(BasicTypeNode) override;
    private:
        ::llvm::LLVMContext _ctx;
        ::llvm::IRBuilder<> _builder;
        ::llvm::Module _module;

    };
}

