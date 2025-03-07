#include "llvm.hpp"
#include "teal-parser/AST.hpp"

using namespace teal::compiler::codegen::llvm;
using namespace teal::parser::ast;

#define $visitor(T) $visitor_decl(T, CodeGenerator)

$visitor(NumberExpression)
{
    if (node.value.find(".") == std::string::npos)
        return ::llvm::ConstantInt::get(_ctx, ::llvm::APInt(64, std::stol(node.value)));
    else
        return ::llvm::ConstantFP::get(_ctx, ::llvm::APFloat(std::stod(node.value)));
}

$visitor(Block)
{
    std::vector<std::any> exprs;
    for (const auto &expr : node.statements) {
        exprs.push_back(visit(*expr.get()));
    }
    return exprs;
}

