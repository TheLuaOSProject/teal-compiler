#include "code-generation/llvm.hpp"
#include "teal-parser/AST.hpp"

#include <print>
#include <fstream>
#include <cstdio>
#include <unistd.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
// #include <llvm/IR/AssemblyAnnotationWriter.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>



_LIBCPP_BEGIN_NAMESPACE_STD

bool __is_posix_terminal(std::FILE *f)
{
    return isatty(fileno(f));
}

_LIBCPP_END_NAMESPACE_STD

using namespace teal::compiler::codegen;
using namespace teal::parser::ast;


#define $visitor(T) $visitor_decl(T, LLVMCodeGenerator)

llvm::Type *LLVMCodeGenerator::TypeConverter::operator()(const BasicTypeNode &node)
{
    if (node.name == "integer")
    return dynamic_cast<::llvm::Type *>(::llvm::IntegerType::get(*_ctx, 64));
    else
        throw Backend::CompilationException();
    return nullptr;
}

template<typename T>
concept IsPrintable = requires(T t, ::llvm::raw_string_ostream s) {
    t.print(s);
};

template<IsPrintable T>
struct std::formatter<T> {
    static constexpr auto TMPBUF_PATH = "/tmp/teal-compiler-tmpbuf-for-llvm-type-formatting-{}";

    static FILE *_tmpbuf;

    constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }
    constexpr auto format(const T &t, std::format_context& ctx) const
    {
        auto tmpbuf_path = std::format(TMPBUF_PATH, std::time(nullptr));
        if (not _tmpbuf) {
            _tmpbuf = std::fopen(tmpbuf_path.c_str(), "w+b");
            //todo: nullptr check adn throw exception
        }

        size_t current_offset = ftell(_tmpbuf);
        //let it be known I hate LLVM
        auto ostream = llvm::raw_fd_ostream(fileno(_tmpbuf), false);
        t.print(ostream);
        ostream.flush();
        auto inf = std::ifstream(tmpbuf_path);
        inf.seekg(current_offset);
        return std::format_to(ctx.out(), "{}", std::string(std::istream_iterator<char>(inf), {}));
    }
};

template<>
struct std::formatter<llvm::Function> {
    constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }
    constexpr auto format(const llvm::Function &t, std::format_context &ctx) const
    {
        using LLVMFormatter = std::formatter<llvm::Value>;
        auto tmpbuf_path = std::format(LLVMFormatter::TMPBUF_PATH, std::time(nullptr));
        if (not LLVMFormatter::_tmpbuf) {
            LLVMFormatter::_tmpbuf = std::fopen(tmpbuf_path.c_str(), "w+b");
            //todo: nullptr check adn throw exception
        }

        size_t current_offset = ftell(LLVMFormatter::_tmpbuf);
        auto ostream = llvm::raw_fd_ostream(fileno(LLVMFormatter::_tmpbuf), false);
        t.print(ostream, nullptr, false, true);
        ostream.flush();
        auto inf = std::ifstream(tmpbuf_path);
        inf.seekg(current_offset);
        return std::format_to(ctx.out(), "{}", std::string(std::istream_iterator<char>(inf), {}));
    }
};

template<IsPrintable T>
FILE *std::formatter<T>::_tmpbuf;

$visitor(NumberExpression)
{
    if (node.value.find(".") == std::string::npos)
        return static_cast<llvm::Value *>(llvm::ConstantInt::get(_ctx, ::llvm::APInt(64, std::stol(node.value))));
    else
        return static_cast<llvm::Value *>(llvm::ConstantFP::get(_ctx, ::llvm::APFloat(std::stod(node.value))));
}

$visitor(Block)
{
    std::vector<std::any> exprs;
    for (const auto &expr : node.statements) {
        exprs.push_back(visit(*expr));
    }
    return exprs;
}

$visitor(ReturnStatement)
{
    return _builder.CreateRet(visit(*node.values.at(0)).to<llvm::Value *>());
}

$visitor(BasicTypeNode)
{ return _type_converter(node); }

$visitor(FunctionBody)
{
    auto params = std::vector<::llvm::Type *>();

    for (const auto &param : node.parameters) {
        params.push_back(visit(*param.type).to<llvm::Type *>());
    }

    //TODO: multi return type
    _func_ctx->type = ::llvm::FunctionType::get(visit(*node.return_types.at(0)).to<llvm::Type *>(), params, false);
    
    _func_ctx->func = llvm::Function::Create(_func_ctx->type, llvm::Function::ExternalLinkage, _func_ctx->name, _module);
    int i = 0;
    for (auto &arg : _func_ctx->func->args()) {
        arg.setName(node.parameters[i++].name);
    }

    auto block = llvm::BasicBlock::Create(_ctx, std::format("<function {} ({}) @{}:{}:{}>", _func_ctx->name, static_cast<void *>(_func_ctx->func),  _options.source.string(), node.line, node.column), _func_ctx->func);
    _builder.SetInsertPoint(block);

    for (auto &arg : _func_ctx->func->args())
        _func_ctx->named_values[std::string(arg.getName())] = &arg;

    return visit(*node.body);
}

$visitor(FunctionCallExpression)
{
    auto callee = _module.getFunction(node.method_name);
    if (not callee)
        throw FunctionNotFoundException();

    if (callee->arg_size() != node.arguments.size())
        throw FunctionNotFoundException();

    auto args = std::vector<llvm::Value *>();
    for (const auto &param : node.arguments) {
        args.push_back(visit(*param).to<llvm::Value *>());
    }

    return _builder.CreateCall(callee, args, std::format("<call to {}>", node.method_name));
}

$visitor(FunctionDeclarationStatement)
{
    if (_func_ctx != nullptr)
        throw NestedFunctionsNotSupportedException();

    auto ctx = FunctionContext {
        .name = node.name_path.at(0), //TODO: proper names
        .visibility = node.visibility,
        .named_values = {},
        .type = nullptr,
        .func = nullptr
    };
    _func_ctx = &ctx;
    visit(*node.body);
    _func_ctx = nullptr;

    llvm::verifyFunction(*ctx.func, &llvm::errs());
    
    return ctx.func;


    // _functions.push({ func, {} });
    // visit(*node.body);
    // _functions.pop();
    
    // llvm::verifyFunction(*func);
    // std::println("{}", *func);
    // // llvm::outs() << *func;
}

