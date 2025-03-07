#include "llvm.hpp"
#include "teal-parser/AST.hpp"
#include <cstdio>
#include <llvm-19/llvm/IR/DerivedTypes.h>
#include <llvm-19/llvm/Support/raw_ostream.h>
#include <llvm/IR/Type.h>
#include <print>
#include <fstream>

using namespace teal::compiler::codegen;
using namespace teal::parser::ast;

#define $visitor(T) $visitor_decl(T, LLVMCodeGenerator)

static class TypeConverter {

public:
    ::llvm::Type *operator()()
    {
        return nullptr;
    }
} type_to_llvm [[gnu::used]];

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
        ostream << t;
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
        return ::llvm::ConstantInt::get(_ctx, ::llvm::APInt(64, std::stol(node.value)));
    else
        return ::llvm::ConstantFP::get(_ctx, ::llvm::APFloat(std::stod(node.value)));
}

$visitor(Block)
{
    std::vector<std::any> exprs;
    for (const auto &expr : node.statements) {
        exprs.push_back(visit(*expr));
    }
    return exprs;
}

$visitor(BasicTypeNode)
{
    if (node.name == "integer")
        return dynamic_cast<::llvm::Type *>(::llvm::IntegerType::get(_ctx, 64));
    else
        throw Backend::CompilationException();
}

$visitor(FunctionDeclarationStatement)
{
    auto params = std::vector<::llvm::Type *>(node.body->parameters.size());
    auto ret_t = visit(*node.body->return_types.at(0)); //TODO: multi return count
    auto f = ::llvm::FunctionType::get(std::any_cast<::llvm::Type *>(ret_t), params, false);
    // ::llvm::outs() << *f;

    // FILE *fp = fopen("tmpbuf", "w+");
    // auto raw_sstr = llvm::raw_fd_ostream(fileno(fp), false);
    // raw_sstr << *f;
    // raw_sstr.flush();
    // auto fit = std::ifstream("tmpbuf");
    // auto s = std::string(std::istreambuf_iterator<char>{fit}, {});
    // fclose(fp);
    // remove("tmpbuf");
    std::println("func: {}", *f);
    return f;
}

