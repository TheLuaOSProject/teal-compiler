#include "code-generation/llvm.hpp"
#include "teal-parser/Lexer.hpp"
#include "teal-parser/Parser.hpp"
#include <print>
#include <filesystem>
#include <fstream>
#include <unistd.h>
#include <argparse/argparse.hpp>

_LIBCPP_BEGIN_NAMESPACE_STD

bool __is_posix_terminal(std::FILE *f)
{
    return isatty(fileno(f));
}

_LIBCPP_END_NAMESPACE_STD

std::string read_all(const std::filesystem::path &path) {
    if (not std::filesystem::exists(path)) {
        throw std::ios_base::failure("File does not exist: " + path.string());
    }

    const auto file_size = std::filesystem::file_size(path);

    constexpr std::uintmax_t MAX_SIZE = 100 * 1024 * 1024;
    if (file_size > MAX_SIZE) {
        throw std::runtime_error(std::format("File too large: {} ({} bytes)", path.string(), file_size));
    }

    std::ifstream stream(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>{stream}, {});
}

int main(int argc, const char *argv[])
{
    auto ap = argparse::ArgumentParser("teal-compiler");
    ap.add_description("Compiler for the teal programming language");

    ap.add_argument("file")
        .help("teal file to run");

    ap.add_argument("--output")
        .help("output file");

    ap.parse_args(argc, argv);

    teal::compiler::codegen::LLVMCompilationOptions opts;
    opts.source = std::filesystem::path(ap.get("file"));
    if (auto out = ap.present("--output")) {
        opts.compile_to = std::filesystem::path(*out);
    } else {
        opts.compile_to = std::filesystem::path(opts.source).replace_extension(".out");
    }

    auto lexer = teal::parser::Lexer(read_all(opts.source));
    auto [tokens, errs] = lexer.tokenize();
    if (errs.size() > 0) {
        for (const auto &err : errs) {
            std::println(stderr, "Lex error ({}:{}:{}): {}", opts.source.string(), err.line, err.column, err.to_string());
        }

        return 1;
    }

    auto parser = teal::parser::Parser(tokens);
    auto [root, perrs] = parser.parse();
    if (perrs.size() > 0) {
        for (const auto &err : perrs) {
            std::println(stderr, "Parse error ({}:{}:{}): {}", opts.source.string(), err.line, err.column, err.message);
        }

        return 1;
    }

    auto codegen = teal::compiler::codegen::LLVMCodeGenerator(std::move(root), opts);

    try {
        codegen.compile();
    } catch(const teal::compiler::codegen::Backend::NodeNotImplementedException &node) {
        std::println(stderr, "Not implemented: {}", node.message);
    } catch (const teal::compiler::codegen::Backend::CompilationException &ex) {
        std::println(stderr, "Codegen error: {}", ex.what());
    }
}
