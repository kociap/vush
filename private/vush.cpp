#include <vush/vush.hpp>

#include <filesystem.hpp>
#include <hierarchy_printer.hpp>
#include <import_resolver.hpp>
#include <parser.hpp>

#include <iostream>

namespace vush {
    static Expected<std::vector<std::string>, String> resolve_import_paths(std::string const& current_path, Owning_Ptr<Declaration_List> const& ast,
                                                                           char const* const* const import_paths, char const* const* const import_paths_end) {
        Import_Resolver import_resolver;
        ast->visit(import_resolver);

        std::vector<std::string> resolved_imports;
        resolved_imports.reserve(import_resolver.imports.size());
        for(std::string const& import_path: import_resolver.imports) {
            bool found = false;
            for(char const* const* path = import_paths; path != import_paths_end; ++path) {
                std::string resolved_path = fs::concat_paths(*path, import_path);
                bool exists = fs::exists(resolved_path);
                if(exists) {
                    if(!found) {
                        found = true;
                        resolved_imports.emplace_back(std::move(resolved_path));
                    } else {
                        std::string msg = current_path + u8": error: ambiguous import path";
                        char* str_data = (char*)::operator new(msg.size() + 1);
                        memcpy(str_data, msg.data(), msg.size() + 1);
                        return {expected_error, str_data, (i64)msg.size()};
                    }
                }
            }

            if(!found) {
                std::string msg = current_path + u8": error: could not resolve import path";
                char* str_data = (char*)::operator new(msg.size() + 1);
                memcpy(str_data, msg.data(), msg.size() + 1);
                return {expected_error, str_data, (i64)msg.size()};
            }
        }
        return {expected_value, std::move(resolved_imports)};
    }

    static Expected<Owning_Ptr<Declaration_List>, String> process_imports(std::string const& path, char const* const* const import_paths,
                                                                          char const* const* const import_paths_end) {
        Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_result = parse_file(path);
        if(!parse_result) {
            Parse_Error error = std::move(parse_result.error());
            std::string msg = std::move(path);
            msg += ":";
            msg += std::to_string(error.line + 1);
            msg += ":";
            msg += std::to_string(error.column + 1);
            msg += ": error: ";
            msg += error.message;
            char* str_data = (char*)::operator new(msg.size() + 1);
            memcpy(str_data, msg.data(), msg.size() + 1);
            return {expected_error, str_data, (i64)msg.size()};
        }

        Owning_Ptr<Declaration_List>& ast = parse_result.value();

        Expected<std::vector<std::string>, String> resolve_result = resolve_import_paths(path, ast, import_paths, import_paths_end);
        if(!resolve_result) {
            return {expected_error, std::move(resolve_result.error())};
        }

        for(std::string const& p: resolve_result.value()) {
            Expected<Owning_Ptr<Declaration_List>, String> result = process_imports(p, import_paths, import_paths_end);
            if(result) {
                for(auto& decl: ast->declarations) {
                    result.value()->append(decl.release());
                }
                ast = std::move(result.value());
            } else {
                return {expected_error, std::move(result.error())};
            }
        }

        return {expected_value, std::move(ast)};
    }

    Expected<Compiled_File, String> compile_to_glsl(char const* source_path, char const* const* const import_paths, i64 const import_paths_count) {
        std::string path = source_path;
        Expected<Owning_Ptr<Declaration_List>, String> result = process_imports(path, import_paths, import_paths + import_paths_count);
        if(!result) {
            return {expected_error, std::move(result.error())};
        }

        Hierarchy_Printer printer(std::cout);
        result.value()->visit(printer);

        return {expected_value};
    }
} // namespace vush
