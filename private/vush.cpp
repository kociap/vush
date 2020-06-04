#include <vush/vush.hpp>

#include <filesystem.hpp>
#include <hierarchy_printer.hpp>
#include <parser.hpp>

#include <iostream>

namespace vush {
    static String build_error_message(std::string const& path, i64 const line, i64 const column, std::string const& message) {
        std::string msg = std::move(path) + u8":" + std::to_string(line + 1) + u8":" + std::to_string(column + 1) + u8": error: " + message;
        char* str_data = (char*)::operator new(msg.size() + 1);
        memcpy(str_data, msg.data(), msg.size() + 1);
        return {str_data, (i64)msg.size()};
    }

    static Expected<std::string, String> resolve_import_path(std::string const& current_path, std::string const& import_path,
                                                             char const* const* const import_paths, char const* const* const import_paths_end) {
        bool found = false;
        std::string out_path;
        for(char const* const* path = import_paths; path != import_paths_end; ++path) {
            std::string resolved_path = fs::concat_paths(*path, import_path);
            bool exists = fs::exists(resolved_path);
            if(exists) {
                if(!found) {
                    found = true;
                    out_path = std::move(resolved_path);
                } else {
                    String error_msg = build_error_message(current_path, 0, 0, u8": error: ambiguous import path");
                    return {expected_error, std::move(error_msg)};
                }
            }
        }

        if(found) {
            return {expected_value, std::move(out_path)};
        } else {
            String error_msg = build_error_message(current_path, 0, 0, u8": error: could not resolve import path");
            return {expected_error, std::move(error_msg)};
        }
    }

    static Expected<Owning_Ptr<Declaration_List>, String>
    process_file_decl_ifs_and_resolve_imports(std::string const& path, char const* const* const import_paths, char const* const* const import_paths_end) {
        Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_result = parse_file(path);
        if(!parse_result) {
            Parse_Error error = std::move(parse_result.error());
            String error_msg = build_error_message(path, error.line, error.column, error.message);
            return {expected_error, std::move(error_msg)};
        }

        Owning_Ptr<Declaration_List>& ast = parse_result.value();
        for(i64 i = 0; i < (i64)ast->declarations.size();) {
            switch(ast->declarations[i]->node_type) {
                case AST_Node_Type::import_decl: {
                    Owning_Ptr<Import_Decl> node = static_cast<Import_Decl*>(ast->declarations[i].release());
                    Expected<std::string, String> import_path = resolve_import_path(path, node->path, import_paths, import_paths_end);
                    if(!import_path) {
                        return {expected_error, std::move(import_path.error())};
                    }

                    Expected<Owning_Ptr<Declaration_List>, String> result =
                        process_file_decl_ifs_and_resolve_imports(import_path.value(), import_paths, import_paths_end);
                    if(!result) {
                        return {expected_error, std::move(result.error())};
                    }

                    ast->declarations.erase(ast->declarations.begin() + i);
                    Declaration_List* decls = result.value().get();
                    auto begin = std::make_move_iterator(decls->declarations.begin());
                    auto end = std::make_move_iterator(decls->declarations.end());
                    ast->declarations.insert(ast->declarations.begin() + i, begin, end);
                } break;

                case AST_Node_Type::declaration_if: {
                    Owning_Ptr<Declaration_If> node = static_cast<Declaration_If*>(ast->declarations[i].release());
                    // Expected<bool, String> result = evaluate_constant_bool_expr(node->condition);
                    // if(!result) {
                    //     return {expected_error, std::move(result.error())};
                    // }

                    ast->declarations.erase(ast->declarations.begin() + i);
                    // Declaration_List* decls = (result.value() ? node->true_declarations.get() : node->false_declarations.get());
                    Declaration_List* decls = node->true_declarations.get();
                    if(decls) {
                        auto begin = std::make_move_iterator(decls->declarations.begin());
                        auto end = std::make_move_iterator(decls->declarations.end());
                        ast->declarations.insert(ast->declarations.begin() + i, begin, end);
                    }
                } break;

                default: {
                    i += 1;
                }
            }
        }

        return {expected_value, std::move(ast)};
    }

    Expected<Compiled_File, String> compile_to_glsl(char const* source_path, char const* const* const import_paths, i64 const import_paths_count) {
        std::string path = source_path;
        Expected<Owning_Ptr<Declaration_List>, String> result =
            process_file_decl_ifs_and_resolve_imports(path, import_paths, import_paths + import_paths_count);
        if(!result) {
            return {expected_error, std::move(result.error())};
        }

        Hierarchy_Printer printer(std::cout);
        result.value()->visit(printer);

        return {expected_value};
    }
} // namespace vush
