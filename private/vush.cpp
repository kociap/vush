#include <vush/vush.hpp>

#include <const_expr_eval.hpp>
#include <context.hpp>
#include <filesystem.hpp>
#include <hierarchy_printer.hpp>
#include <parser.hpp>
#include <symbol.hpp>
#include <utility.hpp>

#include <iostream>

namespace vush {
    static Expected<std::string, String> resolve_import_path(Context& ctx, std::string const& current_path, std::string const& import_path) {
        bool found = false;
        std::string out_path;
        for(char const* const* path = ctx.import_paths; path != ctx.import_paths_end; ++path) {
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

    static Expected<Owning_Ptr<Declaration_List>, String> process_file_decl_ifs_and_resolve_imports(Context& ctx, std::string const& path) {
        Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_result = parse_file(path);
        if(!parse_result) {
            Parse_Error error = std::move(parse_result.error());
            String error_msg = build_error_message(path, error.line, error.column, error.message);
            return {expected_error, std::move(error_msg)};
        }

        std::string const* prev_file = ctx.current_file;
        ctx.current_file = &path;

        Owning_Ptr<Declaration_List>& ast = parse_result.value();
        for(i64 i = 0; i < (i64)ast->declarations.size();) {
            switch(ast->declarations[i]->node_type) {
                case AST_Node_Type::import_decl: {
                    Owning_Ptr<Import_Decl> node = static_cast<Import_Decl*>(ast->declarations[i].release());
                    Expected<std::string, String> import_path = resolve_import_path(ctx, path, node->path);
                    if(!import_path) {
                        return {expected_error, std::move(import_path.error())};
                    }

                    Expected<Owning_Ptr<Declaration_List>, String> result = process_file_decl_ifs_and_resolve_imports(ctx, import_path.value());
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
                    Expected<Const_Expr_Value, String> result = evaluate_expr(ctx, *node->condition);
                    if(!result) {
                        return {expected_error, std::move(result.error())};
                    }

                    if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                        return {expected_error, build_error_message(*ctx.current_file, 0, 0, u8"expression is not implicitly convertible to bool")};
                    }

                    ast->declarations.erase(ast->declarations.begin() + i);
                    Declaration_List* decls = (result.value().as_boolean() ? node->true_declarations.get() : node->false_declarations.get());
                    if(decls) {
                        auto begin = std::make_move_iterator(decls->declarations.begin());
                        auto end = std::make_move_iterator(decls->declarations.end());
                        ast->declarations.insert(ast->declarations.begin() + i, begin, end);
                    }
                } break;

                case AST_Node_Type::function_declaration: {
                    Function_Declaration* node = static_cast<Function_Declaration*>(ast->declarations[i].get());
                    ctx.global_symbols.emplace(node->name->identifier, Symbol{Symbol_Type::function, node});
                    i += 1;
                } break;

                case AST_Node_Type::variable_declaration: {
                    String error_msg = build_error_message(path, 0, 0, u8"illegal variable declaration in global scope");
                    return {expected_error, std::move(error_msg)};
                } break;

                case AST_Node_Type::constant_declaration: {
                    Constant_Declaration* node = static_cast<Constant_Declaration*>(ast->declarations[i].get());
                    ctx.global_symbols.emplace(node->identifier->identifier, Symbol{Symbol_Type::constant, node});
                    i += 1;
                } break;

                default: {
                    i += 1;
                }
            }
        }

        ctx.current_file = prev_file;
        return {expected_value, std::move(ast)};
    }

    Expected<Compiled_File, String> compile_to_glsl(char const* source_path, char const* const* const import_paths, i64 const import_paths_count,
                                                    Constant_Define const* const defines, i64 const defines_count) {
        Context ctx = {};
        ctx.import_paths = import_paths;
        ctx.import_paths_end = import_paths + import_paths_count;
        std::vector<Owning_Ptr<Declaration>> constant_decls;
        {
            Constant_Define const* const defines_end = defines + defines_count;
            for(Constant_Define const* define = defines; define != defines_end; ++define) {
                Symbol symbol;
                symbol.type = Symbol_Type::constant;
                Constant_Declaration* decl = new Constant_Declaration(new Builtin_Type(Builtin_GLSL_Type::glsl_int), new Identifier(define->name),
                                                                      new Integer_Literal(std::to_string(define->value)));
                constant_decls.emplace_back(decl);
                symbol.declaration = decl;
                ctx.global_symbols.emplace(define->name, symbol);
            }
        }
        std::string path = source_path;
        Expected<Owning_Ptr<Declaration_List>, String> result = process_file_decl_ifs_and_resolve_imports(ctx, path);
        if(!result) {
            return {expected_error, std::move(result.error())};
        }

        Hierarchy_Printer printer(std::cout);
        printer.print_hierarchy(*result.value());

        for(auto& kv: ctx.global_symbols) {
            std::cout << kv.first << '\n';
        }

        return {expected_value};
    }
} // namespace vush
