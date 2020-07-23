// TODO: dynamic ifs
// TODO: Annotated variables
// TODO: constant initializers in structs

#include <vush/vush.hpp>

#include <anton/algorithm.hpp>
#include <anton/filesystem.hpp>
#include <anton/iterators.hpp>
#include <codegen.hpp>
#include <const_expr_eval.hpp>
#include <context.hpp>
#include <parser.hpp>
#include <string_stream.hpp>
#include <symbol.hpp>
#include <utility.hpp>

namespace vush {
    // process_fn_param_list
    // Resolves any function parameter ifs and validates that the following requirements are met:
    //  - for functions only ordinary parameters are allowed
    //  - for vertex stages only vertex input parameters and sourced parameters are allowed
    //  - for fragment stages all parameters must be sourced with the exception of the first one which might be an ordinary parameter that is used as the input from the previous stage
    //  - for compute stages only sourced parameters are allowed
    //  - all ordinary parameters that are arrays are sized
    //  - all sourced parameters are of non-opaque types or are sized arrays of non-opaque types
    //
    static anton::Expected<void, anton::String> process_fn_param_list(Context& ctx, Function_Declaration& function) {
        anton::Array<Owning_Ptr<Function_Param>>& params = function.params;
        for(i64 i = 0; i < params.size();) {
            // If the node is a prameter if, we replace it with the contents of one of the branches.
            // We do not advance in that case in order to check the replaced node.
            if(params[i]->node_type == AST_Node_Type::function_param_if) {
                Function_Param_If& node = (Function_Param_If&)*params[i];
                anton::Expected<Expr_Value, anton::String> result = evaluate_const_expr(ctx, *node.condition);
                if(!result) {
                    return {anton::expected_error, anton::move(result.error())};
                }

                if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                    Source_Info const& src = node.source_info;
                    return {anton::expected_error,
                            build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                }

                if(result.value().as_boolean()) {
                    if(node.true_param) {
                        params[i] = anton::move(node.true_param);
                    } else {
                        params.erase(params.begin() + i, params.begin() + i + 1);
                    }
                } else {
                    if(node.false_param) {
                        params[i] = anton::move(node.false_param);
                    } else {
                        params.erase(params.begin() + i, params.begin() + i + 1);
                    }
                }
            } else {
                i += 1;
            }
        }

        for(auto& param: params) {
            if(param->node_type == AST_Node_Type::ordinary_function_param) {
                Ordinary_Function_Param& node = (Ordinary_Function_Param&)*param;
                Type& type = *node.type;

                // Ordinary parameters that are arrays must be sized
                if(is_unsized_array(type)) {
                    Source_Info const& src = type.source_info;
                    return {anton::expected_error,
                            build_error_message(src.file_path, src.line, src.column, u8"ordinary parameters must not be unsized arrays")};
                }
            } else if(param->node_type == AST_Node_Type::sourced_function_param) {
                Source_Info const& src = param->source_info;
                return {anton::expected_error,
                        build_error_message(src.file_path, src.line, src.column, u8"sourced parameters are not allowed on ordinary functions")};
            } else if(param->node_type == AST_Node_Type::vertex_input_param) {
                Source_Info const& src = param->source_info;
                return {anton::expected_error,
                        build_error_message(src.file_path, src.line, src.column, u8"vertex input parameters are not allowed on ordinary functions")};
            } else {
                Source_Info const& src = param->source_info;
                return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"unknown parameter type")};
            }
        }

        return {anton::expected_value};
    }

    static anton::Expected<void, anton::String> process_fn_param_list(Context& ctx, Pass_Stage_Declaration& function) {
        anton::Array<Owning_Ptr<Function_Param>>& params = function.params;
        for(i64 i = 0; i < params.size();) {
            // If the node is a prameter if, we replace it with the contents of one of the branches.
            // We do not advance in that case in order to check the replaced node.
            if(params[i]->node_type == AST_Node_Type::function_param_if) {
                Function_Param_If& node = (Function_Param_If&)*params[i];
                anton::Expected<Expr_Value, anton::String> result = evaluate_const_expr(ctx, *node.condition);
                if(!result) {
                    return {anton::expected_error, anton::move(result.error())};
                }

                if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                    Source_Info const& src = node.source_info;
                    return {anton::expected_error,
                            build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                }

                if(result.value().as_boolean()) {
                    if(node.true_param) {
                        params[i] = anton::move(node.true_param);
                    } else {
                        params.erase(params.begin() + i, params.begin() + i + 1);
                    }
                } else {
                    if(node.false_param) {
                        params[i] = anton::move(node.false_param);
                    } else {
                        params.erase(params.begin() + i, params.begin() + i + 1);
                    }
                }
            } else {
                i += 1;
            }
        }

        switch(function.stage) {
            case Stage_Type::vertex: {
                for(auto& param: params) {
                    if(param->node_type == AST_Node_Type::ordinary_function_param) {
                        Source_Info const& src = param->source_info;
                        return {anton::expected_error,
                                build_error_message(src.file_path, src.line, src.column, u8"ordinary parameters are not allowed on vertex stage")};
                    } else if(param->node_type == AST_Node_Type::sourced_function_param) {
                        Sourced_Function_Param& node = (Sourced_Function_Param&)*param;
                        Type& type = *node.type;

                        // Sourced parameters that are arrays must be sized
                        if(is_unsized_array(type)) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"sourced parameters must not be unsized arrays")};
                        }

                        // Sourced parameters must not be opaque
                        if(is_opaque_type(type)) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"sourced parameters must be of non-opaque type (non-opaque builtin type or user "
                                                        u8"defined type) or an array of non-opaque type")};
                        }
                    } else if(param->node_type == AST_Node_Type::vertex_input_param) {
                        Vertex_Input_Param& node = (Vertex_Input_Param&)*param;
                        Type& type = *node.type;

                        // Vertex input parameters must not be arrays (we do not support them yet)
                        if(type.node_type == AST_Node_Type::array_type) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"vertex input parameters must not be arrays")};
                        }

                        // Vertex input parameters must not be opaque
                        if(is_opaque_type(type)) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"vertex input parameters must be of non-opaque type (non-opaque builtin type or user "
                                                        u8"defined type) or an array of non-opaque type")};
                        }
                    } else {
                        Source_Info const& src = param->source_info;
                        return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"unknown parameter type")};
                    }
                }
            } break;

            case Stage_Type::fragment: {
                if(params.size() > 0) {
                    bool const has_ordinary_parameter = params[0]->node_type == AST_Node_Type::ordinary_function_param;
                    if(has_ordinary_parameter) {
                        Ordinary_Function_Param& node = (Ordinary_Function_Param&)*params[0];
                        Type& type = *node.type;

                        // Sourced parameters that are arrays must be sized
                        if(is_unsized_array(type)) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"stage input parameters must not be unsized arrays")};
                        }

                        // Sourced parameters must not be opaque
                        if(is_opaque_type(type)) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"stage input parameters must be of non-opaque type (non-opaque builtin type or user "
                                                        u8"defined type) or an array of non-opaque type")};
                        }
                    }

                    for(i64 i = has_ordinary_parameter; i < params.size(); ++i) {
                        auto& param = params[i];
                        if(param->node_type == AST_Node_Type::sourced_function_param) {
                            Sourced_Function_Param& node = (Sourced_Function_Param&)*param;
                            Type& type = *node.type;

                            // Sourced parameters that are arrays must be sized
                            if(is_unsized_array(type)) {
                                Source_Info const& src = type.source_info;
                                return {anton::expected_error,
                                        build_error_message(src.file_path, src.line, src.column, u8"sourced parameters must not be unsized arrays")};
                            }

                            // Sourced parameters must not be opaque
                            if(is_opaque_type(type)) {
                                Source_Info const& src = type.source_info;
                                return {anton::expected_error,
                                        build_error_message(src.file_path, src.line, src.column,
                                                            u8"sourced parameters must be of non-opaque type (non-opaque builtin type or user "
                                                            u8"defined type) or an array of non-opaque type")};
                            }
                        } else if(param->node_type == AST_Node_Type::ordinary_function_param) {
                            Source_Info const& src = param->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"ordinary parameters are not allowed on fragment stage")};
                        } else if(param->node_type == AST_Node_Type::vertex_input_param) {
                            Source_Info const& src = param->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"vertex input parameters are not allowed on fragment stage")};
                        } else {
                            Source_Info const& src = param->source_info;
                            return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"unknown parameter type")};
                        }
                    }
                }
            } break;

            case Stage_Type::compute: {
                for(auto& param: params) {
                    if(param->node_type == AST_Node_Type::ordinary_function_param) {
                        Source_Info const& src = param->source_info;
                        return {anton::expected_error,
                                build_error_message(src.file_path, src.line, src.column, u8"ordinary parameters are not allowed on compute stage")};
                    } else if(param->node_type == AST_Node_Type::sourced_function_param) {
                        Sourced_Function_Param& node = (Sourced_Function_Param&)*param;
                        Type& type = *node.type;

                        // Sourced parameters that are arrays must be sized
                        if(is_unsized_array(type)) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"sourced parameters must not be unsized arrays")};
                        }

                        // Sourced parameters must not be opaque
                        if(is_opaque_type(type)) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"sourced parameters must be of non-opaque type (non-opaque builtin type or user "
                                                        u8"defined type) or an array of non-opaque type")};
                        }
                    } else if(param->node_type == AST_Node_Type::vertex_input_param) {
                        Source_Info const& src = param->source_info;
                        return {anton::expected_error,
                                build_error_message(src.file_path, src.line, src.column, u8"vertex input parameters are not allowed on compute stage")};
                    } else {
                        Source_Info const& src = param->source_info;
                        return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"unknown parameter type")};
                    }
                }
            } break;
        }

        return {anton::expected_value};
    }

    static anton::Expected<void, anton::String> process_ast(Context& ctx, Owning_Ptr<AST_Node>& ast_node) {
        switch(ast_node->node_type) {
            case AST_Node_Type::variable_declaration: {
                Owning_Ptr<Variable_Declaration>& node = (Owning_Ptr<Variable_Declaration>&)ast_node;
                if(node->initializer) {
                    process_ast(ctx, (Owning_Ptr<AST_Node>&)node->initializer);
                }
            } break;

            case AST_Node_Type::constant_declaration: {
                Owning_Ptr<Constant_Declaration>& node = (Owning_Ptr<Constant_Declaration>&)ast_node;
                if(node->initializer) {
                    process_ast(ctx, (Owning_Ptr<AST_Node>&)node->initializer);
                }
            } break;

            case AST_Node_Type::statement_list: {
                Owning_Ptr<Statement_List>& node = (Owning_Ptr<Statement_List>&)ast_node;
                for(auto& statement: node->statements) {
                    process_ast(ctx, (Owning_Ptr<AST_Node>&)statement);
                }
            } break;

            case AST_Node_Type::block_statement: {
                Owning_Ptr<Block_Statement>& node = (Owning_Ptr<Block_Statement>&)ast_node;
                process_ast(ctx, (Owning_Ptr<AST_Node>&)node->statements);
            } break;

            case AST_Node_Type::if_statement: {
                // TODO: constant evaluation when possible.
                Owning_Ptr<If_Statement>& node = (Owning_Ptr<If_Statement>&)ast_node;
                process_ast(ctx, (Owning_Ptr<AST_Node>&)node->condition);
                process_ast(ctx, (Owning_Ptr<AST_Node>&)node->true_statement);
                if(node->false_statement) {
                    process_ast(ctx, (Owning_Ptr<AST_Node>&)node->false_statement);
                }
            } break;

            case AST_Node_Type::declaration_statement: {
                Owning_Ptr<Declaration_Statement>& node = (Owning_Ptr<Declaration_Statement>&)ast_node;
                process_ast(ctx, (Owning_Ptr<AST_Node>&)node->declaration);
            } break;

            case AST_Node_Type::expression_if: {
                Owning_Ptr<Expression_If>& node = (Owning_Ptr<Expression_If>&)ast_node;
                anton::Expected<Expr_Value, anton::String> result = evaluate_const_expr(ctx, *node->condition);
                if(!result) {
                    return {anton::expected_error, anton::move(result.error())};
                }

                if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                    Source_Info const& src = node->source_info;
                    return {anton::expected_error,
                            build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                }

                if(result.value().as_boolean()) {
                    ast_node = anton::move(node->true_expr);
                } else {
                    if(node->false_expr->node_type == AST_Node_Type::expression_if) {
                        process_ast(ctx, (Owning_Ptr<AST_Node>&)node->false_expr);
                    }

                    ast_node = anton::move(node->false_expr);
                }
            } break;

            default:
                break;
        }

        return {anton::expected_value};
    }

    // build_ast_from_sources
    // Parse sources, process imports and declaration ifs, build top-level symbol table, validate functions, fold constants, extract settings
    //
    static anton::Expected<Owning_Ptr<Declaration_List>, anton::String> build_ast_from_sources(Context& ctx, anton::String const& path,
                                                                                               anton::Array<Pass_Settings>& settings) {
        Owning_Ptr<Declaration_List> ast;
        // Parse the entry source
        {
            anton::Expected<Source_Request_Result, anton::String> source_request_res = ctx.source_request_cb(path, ctx.source_request_user_data);
            if(!source_request_res) {
                // TODO: Error message
                return {anton::expected_error, anton::move(source_request_res.error())};
            }

            Source_Request_Result& request_res = source_request_res.value();
            Input_String_Stream stream{anton::move(request_res.data)};
            Owning_Ptr<anton::String> const& current_source_name =
                ctx.imported_sources.emplace_back(Owning_Ptr{new anton::String{anton::move(request_res.source_name)}});
            anton::Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_result = parse_source(stream, *current_source_name);
            if(parse_result) {
                ast = anton::move(parse_result.value());
            } else {
                Parse_Error const& error = parse_result.error();
                anton::String error_msg = build_error_message(path, error.line, error.column, error.message);
                return {anton::expected_error, anton::move(error_msg)};
            }
        }

        for(i64 i = 0; i < ast->declarations.size();) {
            bool const should_advance =
                ast->declarations[i]->node_type != AST_Node_Type::import_decl && ast->declarations[i]->node_type != AST_Node_Type::declaration_if;
            switch(ast->declarations[i]->node_type) {
                case AST_Node_Type::import_decl: {
                    Owning_Ptr<Import_Decl> node{static_cast<Import_Decl*>(ast->declarations[i].release())};
                    anton::Expected<Source_Request_Result, anton::String> source_request_res = ctx.source_request_cb(node->path, ctx.source_request_user_data);
                    if(!source_request_res) {
                        // TODO: Error message
                        return {anton::expected_error, anton::move(source_request_res.error())};
                    }

                    ast->declarations.erase(ast->declarations.begin() + i, ast->declarations.begin() + i + 1);
                    Source_Request_Result& request_res = source_request_res.value();

                    // Ensure we're not importing the same source multiple times
                    auto iter = anton::find_if(ctx.imported_sources.begin(), ctx.imported_sources.end(),
                                               [&source_name = request_res.source_name](Owning_Ptr<anton::String> const& v) { return *v == source_name; });
                    if(iter == ctx.imported_sources.end()) {
                        Input_String_Stream stream{anton::move(request_res.data)};
                        Owning_Ptr<anton::String> const& current_source_name =
                            ctx.imported_sources.emplace_back(Owning_Ptr{new anton::String{anton::move(request_res.source_name)}});
                        anton::Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_result = parse_source(stream, *current_source_name);
                        if(!parse_result) {
                            Parse_Error const& error = anton::move(parse_result.error());
                            anton::String error_msg = build_error_message(*current_source_name, error.line, error.column, error.message);
                            return {anton::expected_error, anton::move(error_msg)};
                        }

                        // Insert the result of parsing into the ast
                        Declaration_List& decls = *parse_result.value();
                        anton::Move_Iterator begin(decls.declarations.begin());
                        anton::Move_Iterator end(decls.declarations.end());
                        ast->declarations.insert(i, begin, end);
                    }
                } break;

                case AST_Node_Type::declaration_if: {
                    Owning_Ptr<Declaration_If> node{static_cast<Declaration_If*>(ast->declarations[i].release())};
                    anton::Expected<Expr_Value, anton::String> result = evaluate_const_expr(ctx, *node->condition);
                    if(!result) {
                        return {anton::expected_error, anton::move(result.error())};
                    }

                    if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                        Source_Info const& src = node->source_info;
                        return {anton::expected_error,
                                build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                    }

                    ast->declarations.erase(ast->declarations.begin() + i, ast->declarations.begin() + i + 1);
                    // Insert one of the branches into the ast
                    Declaration_List* decls = (result.value().as_boolean() ? node->true_declarations.get() : node->false_declarations.get());
                    if(decls) {
                        anton::Move_Iterator begin(decls->declarations.begin());
                        anton::Move_Iterator end(decls->declarations.end());
                        ast->declarations.insert(i, begin, end);
                    }
                } break;

                case AST_Node_Type::struct_decl: {
                    Struct_Decl& node = static_cast<Struct_Decl&>(*ast->declarations[i]);
                    ctx.symbols[0].emplace(node.name->value, Symbol{Symbol_Type::struct_decl, &node});
                } break;

                case AST_Node_Type::variable_declaration: {
                    Variable_Declaration& node = static_cast<Variable_Declaration&>(*ast->declarations[i]);
                    Source_Info const& src = node.source_info;
                    anton::String error_msg = build_error_message(src.file_path, src.line, src.column, u8"illegal variable declaration in global scope");
                    return {anton::expected_error, anton::move(error_msg)};
                } break;

                case AST_Node_Type::constant_declaration: {
                    Constant_Declaration& node = static_cast<Constant_Declaration&>(*ast->declarations[i]);
                    ctx.symbols[0].emplace(node.identifier->value, Symbol{Symbol_Type::constant, &node});
                } break;

                case AST_Node_Type::function_declaration: {
                    Function_Declaration& fn = static_cast<Function_Declaration&>(*ast->declarations[i]);
                    ctx.symbols[0].emplace(fn.name->value, Symbol{Symbol_Type::function, &fn});
                    if(anton::Expected<void, anton::String> res = process_fn_param_list(ctx, fn); !res) {
                        return {anton::expected_error, anton::move(res.error())};
                    }

                    if(anton::Expected<void, anton::String> res = process_ast(ctx, (Owning_Ptr<AST_Node>&)fn.body); !res) {
                        return {anton::expected_error, anton::move(res.error())};
                    }
                } break;

                case AST_Node_Type::pass_stage_declaration: {
                    Pass_Stage_Declaration& fn = static_cast<Pass_Stage_Declaration&>(*ast->declarations[i]);
                    if(anton::Expected<void, anton::String> res = process_fn_param_list(ctx, fn); !res) {
                        return {anton::expected_error, anton::move(res.error())};
                    }

                    if(anton::Expected<void, anton::String> res = process_ast(ctx, (Owning_Ptr<AST_Node>&)fn.body); !res) {
                        return {anton::expected_error, anton::move(res.error())};
                    }
                } break;

                case AST_Node_Type::settings_decl: {
                    Settings_Decl& decl = static_cast<Settings_Decl&>(*ast->declarations[i]);
                    Pass_Settings* pass_iter = anton::find_if(
                        settings.begin(), settings.end(), [&pass_name = decl.pass_name->value](Pass_Settings const& v) { return v.pass_name == pass_name; });
                    if(pass_iter == settings.end()) {
                        Pass_Settings& v = settings.emplace_back(Pass_Settings{decl.pass_name->value, {}});
                        pass_iter = &v;
                    }

                    anton::Array<Settings_Group>& settings_groups = pass_iter->settings_groups;
                    for(Settings_Group& group: decl.settings_groups) {
                        Settings_Group* group_iter = anton::find_if(settings_groups.begin(), settings_groups.end(),
                                                                    [&group](Settings_Group const& v) { return v.group_name == group.group_name; });
                        if(group_iter == settings_groups.end()) {
                            Settings_Group& g = settings_groups.emplace_back(Settings_Group{group.group_name, {}});
                            group_iter = &g;
                        }

                        // N^2 loop to overwrite duplicates in the order of occurence
                        for(Setting_Key_Value const& kv_new: group.settings) {
                            auto end = group_iter->settings.end();
                            auto i = anton::find_if(group_iter->settings.begin(), end, [&kv_new](Setting_Key_Value const& v) { return kv_new.key == v.key; });
                            if(i != end) {
                                i->value = kv_new.value;
                            } else {
                                group_iter->settings.emplace_back(kv_new);
                            }
                        }
                    }
                } break;

                default:
                    break;
            }

            if(should_advance) {
                i += 1;
            }
        }

        return {anton::expected_value, anton::move(ast)};
    }

    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config, source_request_callback callback, void* user_data) {
        Context ctx = {};
        ctx.source_request_cb = callback;
        ctx.source_request_user_data = user_data;
        // Add global scope
        ctx.symbols.emplace_back();
        // Create symbols for the constant defines passed via config
        anton::Array<Owning_Ptr<Declaration>> constant_decls;
        for(Constant_Define const& define: config.defines) {
            Symbol symbol;
            symbol.type = Symbol_Type::constant;
            Constant_Declaration* decl = new Constant_Declaration(
                Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_int, {config.source_name, 0, 0, 0})},
                Owning_Ptr{new Identifier(anton::String(define.name), {config.source_name, 0, 0, 0})},
                Owning_Ptr{new Integer_Literal(anton::to_string(define.value), {config.source_name, 0, 0, 0})}, {config.source_name, 0, 0, 0});
            constant_decls.emplace_back(decl);
            symbol.declaration = decl;
            ctx.symbols[0].emplace(define.name, symbol);
        }

        anton::Array<Pass_Settings> settings;
        anton::Expected<Owning_Ptr<Declaration_List>, anton::String> parse_res = build_ast_from_sources(ctx, config.source_name, settings);
        if(!parse_res) {
            return {anton::expected_error, anton::move(parse_res.error())};
        }

        Owning_Ptr<Declaration_List> ast = anton::move(parse_res.value());
        anton::Expected<anton::Array<GLSL_File>, anton::String> codegen_res = generate_glsl(ctx, *ast, config.format);
        if(!codegen_res) {
            return {anton::expected_error, anton::move(codegen_res.error())};
        }

        return {anton::expected_value, Build_Result{anton::move(settings), anton::move(codegen_res.value())}};
    }

    static anton::Expected<anton::String, anton::String> resolve_import_path(anton::String const& import_path,
                                                                             anton::Slice<anton::String const> const import_directories) {
        bool found = false;
        anton::String out_path;
        for(anton::String const& path: import_directories) {
            anton::String resolved_path = anton::fs::concat_paths(path, import_path);
            bool exists = anton::fs::exists(resolved_path);
            if(exists) {
                if(!found) {
                    found = true;
                    out_path = anton::move(resolved_path);
                } else {
                    return {anton::expected_error, anton::String{u8"ambiguous import path"}};
                }
            }
        }

        if(found) {
            return {anton::expected_value, anton::move(out_path)};
        } else {
            return {anton::expected_error, anton::String{u8"could not resolve import path"}};
        }
    }

    static anton::Expected<Source_Request_Result, anton::String> file_read_callback(anton::String const& path, void* user_data) {
        anton::Slice<anton::String const>& import_directories = *reinterpret_cast<anton::Slice<anton::String const>*>(user_data);
        anton::Expected<anton::String, anton::String> res = resolve_import_path(path, import_directories);
        if(!res) {
            return {anton::expected_error, anton::move(res.error())};
        }

        anton::fs::Input_File_Stream file;
        if(!file.open(res.value())) {
            return {anton::expected_error, u8"could not open " + res.value() + u8" for reading"};
        }

        file.seek(anton::Seek_Dir::end, 0);
        i64 size = file.tell();
        file.seek(anton::Seek_Dir::beg, 0);
        anton::String file_contents{anton::reserve, size};
        file_contents.force_size(size);
        file.read(file_contents.data(), size);
        return {anton::expected_value, Source_Request_Result{anton::move(res.value()), anton::move(file_contents)}};
    }

    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config) {
        anton::Slice<anton::String const> import_directories{config.import_directories};
        return compile_to_glsl(config, file_read_callback, &import_directories);
    }
} // namespace vush
