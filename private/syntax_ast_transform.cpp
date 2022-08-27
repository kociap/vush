#include <ast2.hpp>

#include <anton/expected.hpp>
#include <anton/intrinsics.hpp>
#include <anton/optional.hpp>

#include <context.hpp>
#include <diagnostics.hpp>
#include <memory.hpp>
#include <parser.hpp>
#include <syntax_accessors.hpp>
#include <vush/vush.hpp>

namespace vush {
    using namespace anton::literals;

    anton::Expected<ast::Decl_List, Error> import_source_code(Context& ctx, anton::String_View const source_name,
                                                              anton::Optional<Source_Info> const source_info) {
        anton::Expected<Source_Request_Result, anton::String> source_request_res = ctx.source_request_cb(source_name, ctx.source_request_user_data);
        if(!source_request_res) {
            if(source_info) {
                return {anton::expected_error, format_import_source_failed(ctx, *source_info, source_request_res.error())};
            } else {
                return {anton::expected_error, format_import_source_failed_no_location(ctx, source_request_res.error())};
            }
        }

        Source_Request_Result& request_res = source_request_res.value();
        // Ensure we're not importing the same source multiple times.
        auto iter = ctx.source_registry.find(request_res.source_name);
        if(iter == ctx.source_registry.end()) {
            Source_Data source{ANTON_MOV(request_res.source_name), ANTON_MOV(request_res.data)};
            Parse_Syntax_Options options{.include_whitespace_and_comments = false};
            anton::Expected<Array<SNOT>, Error> parse_result = parse_source_to_syntax_tree(ctx.allocator, source.name, source.data, options);
            if(!parse_result) {
                return {anton::expected_error, ANTON_MOV(parse_result.error())};
            }

            anton::Expected<ast::Decl_List, Error> transform_result = transform_syntax_tree_to_ast(ctx, parse_result.value());
            if(!transform_result) {
                return {anton::expected_error, ANTON_MOV(transform_result.error())};
            }

            ctx.source_registry.emplace(source.name, ANTON_MOV(source));
            return {anton::expected_value, ANTON_MOV(transform_result.value())};
        } else {
            return {anton::expected_value, ast::Decl_List{}};
        }
    }

    // resolve_imports_and_declaration_ifs
    // Processes top-level ast to resolve import declarations and declaration ifs. Modifies ast.
    //
    // static anton::Expected<void, anton::String> resolve_imports_and_declaration_ifs(Context& ctx, Declaration_List& ast) {
    //     // TODO: Currently constants cannot be used in declaration ifs because they are not added to the symbol table as
    //     //       we process the ast. Should we allow constants in declaration ifs?
    //     for(i64 i = 0; i < ast.size();) {
    //         bool const should_advance = ast[i]->node_type != AST_Node_Type::import_declaration && ast[i]->node_type != AST_Node_Type::declaration_if;
    //         if(ast[i]->node_type == AST_Node_Type::import_declaration) {
    //             Owning_Ptr<Import_Declaration> node{downcast, ANTON_MOV(ast[i])};
    //             ast.erase(ast.begin() + i, ast.begin() + i + 1);
    //             anton::Expected<Declaration_List, Error> import_result = import_source_code(ctx, node->path->value, node->source_info);
    //             if(!import_result) {
    //                 return {anton::expected_error, import_result.error().format(ctx.allocator, ctx.diagnostics.extended)};
    //             }

    //             // Insert the result of parsing into the ast.
    //             Declaration_List& decls = import_result.value();
    //             anton::Move_Iterator begin(decls.begin());
    //             anton::Move_Iterator end(decls.end());
    //             ast.insert(i, begin, end);
    //         } else if(ast[i]->node_type == AST_Node_Type::declaration_if) {
    //             Owning_Ptr<Declaration_If> node{downcast, ANTON_MOV(ast[i])};
    //             ast.erase(ast.begin() + i, ast.begin() + i + 1);

    //             anton::Expected<Expr_Value, anton::String> result = evaluate_const_expr(ctx, *node->condition);
    //             if(!result) {
    //                 return {anton::expected_error, ANTON_MOV(result.error())};
    //             }

    //             if(!is_implicitly_convertible_to_boolean(result.value().type)) {
    //                 Source_Info const& src = node->condition->source_info;
    //                 return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
    //             }

    //             // Insert one of the branches into the ast
    //             Declaration_List& decls = (result.value().as_boolean() ? node->true_declarations : node->false_declarations);
    //             anton::Move_Iterator begin(decls.begin());
    //             anton::Move_Iterator end(decls.end());
    //             ast.insert(i, begin, end);
    //         }

    //         i += should_advance;
    //     }

    //     return {anton::expected_value};
    // }

    [[nodiscard]] static anton::Slice<SNOT const> get_attribute_lists(Syntax_Node const& node) {
        auto begin = node.children.begin();
        while(begin->is_right()) {
            ++begin;
        }
        auto end = begin;
        while(end->is_left() && end->left().type == Syntax_Node_Kind::attribute_list) {
            ++end;
        }
        return {begin, end};
    }

    [[nodiscard]] static i64 get_past_attribute_lists_offset(Syntax_Node const& node) {
        auto iterator = node.children.begin();
        while(iterator->is_right()) {
            ++iterator;
        }
        while(iterator->is_left() && iterator->left().type == Syntax_Node_Kind::attribute_list) {
            ++iterator;
        }
        return iterator - node.children.begin();
    }

    [[nodiscard]] static ast::Identifier const* transform_identifier(Context const& ctx, Syntax_Token const& token) {
        anton::String const* const value = allocate<anton::String>(ctx.allocator, token.value, ctx.allocator);
        return allocate<ast::Identifier>(ctx.allocator, *value, token.source_info);
    }

    [[nodiscard]] static anton::Expected<ast::Lt_Integer const*, Error> transform_lt_integer(Context const& ctx, Syntax_Node const& node) {
        Syntax_Token const& value_token = get_expr_lt_integer_value(node);
        ast::Lt_Integer_Base base;
        switch(value_token.type) {
            case Syntax_Node_Kind::lt_bin_integer:
                base = ast::Lt_Integer_Base::bin;
                break;
            case Syntax_Node_Kind::lt_dec_integer:
                base = ast::Lt_Integer_Base::dec;
                break;
            case Syntax_Node_Kind::lt_hex_integer:
                base = ast::Lt_Integer_Base::hex;
                break;

            default:
                ANTON_ASSERT(false, ""); // TODO: Error
                ANTON_UNREACHABLE();
        }

        // The default integer literal type is i32.
        ast::Lt_Integer_Kind kind = ast::Lt_Integer_Kind::i32;
        if(anton::Optional<Syntax_Token const&> suffix_token = get_expr_lt_integer_suffix(node)) {
            anton::String_View const suffix = suffix_token->value;
            if(suffix == "u"_sv || suffix == "U"_sv) {
                kind = ast::Lt_Integer_Kind::u32;
            } else {
                return {anton::expected_error, err_invalid_integer_suffix(ctx, suffix_token->source_info)};
            }
        }

        anton::String const* const value = allocate<anton::String>(ctx.allocator, value_token.value, ctx.allocator);
        return {anton::expected_value, allocate<ast::Lt_Integer>(ctx.allocator, *value, kind, base, node.source_info)};
    }

    [[nodiscard]] static anton::Expected<ast::Type const*, Error> transform_type(Context const& ctx, Syntax_Node const& node) {
        if(node.type == Syntax_Node_Kind::type_builtin) {
            Syntax_Token const& value_token = get_type_builtin_value(node);
            anton::Optional<ast::GLSL_Type> result = ast::enumify_glsl_type(value_token.value);
            ANTON_ASSERT(result, "invalid builtin type");
            return {anton::expected_value, allocate<ast::Type_Builtin>(ctx.allocator, result.value(), node.source_info)};
        } else if(node.type == Syntax_Node_Kind::type_user_defined) {
            Syntax_Token const& value_token = get_type_user_defined_value(node);
            return {anton::expected_value, allocate<ast::Type_User_Defined>(ctx.allocator, value_token.value, node.source_info)};
        } else if(node.type == Syntax_Node_Kind::type_array) {
            Syntax_Node const& base_node = get_type_array_base(node);
            anton::Expected<ast::Type const*, Error> base_result = transform_type(ctx, base_node);
            if(!base_result) {
                return {anton::expected_error, ANTON_MOV(base_result.error())};
            }

            Syntax_Node const& size_node = get_type_array_size(node);
            anton::Expected<ast::Lt_Integer const*, Error> size_result = transform_lt_integer(ctx, size_node);
            if(!size_result) {
                return {anton::expected_error, ANTON_MOV(size_result.error())};
            }

            return {anton::expected_value, allocate<ast::Type_Array>(ctx.allocator, base_result.value(), size_result.value(), node.source_info)};
        } else {
            ANTON_ASSERT(false, "invalid type");
            ANTON_UNREACHABLE();
        }
    }

    [[nodiscard]] static anton::Expected<ast::Expr const*, Error> transform_expr(Context const& ctx, Syntax_Node const& node) {
        switch(node.type) {
            case Syntax_Node_Kind::expr_if: {
                Syntax_Node const& condition_node = get_expr_if_condition(node);
                anton::Expected<ast::Expr const*, Error> condition = transform_expr(ctx, condition_node);
                if(!condition) {
                    return {anton::expected_error, ANTON_MOV(condition.error())};
                }

                Syntax_Node const& then_expr_node = get_expr_block_expression(get_expr_if_then_branch(node));
                anton::Expected<ast::Expr const*, Error> then_branch = transform_expr(ctx, then_expr_node);
                if(!then_branch) {
                    return {anton::expected_error, ANTON_MOV(then_branch.error())};
                }

                Syntax_Node const& else_expr_node = get_expr_block_expression(get_expr_if_else_branch(node));
                anton::Expected<ast::Expr const*, Error> else_branch = transform_expr(ctx, else_expr_node);
                if(!else_branch) {
                    return {anton::expected_error, ANTON_MOV(else_branch.error())};
                }

                return {anton::expected_value,
                        allocate<ast::Expr_If>(ctx.allocator, condition.value(), then_branch.value(), else_branch.value(), node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_identifier: {
                Syntax_Token const& value_token = get_expr_identifier_value(node);
                anton::String const* const value = allocate<anton::String>(ctx.allocator, value_token.value, ctx.allocator);
                return {anton::expected_value, allocate<ast::Expr_Identifier>(ctx.allocator, *value, node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_binary: {
                auto translate_kind = [](Syntax_Node_Kind const kind) -> ast::Expr_Binary_Kind {
                    switch(kind) {
                        case Syntax_Node_Kind::tk_langle:
                            return ast::Expr_Binary_Kind::lt;
                        case Syntax_Node_Kind::tk_rangle:
                            return ast::Expr_Binary_Kind::gt;
                        case Syntax_Node_Kind::tk_plus:
                            return ast::Expr_Binary_Kind::add;
                        case Syntax_Node_Kind::tk_minus:
                            return ast::Expr_Binary_Kind::sub;
                        case Syntax_Node_Kind::tk_asterisk:
                            return ast::Expr_Binary_Kind::mul;
                        case Syntax_Node_Kind::tk_slash:
                            return ast::Expr_Binary_Kind::div;
                        case Syntax_Node_Kind::tk_percent:
                            return ast::Expr_Binary_Kind::mod;
                        case Syntax_Node_Kind::tk_amp:
                            return ast::Expr_Binary_Kind::bit_and;
                        case Syntax_Node_Kind::tk_pipe:
                            return ast::Expr_Binary_Kind::bit_or;
                        case Syntax_Node_Kind::tk_hat:
                            return ast::Expr_Binary_Kind::bit_xor;
                        case Syntax_Node_Kind::tk_equals:
                            return ast::Expr_Binary_Kind::assign;
                        case Syntax_Node_Kind::tk_amp2:
                            return ast::Expr_Binary_Kind::logic_and;
                        case Syntax_Node_Kind::tk_pipe2:
                            return ast::Expr_Binary_Kind::logic_or;
                        case Syntax_Node_Kind::tk_hat2:
                            return ast::Expr_Binary_Kind::logic_xor;
                        case Syntax_Node_Kind::tk_shl:
                            return ast::Expr_Binary_Kind::shl;
                        case Syntax_Node_Kind::tk_shr:
                            return ast::Expr_Binary_Kind::shr;
                        case Syntax_Node_Kind::tk_eq2:
                            return ast::Expr_Binary_Kind::eq;
                        case Syntax_Node_Kind::tk_neq:
                            return ast::Expr_Binary_Kind::neq;
                        case Syntax_Node_Kind::tk_lteq:
                            return ast::Expr_Binary_Kind::lteq;
                        case Syntax_Node_Kind::tk_gteq:
                            return ast::Expr_Binary_Kind::gteq;
                        case Syntax_Node_Kind::tk_pluseq:
                            return ast::Expr_Binary_Kind::assign_add;
                        case Syntax_Node_Kind::tk_minuseq:
                            return ast::Expr_Binary_Kind::assign_sub;
                        case Syntax_Node_Kind::tk_asteriskeq:
                            return ast::Expr_Binary_Kind::assign_mul;
                        case Syntax_Node_Kind::tk_slasheq:
                            return ast::Expr_Binary_Kind::assign_div;
                        case Syntax_Node_Kind::tk_percenteq:
                            return ast::Expr_Binary_Kind::assign_mod;
                        case Syntax_Node_Kind::tk_ampeq:
                            return ast::Expr_Binary_Kind::assign_bit_and;
                        case Syntax_Node_Kind::tk_pipeeq:
                            return ast::Expr_Binary_Kind::assign_bit_or;
                        case Syntax_Node_Kind::tk_hateq:
                            return ast::Expr_Binary_Kind::assign_bit_xor;
                        case Syntax_Node_Kind::tk_shleq:
                            return ast::Expr_Binary_Kind::assign_shl;
                        case Syntax_Node_Kind::tk_shreq:
                            return ast::Expr_Binary_Kind::assign_shr;
                        default:
                            ANTON_ASSERT(false, ""); // TODO: ERROR
                            ANTON_UNREACHABLE();
                    }
                };

                Syntax_Token const& operator_token = get_expr_binary_operator(node);
                ast::Expr_Binary_Kind const kind = translate_kind(operator_token.type);

                Syntax_Node const& lhs_node = get_expr_binary_lhs(node);
                anton::Expected<ast::Expr const*, Error> lhs = transform_expr(ctx, lhs_node);
                if(!lhs) {
                    return ANTON_MOV(lhs);
                }

                Syntax_Node const& rhs_node = get_expr_binary_rhs(node);
                anton::Expected<ast::Expr const*, Error> rhs = transform_expr(ctx, rhs_node);
                if(!rhs) {
                    return ANTON_MOV(rhs);
                }

                return {anton::expected_value,
                        allocate<ast::Expr_Binary>(ctx.allocator, lhs.value(), rhs.value(),
                                                   ast::With_Source<ast::Expr_Binary_Kind>{kind, operator_token.source_info}, node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_prefix: {
                auto translate_kind = [](Syntax_Node_Kind const kind) -> ast::Expr_Prefix_Kind {
                    switch(kind) {
                        case Syntax_Node_Kind::tk_plus:
                            return ast::Expr_Prefix_Kind::plus;
                        case Syntax_Node_Kind::tk_minus:
                            return ast::Expr_Prefix_Kind::minus;
                        case Syntax_Node_Kind::tk_plus2:
                            return ast::Expr_Prefix_Kind::inc;
                        case Syntax_Node_Kind::tk_minus2:
                            return ast::Expr_Prefix_Kind::dec;
                        case Syntax_Node_Kind::tk_bang:
                            return ast::Expr_Prefix_Kind::logic_not;
                        case Syntax_Node_Kind::tk_tilde:
                            return ast::Expr_Prefix_Kind::bit_not;
                        default:
                            ANTON_ASSERT(false, ""); // TODO: ERROR
                            ANTON_UNREACHABLE();
                    }
                };

                Syntax_Token const& operator_token = get_expr_prefix_operator(node);
                ast::Expr_Prefix_Kind const kind = translate_kind(operator_token.type);

                Syntax_Node const& expression_node = get_expr_prefix_expression(node);
                anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                return {anton::expected_value,
                        allocate<ast::Expr_Prefix>(ctx.allocator, expression.value(), ast::With_Source<ast::Expr_Prefix_Kind>{kind, operator_token.source_info},
                                                   node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_postfix: {
                auto translate_kind = [](Syntax_Node_Kind const kind) -> ast::Expr_Postfix_Kind {
                    switch(kind) {
                        case Syntax_Node_Kind::tk_plus2:
                            return ast::Expr_Postfix_Kind::inc;
                        case Syntax_Node_Kind::tk_minus2:
                            return ast::Expr_Postfix_Kind::dec;
                        default:
                            ANTON_ASSERT(false, ""); // TODO: ERROR
                            ANTON_UNREACHABLE();
                    }
                };

                Syntax_Token const& operator_token = get_expr_postfix_operator(node);
                ast::Expr_Postfix_Kind const kind = translate_kind(operator_token.type);

                Syntax_Node const& expression_node = get_expr_postfix_expression(node);
                anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                return {anton::expected_value,
                        allocate<ast::Expr_Postfix>(ctx.allocator, expression.value(),
                                                    ast::With_Source<ast::Expr_Postfix_Kind>{kind, operator_token.source_info}, node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_member_access: {
                Syntax_Node const& expression_node = get_expr_member_access_expression(node);
                anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                Syntax_Token const& identifier_token = get_expr_member_access_identifier(node);
                ast::Identifier const* identifier = transform_identifier(ctx, identifier_token);
                return {anton::expected_value, allocate<ast::Expr_Member_Access>(ctx.allocator, expression.value(), identifier, node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_array_access: {
                Syntax_Node const& expression_node = get_expr_array_access_expression(node);
                anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                Syntax_Node const& index_node = get_expr_array_access_index(node);
                anton::Expected<ast::Expr const*, Error> index = transform_expr(ctx, index_node);
                if(!index) {
                    return ANTON_MOV(index);
                }

                return {anton::expected_value, allocate<ast::Expr_Array_Access>(ctx.allocator, expression.value(), index.value(), node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_parentheses: {
                Syntax_Node const& expression_node = get_expr_parentheses_expression(node);
                anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                return {anton::expected_value, allocate<ast::Expr_Parentheses>(ctx.allocator, expression.value(), node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_reinterpret: {
                ANTON_UNREACHABLE();
            } break;

            case Syntax_Node_Kind::expr_call: {
                Syntax_Token const& identifier_token = get_expr_call_identifier(node);
                ast::Identifier const* const identifier = transform_identifier(ctx, identifier_token);

                Syntax_Node const& arguments_node = get_expr_call_arguments(node);
                Array<ast::Expr const*>& arguments = *allocate<Array<ast::Expr const*>>(ctx.allocator, ctx.allocator);
                for(SNOT const& snot: arguments_node.children) {
                    if(snot.is_left()) {
                        anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, snot.left());
                        if(expression) {
                            arguments.push_back(expression.value());
                        } else {
                            return {anton::expected_error, ANTON_MOV(expression.error())};
                        }
                    }
                }

                return {anton::expected_value, allocate<ast::Expr_Call>(ctx.allocator, identifier, arguments, node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_lt_bool: {
                Syntax_Token const& value_token = get_expr_lt_bool_value(node);
                bool const value = value_token.value == "true"_sv;
                return {anton::expected_value, allocate<ast::Lt_Bool>(ctx.allocator, value, node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_lt_string: {
                // There are no string literals in the language besides the ones
                // used by decl_import which we have special handling for in place,
                // therefore we leave this unimplemented.
                ANTON_ASSERT(false, "unimplemented");
                ANTON_UNREACHABLE();
            } break;

            case Syntax_Node_Kind::expr_lt_float: {
                Syntax_Token const& value_token = get_expr_lt_float_value(node);
                // The default float literal type is f32.
                ast::Lt_Float_Kind kind = ast::Lt_Float_Kind::f32;
                if(anton::Optional<Syntax_Token const&> suffix_token = get_expr_lt_float_suffix(node)) {
                    anton::String_View const suffix = suffix_token->value;
                    if(suffix == "d"_sv || suffix == "D"_sv) {
                        kind = ast::Lt_Float_Kind::f64;
                    } else {
                        return {anton::expected_error, err_invalid_float_suffix(ctx, suffix_token->source_info)};
                    }
                }

                anton::String const* const value = allocate<anton::String>(ctx.allocator, value_token.value, ctx.allocator);
                return {anton::expected_value, allocate<ast::Lt_Float>(ctx.allocator, *value, kind, node.source_info)};
            } break;

            case Syntax_Node_Kind::expr_lt_integer: {
                anton::Expected<ast::Lt_Integer const*, Error> result = transform_lt_integer(ctx, node);
                if(result) {
                    return {anton::expected_value, result.value()};
                } else {
                    return {anton::expected_error, result.error()};
                }
            } break;

            case Syntax_Node_Kind::expr_default: {
                return {anton::expected_value, allocate<ast::Expr_Default>(ctx.allocator, node.source_info)};
            } break;

            default:
                ANTON_UNREACHABLE();
        }
    }

    // transform_stmt
    //
    // Returns:
    // nullptr if the statement does not have a representation in the AST.
    //
    [[nodiscard]] static anton::Expected<ast::Stmt const*, Error> transform_stmt(Context const& ctx, Syntax_Node const& node);
    [[nodiscard]] static anton::Expected<ast::Stmt_List, Error> transform_stmt_block_child_stmts(Context const& ctx, Syntax_Node const& node);

    anton::Expected<ast::Stmt_List, Error> transform_stmt_block_child_stmts(Context const& ctx, Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Kind::stmt_block, "Syntax_Node is not stmt_block");
        Array<ast::Stmt const*>& statements = *allocate<Array<ast::Stmt const*>>(ctx.allocator, ctx.allocator);
        for(SNOT const& snot: node.children) {
            if(!snot.is_left()) {
                continue;
            }

            Syntax_Node const& stmt_node = snot.left();
            anton::Expected<ast::Stmt const*, Error> stmt = transform_stmt(ctx, stmt_node);
            if(stmt) {
                if(stmt.value() != nullptr) {
                    statements.push_back(stmt.value());
                }
            } else {
                return {anton::expected_error, ANTON_MOV(stmt.error())};
            }
        }
        return {anton::expected_value, statements};
    }

    [[nodiscard]] static anton::Expected<ast::Stmt_Variable const*, Error> transform_stmt_variable(Context const& ctx, Syntax_Node const& node) {
        ast::Identifier const* const identifier = transform_identifier(ctx, get_stmt_variable_identifier(node));
        anton::Expected<ast::Type const*, Error> type = transform_type(ctx, get_stmt_variable_type(node));
        if(!type) {
            return {anton::expected_error, ANTON_MOV(type.error())};
        }

        ast::Expr const* initializer = nullptr;
        if(anton::Optional initializer_node = get_stmt_variable_initializer(node)) {
            anton::Expected<ast::Expr const*, Error> result = transform_expr(ctx, initializer_node.value());
            if(result) {
                initializer = result.value();
            } else {
                return {anton::expected_error, ANTON_MOV(result.error())};
            }
        }
        return {anton::expected_value, allocate<ast::Stmt_Variable>(ctx.allocator, type.value(), identifier, initializer, node.source_info)};
    }

    anton::Expected<ast::Stmt const*, Error> transform_stmt(Context const& ctx, Syntax_Node const& node) {
        switch(node.type) {
            case Syntax_Node_Kind::stmt_block: {
                anton::Expected<ast::Stmt_List, Error> result = transform_stmt_block_child_stmts(ctx, node);
                if(!result) {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }

                return {anton::expected_value, allocate<ast::Stmt_Block>(ctx.allocator, result.value(), node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_if: {
                anton::Expected<ast::Expr const*, Error> condition = transform_expr(ctx, get_stmt_if_condition(node));
                if(!condition) {
                    return {anton::expected_error, ANTON_MOV(condition.error())};
                }

                anton::Expected<ast::Stmt_List, Error> then_branch = transform_stmt_block_child_stmts(ctx, get_stmt_if_then_branch(node));
                if(!then_branch) {
                    return {anton::expected_error, ANTON_MOV(then_branch.error())};
                }

                ast::Stmt_List else_branch;
                if(anton::Optional<Syntax_Node const&> else_node = get_stmt_if_else_branch(node)) {
                    anton::Expected<ast::Stmt_List, Error> result = transform_stmt_block_child_stmts(ctx, else_node.value());
                    if(result) {
                        else_branch = result.value();
                    } else {
                        return {anton::expected_error, ANTON_MOV(result.error())};
                    }
                }

                return {anton::expected_value, allocate<ast::Stmt_If>(ctx.allocator, condition.value(), then_branch.value(), else_branch, node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_switch: {
                anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, get_stmt_switch_expression(node));
                if(!expression) {
                    return {anton::expected_error, ANTON_MOV(expression.error())};
                }

                Array<ast::Switch_Arm const*>* const arm_list = allocate<Array<ast::Switch_Arm const*>>(ctx.allocator, ctx.allocator);
                {
                    Syntax_Node const& arm_list_node = get_stmt_switch_arm_list(node);
                    for(SNOT const& snot: arm_list_node.children) {
                        if(!snot.is_left()) {
                            continue;
                        }

                        Syntax_Node const& arm_node = snot.left();
                        Array<ast::Expr const*>* const labels = allocate<Array<ast::Expr const*>>(ctx.allocator, ctx.allocator);
                        for(SNOT const& snot: arm_node.children) {
                            if(snot.is_left() && snot.left().type == Syntax_Node_Kind::switch_arm_label) {
                                Syntax_Node const& expression_node = snot.left().children[0].left();
                                anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
                                if(expression) {
                                    labels->push_back(expression.value());
                                } else {
                                    return {anton::expected_error, ANTON_MOV(expression.error())};
                                }
                            }
                        }

                        anton::Expected<ast::Stmt_List, Error> statements = transform_stmt_block_child_stmts(ctx, get_switch_arm_body(arm_node));
                        if(!statements) {
                            return {anton::expected_error, ANTON_MOV(statements.error())};
                        }

                        ast::Switch_Arm const* const arm = allocate<ast::Switch_Arm>(ctx.allocator, *labels, statements.value(), node.source_info);
                        arm_list->push_back(arm);
                    }
                }

                return {anton::expected_value, allocate<ast::Stmt_Switch>(ctx.allocator, expression.value(), *arm_list, node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_for: {
                // We transform the for loop into a block statement that contains
                // the definition of the variable and the loop itself in order to limit scope.
                Array<ast::Stmt const*>* const statements = allocate<Array<ast::Stmt const*>>(ctx.allocator, ctx.allocator);
                if(anton::Optional variable_node = get_stmt_for_variable(node)) {
                    anton::Expected<ast::Stmt_Variable const*, Error> variable = transform_stmt_variable(ctx, variable_node.value());
                    if(variable) {
                        statements->push_back(variable.value());
                    } else {
                        return {anton::expected_error, ANTON_MOV(variable.error())};
                    }
                }

                {
                    ast::Expr const* condition = nullptr;
                    if(anton::Optional condition_node = get_stmt_for_condition(node)) {
                        anton::Expected<ast::Expr const*, Error> result = transform_expr(ctx, condition_node.value());
                        if(result) {
                            condition = result.value();
                        } else {
                            return {anton::expected_error, ANTON_MOV(result.error())};
                        }
                    }

                    ast::Stmt_List continuation;
                    if(anton::Optional expression_node = get_stmt_for_expression(node)) {
                        anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node.value());
                        if(!expression) {
                            return {anton::expected_error, ANTON_MOV(expression.error())};
                        }

                        Array<ast::Stmt const*>* const continuation_statements = allocate<Array<ast::Stmt const*>>(ctx.allocator, ctx.allocator);
                        ast::Stmt_Expression const* const stmt =
                            allocate<ast::Stmt_Expression>(ctx.allocator, expression.value(), expression.value()->source_info);
                        continuation_statements->push_back(stmt);
                        continuation = *continuation_statements;
                    }

                    anton::Expected<ast::Stmt_List, Error> body = transform_stmt_block_child_stmts(ctx, get_stmt_for_body(node));
                    if(!body) {
                        return {anton::expected_error, ANTON_MOV(body.error())};
                    }

                    ast::Stmt_Loop const* const loop = allocate<ast::Stmt_Loop>(ctx.allocator, condition, continuation, body.value(), node.source_info);
                    statements->push_back(loop);
                }

                return {anton::expected_value, allocate<ast::Stmt_Block>(ctx.allocator, *statements, Source_Info{})};
            } break;

            case Syntax_Node_Kind::stmt_while: {
                anton::Expected<ast::Expr const*, Error> condition = transform_expr(ctx, get_stmt_while_condition(node));
                if(!condition) {
                    return {anton::expected_error, ANTON_MOV(condition.error())};
                }

                anton::Expected<ast::Stmt_List, Error> statements = transform_stmt_block_child_stmts(ctx, get_stmt_while_statements(node));
                if(!statements) {
                    return {anton::expected_error, ANTON_MOV(statements.error())};
                }

                return {anton::expected_value,
                        allocate<ast::Stmt_Loop>(ctx.allocator, condition.value(), ast::Stmt_List{}, statements.value(), node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_do_while: {
                anton::Expected<ast::Stmt_List, Error> statements = transform_stmt_block_child_stmts(ctx, get_stmt_do_while_body(node));
                if(!statements) {
                    return {anton::expected_error, ANTON_MOV(statements.error())};
                }

                // We want to place the condition in the continuation block,
                // therefore we transform it into 'if condition { } else { break }'.
                Array<ast::Stmt const*>* const continuation = allocate<Array<ast::Stmt const*>>(ctx.allocator, ctx.allocator);
                {
                    anton::Expected<ast::Expr const*, Error> condition = transform_expr(ctx, get_stmt_do_while_condition(node));
                    if(!condition) {
                        return {anton::expected_error, ANTON_MOV(condition.error())};
                    }

                    Array<ast::Stmt const*>* const else_block = allocate<Array<ast::Stmt const*>>(ctx.allocator, ctx.allocator);
                    else_block->push_back(allocate<ast::Stmt_Break>(ctx.allocator, Source_Info{}));
                    ast::Stmt_If const* const stmt_if = allocate<ast::Stmt_If>(ctx.allocator, condition.value(), ast::Stmt_List{}, *else_block, Source_Info{});
                    continuation->push_back(stmt_if);
                }

                return {anton::expected_value, allocate<ast::Stmt_Loop>(ctx.allocator, nullptr, *continuation, statements.value(), node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_return: {
                ast::Expr const* expression = nullptr;
                if(anton::Optional expression_node = get_stmt_return_expression(node)) {
                    anton::Expected<ast::Expr const*, Error> result = transform_expr(ctx, expression_node.value());
                    if(result) {
                        expression = result.value();
                    } else {
                        return {anton::expected_error, ANTON_MOV(result.error())};
                    }
                }
                return {anton::expected_value, allocate<ast::Stmt_Return>(ctx.allocator, expression, node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_break: {
                return {anton::expected_value, allocate<ast::Stmt_Break>(ctx.allocator, node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_continue: {
                return {anton::expected_value, allocate<ast::Stmt_Continue>(ctx.allocator, node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_discard: {
                return {anton::expected_value, allocate<ast::Stmt_Discard>(ctx.allocator, node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_variable: {
                anton::Expected<ast::Stmt_Variable const*, Error> variable = transform_stmt_variable(ctx, node);
                if(variable) {
                    return {anton::expected_value, variable.value()};
                } else {
                    return {anton::expected_error, ANTON_MOV(variable.error())};
                }
            } break;

            case Syntax_Node_Kind::stmt_expression: {
                anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, get_stmt_expression_expression(node));
                if(!expression) {
                    return {anton::expected_error, ANTON_MOV(expression.error())};
                }

                return {anton::expected_value, allocate<ast::Stmt_Expression>(ctx.allocator, expression.value(), node.source_info)};
            } break;

            case Syntax_Node_Kind::stmt_empty: {
                return {anton::expected_value, nullptr};
            } break;

            default:
                ANTON_UNREACHABLE();
        }
    }

    [[nodiscard]] static anton::Expected<ast::Attr_List, Error> transform_attribute_list(Context const& ctx, anton::Slice<SNOT const> nodes) {
        if(nodes.size() == 0) {
            return {anton::expected_value, anton::Slice<ast::Attr const*>()};
        }

        Array<ast::Attr const*>& attributes = *allocate<Array<ast::Attr const*>>(ctx.allocator, ctx.allocator);
        for(SNOT const& attribute_list: nodes) {
            ANTON_ASSERT(attribute_list.is_left(), "Syntax_Token is not an attribute_list");
            ANTON_ASSERT(attribute_list.left().type == Syntax_Node_Kind::attribute_list, "Syntax_Node is not an attribute_list");
            for(SNOT const& node: attribute_list.left().children) {
                if(!node.is_left()) {
                    continue;
                }

                Syntax_Node const& attribute = node.left();
                switch(attribute.type) {
                    case Syntax_Node_Kind::attr_workgroup: {
                        Syntax_Node const& x_node = get_attr_workgroup_x(attribute);
                        anton::Expected<ast::Lt_Integer const*, Error> x_result = transform_lt_integer(ctx, x_node);
                        if(!x_result) {
                            return {anton::expected_error, ANTON_MOV(x_result.error())};
                        }
                        ast::Lt_Integer const* const x = x_result.value();

                        ast::Lt_Integer const* y = nullptr;
                        if(anton::Optional<Syntax_Node const&> y_node = get_attr_workgroup_y(attribute)) {
                            anton::Expected<ast::Lt_Integer const*, Error> result = transform_lt_integer(ctx, y_node.value());
                            if(result) {
                                y = result.value();
                            } else {
                                return {anton::expected_error, ANTON_MOV(result.error())};
                            }
                        }

                        ast::Lt_Integer const* z = nullptr;
                        if(anton::Optional<Syntax_Node const&> z_node = get_attr_workgroup_z(attribute)) {
                            anton::Expected<ast::Lt_Integer const*, Error> result = transform_lt_integer(ctx, z_node.value());
                            if(result) {
                                z = result.value();
                            } else {
                                return {anton::expected_error, ANTON_MOV(result.error())};
                            }
                        }

                        attributes.push_back(allocate<ast::Attr_Workgroup>(ctx.allocator, x, y, z, attribute.source_info));
                    } break;

                    case Syntax_Node_Kind::attr_flat: {
                        attributes.push_back(allocate<ast::Attr_Flat>(ctx.allocator, attribute.source_info));
                    } break;

                    case Syntax_Node_Kind::attr_smooth: {
                        attributes.push_back(allocate<ast::Attr_Smooth>(ctx.allocator, attribute.source_info));
                    } break;

                    case Syntax_Node_Kind::attr_noperspective: {
                        attributes.push_back(allocate<ast::Attr_Noperspective>(ctx.allocator, attribute.source_info));
                    } break;

                    case Syntax_Node_Kind::attr_invariant: {
                        attributes.push_back(allocate<ast::Attr_Invariant>(ctx.allocator, attribute.source_info));
                    } break;

                    default:
                        // TODO: Error.
                        ANTON_ASSERT(false, "unreachable");
                        ANTON_UNREACHABLE();
                }
            }
        }
        return {anton::expected_value, attributes};
    }

    [[nodiscard]] static anton::Expected<bool, Error> evaluate_constant_expression(Context const& ctx, Syntax_Node const& node) {
        // TODO
        return {anton::expected_value, true};
    }

    [[nodiscard]] static anton::Expected<ast::Decl_List, Error> transform_decl_if(Context& ctx, Syntax_Node const& node) {
        anton::Expected<bool, Error> condition = evaluate_constant_expression(ctx, get_decl_if_condition(node));
        if(!condition) {
            return {anton::expected_error, ANTON_MOV(condition.error())};
        }

        if(condition.value()) {
            Syntax_Node const& then_node = get_decl_if_then_branch(node);
            return transform_syntax_tree_to_ast(ctx, then_node.children);
        } else {
            anton::Optional<Syntax_Node const&> else_node = get_decl_if_else_branch(node);
            if(else_node) {
                return transform_syntax_tree_to_ast(ctx, else_node.value().children);
            } else {
                return {anton::expected_value, ast::Decl_List{}};
            }
        }
    }

    [[nodiscard]] static anton::Expected<ast::Decl_List, Error> transform_decl_import(Context& ctx, Syntax_Node const& node) {
        Syntax_Node const& path_node = get_decl_import_path(node);
        Syntax_Token const& path_token = get_expr_lt_string_value(path_node);
        return import_source_code(ctx, path_token.value, node.source_info);
    }

    [[nodiscard]] static anton::Expected<ast::Decl_Constant const*, Error> transform_decl_constant(Context const& ctx, Syntax_Node const& node) {
        ast::Identifier const* const identifier = transform_identifier(ctx, get_decl_constant_identifier(node));
        anton::Expected<ast::Type const*, Error> type = transform_type(ctx, get_decl_constant_type(node));
        if(!type) {
            return {anton::expected_error, ANTON_MOV(type.error())};
        }

        anton::Expected<ast::Expr const*, Error> initializer = transform_expr(ctx, get_decl_constant_initializer(node));
        if(!initializer) {
            return {anton::expected_error, ANTON_MOV(initializer.error())};
        }

        return {anton::expected_value, allocate<ast::Decl_Constant>(ctx.allocator, identifier, type.value(), initializer.value(), node.source_info)};
    }

    [[nodiscard]] static anton::Expected<ast::Decl_Struct const*, Error> transform_decl_struct(Context const& ctx, Syntax_Node const& node) {
        Array<ast::Struct_Member const*>& members = *allocate<Array<ast::Struct_Member const*>>(ctx.allocator, ctx.allocator);
        Syntax_Node const& members_node = get_decl_struct_members(node);
        for(SNOT const& member_snot: members_node.children) {
            Syntax_Node const& member_node = member_snot.left();
            anton::Expected<ast::Attr_List, Error> attribute_list = transform_attribute_list(ctx, get_attribute_lists(member_node));
            if(!attribute_list) {
                return {anton::expected_error, ANTON_MOV(attribute_list.error())};
            }

            i64 const offset = get_past_attribute_lists_offset(member_node);
            ast::Identifier const* const identifier = transform_identifier(ctx, get_struct_member_identifier(member_node, offset));
            anton::Expected<ast::Type const*, Error> type = transform_type(ctx, get_struct_member_type(member_node, offset));
            if(!type) {
                return {anton::expected_error, ANTON_MOV(type.error())};
            }
            ast::Expr const* initializer = nullptr;
            anton::Optional initializer_node = get_struct_member_initializer(member_node, offset);
            if(initializer_node) {
                anton::Expected<ast::Expr const*, Error> result = transform_expr(ctx, initializer_node.value());
                if(result) {
                    initializer = result.value();
                } else {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }
            }

            members.push_back(allocate<ast::Struct_Member>(ctx.allocator, attribute_list.value(), identifier, type.value(), initializer, node.source_info));
        }

        ast::Identifier const* const identifier = transform_identifier(ctx, get_decl_struct_identifier(node));
        return {anton::expected_value, allocate<ast::Decl_Struct>(ctx.allocator, identifier, members, node.source_info)};
    }

    [[nodiscard]] static anton::Expected<ast::Func_Parameter const*, Error> transform_parameter(Context const& ctx, Syntax_Node const& node) {
        if(node.type == Syntax_Node_Kind::func_parameter) {
            ast::Identifier const* const identifier = transform_identifier(ctx, get_func_parameter_identifier(node));
            anton::Expected<ast::Type const*, Error> type = transform_type(ctx, get_func_parameter_type(node));
            if(!type) {
                return {anton::expected_error, ANTON_MOV(type.error())};
            }

            ast::Identifier const* source = nullptr;
            if(anton::Optional result = get_func_parameter_source(node)) {
                source = transform_identifier(ctx, result.value());
            }

            return {anton::expected_value, allocate<ast::Func_Parameter>(ctx.allocator, identifier, type.value(), source, node.source_info)};
        } else if(node.type == Syntax_Node_Kind::func_parameter_if) {
            anton::Expected<bool, Error> condition = evaluate_constant_expression(ctx, get_func_parameter_if_condition(node));
            if(!condition) {
                return {anton::expected_error, ANTON_MOV(condition.error())};
            }

            if(condition.value()) {
                Syntax_Node const& then_node = get_func_parameter_if_then_branch(node);
                return transform_parameter(ctx, then_node);
            } else {
                Syntax_Node const& else_node = get_func_parameter_if_else_branch(node);
                return transform_parameter(ctx, else_node);
            }
        } else {
            // TODO: Error.
            ANTON_ASSERT(false, "unreachable");
            ANTON_UNREACHABLE();
        }
    }

    [[nodiscard]] static anton::Expected<ast::Func_Parameter_List, Error> transform_parameter_list(Context const& ctx, Syntax_Node const& node) {
        Array<ast::Func_Parameter const*>& parameters = *allocate<Array<ast::Func_Parameter const*>>(ctx.allocator, ctx.allocator);
        for(SNOT const& snot: node.children) {
            if(!snot.is_left()) {
                continue;
            }

            Syntax_Node const& parameter_node = snot.left();
            anton::Expected<ast::Func_Parameter const*, Error> parameter = transform_parameter(ctx, parameter_node);
            if(parameter) {
                parameters.push_back(parameter.value());
            } else {
                return {anton::expected_error, ANTON_MOV(parameter.error())};
            }
        }
        return {anton::expected_value, parameters};
    }

    [[nodiscard]] static anton::Expected<ast::Decl_Function const*, Error> transform_decl_function(Context const& ctx, Syntax_Node const& node) {
        anton::Expected<ast::Attr_List, Error> attribute_list = transform_attribute_list(ctx, get_attribute_lists(node));
        if(!attribute_list) {
            return {anton::expected_error, ANTON_MOV(attribute_list.error())};
        }

        i64 const offset = get_past_attribute_lists_offset(node);
        ast::Identifier const* const identifier = transform_identifier(ctx, get_decl_function_identifier(node, offset));
        anton::Expected<ast::Func_Parameter_List, Error> parameters = transform_parameter_list(ctx, get_decl_function_parameter_list(node, offset));
        if(!parameters) {
            return {anton::expected_error, ANTON_MOV(parameters.error())};
        }

        anton::Expected<ast::Type const*, Error> return_type = transform_type(ctx, get_decl_function_return_type(node, offset));
        if(!return_type) {
            return {anton::expected_error, ANTON_MOV(return_type.error())};
        }

        anton::Expected<ast::Stmt_List, Error> body = transform_stmt_block_child_stmts(ctx, get_decl_function_body(node, offset));
        if(!body) {
            return {anton::expected_error, ANTON_MOV(body.error())};
        }

        return {anton::expected_value, allocate<ast::Decl_Function>(ctx.allocator, attribute_list.value(), identifier, parameters.value(), return_type.value(),
                                                                    body.value(), false, node.source_info)};
    }

    [[nodiscard]] static anton::Expected<ast::Decl_Stage_Function const*, Error> transform_decl_stage_function(Context const& ctx, Syntax_Node const& node) {
        auto transform_stage_kind = [](Syntax_Token const& token) -> ast::With_Source<Stage_Kind> {
            if(token.value == "vertex"_sv) {
                return {Stage_Kind::vertex, token.source_info};
            } else if(token.value == "fragment"_sv) {
                return {Stage_Kind::fragment, token.source_info};
            } else if(token.value == "compute"_sv) {
                return {Stage_Kind::compute, token.source_info};
            } else {
                // TODO: Error
                ANTON_ASSERT(false, "unreachable");
                ANTON_UNREACHABLE();
            }
        };

        anton::Expected<ast::Attr_List, Error> attribute_list = transform_attribute_list(ctx, get_attribute_lists(node));
        if(!attribute_list) {
            return {anton::expected_error, ANTON_MOV(attribute_list.error())};
        }

        i64 const offset = get_past_attribute_lists_offset(node);
        ast::Identifier const* const pass = transform_identifier(ctx, get_decl_stage_function_pass(node, offset));
        ast::With_Source<Stage_Kind> const stage = transform_stage_kind(get_decl_stage_function_stage(node, offset));
        anton::Expected<ast::Func_Parameter_List, Error> parameters = transform_parameter_list(ctx, get_decl_stage_function_parameter_list(node, offset));
        if(!parameters) {
            return {anton::expected_error, ANTON_MOV(parameters.error())};
        }

        anton::Expected<ast::Type const*, Error> return_type = transform_type(ctx, get_decl_stage_function_return_type(node, offset));
        if(!return_type) {
            return {anton::expected_error, ANTON_MOV(return_type.error())};
        }

        anton::Expected<ast::Stmt_List, Error> body = transform_stmt_block_child_stmts(ctx, get_decl_stage_function_body(node, offset));
        if(!body) {
            return {anton::expected_error, ANTON_MOV(body.error())};
        }

        return {anton::expected_value, allocate<ast::Decl_Stage_Function>(ctx.allocator, attribute_list.value(), pass, stage, parameters.value(),
                                                                          return_type.value(), body.value(), node.source_info)};
    }

    anton::Expected<ast::Decl_List, Error> transform_syntax_tree_to_ast(Context& ctx, Array<SNOT> const& syntax) {
        Array<ast::Decl const*>& abstract = *allocate<Array<ast::Decl const*>>(ctx.allocator, ctx.allocator);
        for(SNOT const& snot: syntax) {
            if(!snot.is_left()) {
                return {anton::expected_error};
            }

            Syntax_Node const& syntax_node = snot.left();
            switch(syntax_node.type) {
                case Syntax_Node_Kind::decl_if: {
                    anton::Expected<ast::Decl_List, Error> result = transform_decl_if(ctx, syntax_node);
                    if(!result) {
                        return {anton::expected_error, ANTON_MOV(result.error())};
                    }

                    ast::Decl_List const decls = result.value();
                    abstract.insert(abstract.end(), decls.begin(), decls.end());
                } break;

                case Syntax_Node_Kind::decl_import: {
                    anton::Expected<ast::Decl_List, Error> result = transform_decl_import(ctx, syntax_node);
                    if(!result) {
                        return {anton::expected_error, ANTON_MOV(result.error())};
                    }

                    ast::Decl_List const decls = result.value();
                    abstract.insert(abstract.end(), decls.begin(), decls.end());
                } break;

                case Syntax_Node_Kind::decl_constant: {
                    anton::Expected<ast::Decl_Constant const*, Error> result = transform_decl_constant(ctx, syntax_node);
                    if(!result) {
                        return {anton::expected_error, ANTON_MOV(result.error())};
                    }

                    ast::Decl_Constant const* const decl = result.value();
                    abstract.insert(abstract.end(), decl);
                } break;

                case Syntax_Node_Kind::decl_struct: {
                    anton::Expected<ast::Decl_Struct const*, Error> result = transform_decl_struct(ctx, syntax_node);
                    if(!result) {
                        return {anton::expected_error, ANTON_MOV(result.error())};
                    }

                    ast::Decl_Struct const* const decl = result.value();
                    abstract.insert(abstract.end(), decl);
                } break;

                case Syntax_Node_Kind::decl_settings: {
                    // TODO
                } break;

                case Syntax_Node_Kind::decl_function: {
                    anton::Expected<ast::Decl_Function const*, Error> result = transform_decl_function(ctx, syntax_node);
                    if(!result) {
                        return {anton::expected_error, ANTON_MOV(result.error())};
                    }

                    ast::Decl_Function const* const decl = result.value();
                    abstract.insert(abstract.end(), decl);
                } break;

                case Syntax_Node_Kind::decl_stage_function: {
                    anton::Expected<ast::Decl_Stage_Function const*, Error> result = transform_decl_stage_function(ctx, syntax_node);
                    if(!result) {
                        return {anton::expected_error, ANTON_MOV(result.error())};
                    }

                    ast::Decl_Stage_Function const* const decl = result.value();
                    abstract.insert(abstract.end(), decl);
                } break;

                default:
                    ANTON_UNREACHABLE();
            }
        }
        return {anton::expected_value, abstract};
    }
} // namespace vush
