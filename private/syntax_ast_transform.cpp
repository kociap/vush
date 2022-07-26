#include <ast.hpp>

#include <anton/expected.hpp>
#include <anton/intrinsics.hpp>
#include <anton/optional.hpp>

#include <ast_fwd.hpp>
#include <context.hpp>
#include <diagnostics.hpp>
#include <memory.hpp>
#include <owning_ptr.hpp>
#include <syntax_accessors.hpp>
#include <vush/vush.hpp>


namespace vush {
    using namespace anton::literals;

    [[nodiscard]] static anton::Expected<Expression_List, Error> transform_call_arg_list(Context const& ctx, Syntax_Node const& node);
    [[nodiscard]] static anton::Expected<Owning_Ptr<Expression>, Error> transform_expr_block(Context const& ctx, Syntax_Node const& node);
    [[nodiscard]] static anton::Expected<Owning_Ptr<Expression>, Error> transform_expr(Context const& ctx, Syntax_Node const& node);
    [[nodiscard]] static anton::Expected<Declaration_List, Error> transform_decl_block(Context const& ctx, Syntax_Node const& node);

    anton::Expected<Expression_List, Error> transform_call_arg_list(Context const& ctx, Syntax_Node const& node) {
        Expression_List arguments;
        for(SNOT const& snot: node.children) {
            if(snot.is_left()) {
                anton::Expected<Owning_Ptr<Expression>, Error> expression = transform_expr(ctx, snot.left());
                if(expression) {
                    arguments.push_back(ANTON_MOV(*expression));
                } else {
                    return {anton::expected_error, ANTON_MOV(expression.error())};
                }
            }
        }
        return {anton::expected_value, ANTON_MOV(arguments)};
    }

    anton::Expected<Owning_Ptr<Expression>, Error> transform_expr_block(Context const& ctx, Syntax_Node const& node) {
        Syntax_Node const& expr_node = get_expr_block_expression(node);
        anton::Expected<Owning_Ptr<Expression>, Error> expr = transform_expr(ctx, expr_node);
        return expr;
    }

    anton::Expected<Owning_Ptr<Expression>, Error> transform_expr(Context const& ctx, Syntax_Node const& node) {
        switch(node.type) {
            case Syntax_Node_Type::expr_if: {
                Syntax_Node const& condition_node = get_expr_if_condition(node);
                anton::Expected<Owning_Ptr<Expression>, Error> condition = transform_expr(ctx, condition_node);
                if(!condition) {
                    return ANTON_MOV(condition);
                }

                Syntax_Node const& then_branch_node = get_expr_if_then_branch(node);
                anton::Expected<Owning_Ptr<Expression>, Error> then_branch = transform_expr_block(ctx, then_branch_node);
                if(!then_branch) {
                    return ANTON_MOV(then_branch);
                }

                Syntax_Node const& else_branch_node = get_expr_if_else_branch(node);
                anton::Expected<Owning_Ptr<Expression>, Error> else_branch = transform_expr_block(ctx, else_branch_node);
                if(!else_branch) {
                    return ANTON_MOV(else_branch);
                }

                return {anton::expected_value,
                        Owning_Ptr<Expression>{downcast, allocate_owning<Expression_If>(ctx.allocator, ANTON_MOV(*condition), ANTON_MOV(*then_branch),
                                                                                        ANTON_MOV(*else_branch), node.source_info)}};
            } break;

            case Syntax_Node_Type::expr_identifier: {
                Syntax_Token const& identifier_token = get_expr_identifier_identifier(node);
                return {anton::expected_value,
                        Owning_Ptr<Expression>{downcast, allocate_owning<Identifier_Expression>(ctx.allocator, identifier_token.value, node.source_info)}};
            } break;

            case Syntax_Node_Type::expr_binary: {
                auto translate_type = [](Syntax_Node_Type const type) -> Binary_Expression_Type {
                    switch(type) {
                        case Syntax_Node_Type::tk_langle:
                            return Binary_Expression_Type::lt;
                        case Syntax_Node_Type::tk_rangle:
                            return Binary_Expression_Type::gt;
                        case Syntax_Node_Type::tk_plus:
                            return Binary_Expression_Type::add;
                        case Syntax_Node_Type::tk_minus:
                            return Binary_Expression_Type::sub;
                        case Syntax_Node_Type::tk_asterisk:
                            return Binary_Expression_Type::mul;
                        case Syntax_Node_Type::tk_slash:
                            return Binary_Expression_Type::div;
                        case Syntax_Node_Type::tk_percent:
                            return Binary_Expression_Type::mod;
                        case Syntax_Node_Type::tk_amp:
                            return Binary_Expression_Type::band;
                        case Syntax_Node_Type::tk_pipe:
                            return Binary_Expression_Type::bor;
                        case Syntax_Node_Type::tk_hat:
                            return Binary_Expression_Type::bxor;
                        case Syntax_Node_Type::tk_equals:
                            return Binary_Expression_Type::assign;
                        case Syntax_Node_Type::tk_amp2:
                            return Binary_Expression_Type::land;
                        case Syntax_Node_Type::tk_pipe2:
                            return Binary_Expression_Type::lor;
                        case Syntax_Node_Type::tk_hat2:
                            return Binary_Expression_Type::lxor;
                        case Syntax_Node_Type::tk_shl:
                            return Binary_Expression_Type::shl;
                        case Syntax_Node_Type::tk_shr:
                            return Binary_Expression_Type::shr;
                        case Syntax_Node_Type::tk_eq2:
                            return Binary_Expression_Type::eq;
                        case Syntax_Node_Type::tk_neq:
                            return Binary_Expression_Type::neq;
                        case Syntax_Node_Type::tk_lteq:
                            return Binary_Expression_Type::lteq;
                        case Syntax_Node_Type::tk_gteq:
                            return Binary_Expression_Type::gteq;
                        case Syntax_Node_Type::tk_pluseq:
                            return Binary_Expression_Type::add_assign;
                        case Syntax_Node_Type::tk_minuseq:
                            return Binary_Expression_Type::sub_assign;
                        case Syntax_Node_Type::tk_asteriskeq:
                            return Binary_Expression_Type::mul_assign;
                        case Syntax_Node_Type::tk_slasheq:
                            return Binary_Expression_Type::div_assign;
                        case Syntax_Node_Type::tk_percenteq:
                            return Binary_Expression_Type::mod_assign;
                        case Syntax_Node_Type::tk_ampeq:
                            return Binary_Expression_Type::band_assign;
                        case Syntax_Node_Type::tk_pipeeq:
                            return Binary_Expression_Type::bor_assign;
                        case Syntax_Node_Type::tk_hateq:
                            return Binary_Expression_Type::bxor_assign;
                        case Syntax_Node_Type::tk_shleq:
                            return Binary_Expression_Type::shl_assign;
                        case Syntax_Node_Type::tk_shreq:
                            return Binary_Expression_Type::shr_assign;
                        default:
                            ANTON_ASSERT(false, ""); // TODO: ERROR
                            ANTON_UNREACHABLE();
                    }
                };

                Syntax_Token const& operator_token = get_expr_binary_operator(node);
                Binary_Expression_Type const type = translate_type(operator_token.type);

                Syntax_Node const& lhs_node = get_expr_binary_lhs(node);
                anton::Expected<Owning_Ptr<Expression>, Error> lhs = transform_expr(ctx, lhs_node);
                if(!lhs) {
                    return ANTON_MOV(lhs);
                }

                Syntax_Node const& rhs_node = get_expr_binary_rhs(node);
                anton::Expected<Owning_Ptr<Expression>, Error> rhs = transform_expr(ctx, rhs_node);
                if(!rhs) {
                    return ANTON_MOV(rhs);
                }

                return {anton::expected_value, Owning_Ptr<Expression>{downcast, allocate_owning<Binary_Expression>(ctx.allocator, type, ANTON_MOV(*lhs),
                                                                                                                   ANTON_MOV(*rhs), node.source_info)}};
            } break;

            case Syntax_Node_Type::expr_prefix: {
                auto translate_type = [](Syntax_Node_Type const type) -> Prefix_Expression_Type {
                    switch(type) {
                        case Syntax_Node_Type::tk_plus:
                            return Prefix_Expression_Type::plus;
                        case Syntax_Node_Type::tk_minus:
                            return Prefix_Expression_Type::minus;
                        case Syntax_Node_Type::tk_plus2:
                            return Prefix_Expression_Type::inc;
                        case Syntax_Node_Type::tk_minus2:
                            return Prefix_Expression_Type::dec;
                        case Syntax_Node_Type::tk_bang:
                            return Prefix_Expression_Type::lnot;
                        case Syntax_Node_Type::tk_tilde:
                            return Prefix_Expression_Type::bnot;
                        default:
                            ANTON_ASSERT(false, ""); // TODO: ERROR
                            ANTON_UNREACHABLE();
                    }
                };

                Syntax_Token const& operator_token = get_expr_prefix_operator(node);
                Prefix_Expression_Type const type = translate_type(operator_token.type);

                Syntax_Node const& expression_node = get_expr_prefix_expression(node);
                anton::Expected<Owning_Ptr<Expression>, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                return {anton::expected_value,
                        Owning_Ptr<Expression>{downcast, allocate_owning<Prefix_Expression>(ctx.allocator, type, ANTON_MOV(*expression), node.source_info)}};
            } break;

            case Syntax_Node_Type::expr_postfix: {
                auto translate_type = [](Syntax_Node_Type const type) -> Postfix_Expression_Type {
                    switch(type) {
                        case Syntax_Node_Type::tk_plus2:
                            return Postfix_Expression_Type::inc;
                        case Syntax_Node_Type::tk_minus2:
                            return Postfix_Expression_Type::dec;
                        default:
                            ANTON_ASSERT(false, ""); // TODO: ERROR
                            ANTON_UNREACHABLE();
                    }
                };

                Syntax_Token const& operator_token = get_expr_postfix_operator(node);
                Postfix_Expression_Type const type = translate_type(operator_token.type);

                Syntax_Node const& expression_node = get_expr_postfix_expression(node);
                anton::Expected<Owning_Ptr<Expression>, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                return {anton::expected_value,
                        Owning_Ptr<Expression>{downcast, allocate_owning<Postfix_Expression>(ctx.allocator, type, ANTON_MOV(*expression), node.source_info)}};
            } break;

            case Syntax_Node_Type::expr_member_access: {
                Syntax_Node const& expression_node = get_expr_member_access_expression(node);
                anton::Expected<Owning_Ptr<Expression>, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                Syntax_Token const& identifier_token = get_expr_member_access_identifier(node);
                Owning_Ptr identifier = allocate_owning<Identifier>(ctx.allocator, identifier_token.value, identifier_token.source_info);

                Owning_Ptr result = allocate_owning<Member_Access_Expression>(ctx.allocator, ANTON_MOV(*expression), ANTON_MOV(identifier), node.source_info);
                return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
            } break;

            case Syntax_Node_Type::expr_array_access: {
                Syntax_Node const& expression_node = get_expr_array_access_expression(node);
                anton::Expected<Owning_Ptr<Expression>, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                Syntax_Node const& index_node = get_expr_array_access_index(node);
                anton::Expected<Owning_Ptr<Expression>, Error> index = transform_expr(ctx, index_node);
                if(!index) {
                    return ANTON_MOV(index);
                }

                Owning_Ptr result = allocate_owning<Array_Access_Expression>(ctx.allocator, ANTON_MOV(*expression), ANTON_MOV(*index), node.source_info);
                return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
            } break;

            case Syntax_Node_Type::expr_parentheses: {
                Syntax_Node const& expression_node = get_expr_parentheses_expression(node);
                anton::Expected<Owning_Ptr<Expression>, Error> expression = transform_expr(ctx, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                Owning_Ptr result = allocate_owning<Parenthesised_Expression>(ctx.allocator, ANTON_MOV(*expression), node.source_info);
                return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
            } break;

            case Syntax_Node_Type::expr_reinterpret: {
                ANTON_UNREACHABLE();
            } break;

            case Syntax_Node_Type::expr_call: {
                Syntax_Token const& identifier_token = get_expr_call_identifier(node);
                Owning_Ptr identifier = allocate_owning<Identifier>(ctx.allocator, identifier_token.value, identifier_token.source_info);

                Syntax_Node const& arguments_node = get_expr_call_arguments(node);
                anton::Expected<Expression_List, Error> arguments = transform_call_arg_list(ctx, arguments_node);
                if(!arguments) {
                    return {anton::expected_error, ANTON_MOV(arguments.error())};
                }

                Owning_Ptr result = allocate_owning<Function_Call_Expression>(ctx.allocator, ANTON_MOV(identifier), ANTON_MOV(*arguments), node.source_info);
                return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
            } break;

            case Syntax_Node_Type::expr_literal: {
                Syntax_Token const& value_token = get_expr_literal_value(node);
                switch(value_token.type) {
                    case Syntax_Node_Type::lt_bool: {
                        bool const value = value_token.value == "true"_sv;
                        Owning_Ptr result = allocate_owning<Bool_Literal>(ctx.allocator, value, node.source_info);
                        return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
                    }

                    case Syntax_Node_Type::lt_string: {
                        Owning_Ptr result = allocate_owning<String_Literal>(ctx.allocator, value_token.value, node.source_info);
                        return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
                    }

                    case Syntax_Node_Type::lt_float: {
                        // The default float literal type is f32.
                        Float_Literal_Type type = Float_Literal_Type::f32;
                        if(anton::Optional<Syntax_Token const&> suffix_token = get_expr_literal_suffix(node)) {
                            anton::String_View const suffix = suffix_token->value;
                            if(suffix == "d"_sv || suffix == "D"_sv) {
                                type = Float_Literal_Type::f64;
                            } else {
                                return {anton::expected_error, format_invalid_float_suffix(ctx, suffix_token->source_info)};
                            }
                        }

                        Owning_Ptr result = allocate_owning<Float_Literal>(ctx.allocator, value_token.value, type, node.source_info);
                        return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
                    }

                    case Syntax_Node_Type::lt_bin_integer:
                    case Syntax_Node_Type::lt_oct_integer:
                    case Syntax_Node_Type::lt_dec_integer:
                    case Syntax_Node_Type::lt_hex_integer: {
                        // The default integer literal type is i32.
                        Integer_Literal_Type type = Integer_Literal_Type::i32;
                        if(anton::Optional<Syntax_Token const&> suffix_token = get_expr_literal_suffix(node)) {
                            anton::String_View const suffix = suffix_token->value;
                            if(suffix == "u"_sv || suffix == "U"_sv) {
                                type = Integer_Literal_Type::u32;
                            } else {
                                return {anton::expected_error, format_invalid_float_suffix(ctx, suffix_token->source_info)};
                            }
                        }

                        Integer_Literal_Base base;
                        switch(value_token.type) {
                            case Syntax_Node_Type::lt_bin_integer:
                                base = Integer_Literal_Base::bin;
                                break;
                            case Syntax_Node_Type::lt_oct_integer:
                                base = Integer_Literal_Base::oct;
                                break;
                            case Syntax_Node_Type::lt_dec_integer:
                                base = Integer_Literal_Base::dec;
                                break;
                            case Syntax_Node_Type::lt_hex_integer:
                                base = Integer_Literal_Base::hex;
                                break;

                            default:
                                ANTON_ASSERT(false, ""); // TODO: Error
                                ANTON_UNREACHABLE();
                        }

                        Owning_Ptr result = allocate_owning<Integer_Literal>(ctx.allocator, value_token.value, type, base, node.source_info);
                        return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
                    }

                    default:
                        ANTON_UNREACHABLE();
                }
            } break;

            case Syntax_Node_Type::expr_default: {
                Owning_Ptr result = allocate_owning<Default_Expression>(ctx.allocator, node.source_info);
                return {anton::expected_value, Owning_Ptr<Expression>{downcast, ANTON_MOV(result)}};
            } break;

            default:
                ANTON_UNREACHABLE();
        }
    }

    anton::Expected<Declaration_List, Error> transform_decl_block(Context const& ctx, Syntax_Node const& node) {
        return {anton::expected_error, Error{}};
    }

    [[nodiscard]] static anton::Expected<Owning_Ptr<AST_Node>, Error> transform_decl_if(Context const& ctx, Syntax_Node const& node) {
        Syntax_Node const& condition_node = get_decl_if_condition(node);
        anton::Expected<Owning_Ptr<Expression>, Error> condition = transform_expr(ctx, condition_node);
        if(!condition) {
            return {anton::expected_error, ANTON_MOV(condition.error())};
        }

        Syntax_Node const& then_branch_node = get_decl_if_then_branch(node);
        anton::Expected<Declaration_List, Error> then_branch = transform_decl_block(ctx, then_branch_node);
        if(!then_branch) {
            return {anton::expected_error, ANTON_MOV(then_branch.error())};
        }

        Syntax_Node const& else_branch_node = get_decl_if_else_branch(node);
        anton::Expected<Declaration_List, Error> else_branch = transform_decl_block(ctx, else_branch_node);
        if(!else_branch) {
            return {anton::expected_error, ANTON_MOV(else_branch.error())};
        }

        return {anton::expected_value,
                allocate_owning<Declaration_If>(ctx.allocator, ANTON_MOV(*condition), ANTON_MOV(*then_branch), ANTON_MOV(*else_branch), node.source_info)};
    }

    anton::Expected<Declaration_List, Error> transform_syntax_tree_to_ast(Context const& ctx, Array<SNOT> const& syntax) {
        Declaration_List ast;
        for(SNOT const& snot: syntax) {
            if(!snot.is_left()) {
                return {anton::expected_error};
            }

            Syntax_Node const& syntax_node = snot.left();
            switch(syntax_node.type) {
                case Syntax_Node_Type::decl_if: {
                } break;

                case Syntax_Node_Type::decl_import: {
                } break;

                case Syntax_Node_Type::decl_constant: {
                } break;

                case Syntax_Node_Type::decl_struct: {
                } break;

                case Syntax_Node_Type::decl_settings: {
                } break;

                case Syntax_Node_Type::decl_function: {
                } break;

                case Syntax_Node_Type::decl_pass_stage: {
                } break;

                default:
                    ANTON_UNREACHABLE();
            }
        }
        return {anton::expected_value, ANTON_MOV(ast)};
    }
} // namespace vush
