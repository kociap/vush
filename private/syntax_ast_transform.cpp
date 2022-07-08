#include <ast.hpp>

#include <ast_fwd.hpp>
#include <memory.hpp>
#include <owning_ptr.hpp>
#include <vush/vush.hpp>

#include <anton/expected.hpp>
#include <anton/intrinsics.hpp>

#include <syntax_accessors.hpp>

namespace vush {
    [[nodiscard]] static anton::Expected<Owning_Ptr<Expression>, Error> transform_expr_block(Allocator* const allocator, Syntax_Node const& node);
    [[nodiscard]] static anton::Expected<Owning_Ptr<Expression>, Error> transform_expr(Allocator* const allocator, Syntax_Node const& node);

    anton::Expected<Owning_Ptr<Expression>, Error> transform_expr_block(Allocator* const allocator, Syntax_Node const& node) {
        Syntax_Node const& expr_node = get_expr_block_expression(node);
        anton::Expected<Owning_Ptr<Expression>, Error> expr = transform_expr(allocator, expr_node);
        return expr;
    }

    anton::Expected<Owning_Ptr<Expression>, Error> transform_expr(Allocator* const allocator, Syntax_Node const& node) {
        switch(node.type) {
            case Syntax_Node_Type::expr_if: {
                Syntax_Node const& condition_node = get_expr_if_condition(node);
                anton::Expected<Owning_Ptr<Expression>, Error> condition = transform_expr(allocator, condition_node);
                if(!condition) {
                    return ANTON_MOV(condition);
                }

                Syntax_Node const& then_branch_node = get_expr_if_then_branch(node);
                anton::Expected<Owning_Ptr<Expression>, Error> then_branch = transform_expr_block(allocator, then_branch_node);
                if(!then_branch) {
                    return ANTON_MOV(then_branch);
                }

                Syntax_Node const& else_branch_node = get_expr_if_else_branch(node);
                anton::Expected<Owning_Ptr<Expression>, Error> else_branch = transform_expr_block(allocator, else_branch_node);
                if(!else_branch) {
                    return ANTON_MOV(else_branch);
                }

                return {anton::expected_value,
                        Owning_Ptr<Expression>{downcast, allocate_owning<Expression_If>(allocator, ANTON_MOV(*condition), ANTON_MOV(*then_branch),
                                                                                        ANTON_MOV(*else_branch), node.source_info)}};
            } break;

            case Syntax_Node_Type::expr_identifier: {
                Syntax_Token const& identifier_token = get_expr_identifier_identifier(node);
                return {anton::expected_value,
                        Owning_Ptr<Expression>{downcast, allocate_owning<Identifier_Expression>(allocator, identifier_token.value, node.source_info)}};
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
                anton::Expected<Owning_Ptr<Expression>, Error> lhs = transform_expr(allocator, lhs_node);
                if(!lhs) {
                    return ANTON_MOV(lhs);
                }

                Syntax_Node const& rhs_node = get_expr_binary_rhs(node);
                anton::Expected<Owning_Ptr<Expression>, Error> rhs = transform_expr(allocator, rhs_node);
                if(!rhs) {
                    return ANTON_MOV(rhs);
                }

                return {anton::expected_value, Owning_Ptr<Expression>{downcast, allocate_owning<Binary_Expression>(allocator, type, ANTON_MOV(*lhs),
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
                anton::Expected<Owning_Ptr<Expression>, Error> expression = transform_expr(allocator, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                return {anton::expected_value,
                        Owning_Ptr<Expression>{downcast, allocate_owning<Prefix_Expression>(allocator, type, ANTON_MOV(*expression), node.source_info)}};
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
                anton::Expected<Owning_Ptr<Expression>, Error> expression = transform_expr(allocator, expression_node);
                if(!expression) {
                    return ANTON_MOV(expression);
                }

                return {anton::expected_value,
                        Owning_Ptr<Expression>{downcast, allocate_owning<Postfix_Expression>(allocator, type, ANTON_MOV(*expression), node.source_info)}};
            } break;

            case Syntax_Node_Type::expr_member_access: {
            } break;

            case Syntax_Node_Type::expr_array_access: {
            } break;

            case Syntax_Node_Type::expr_parentheses: {
            } break;

            case Syntax_Node_Type::expr_reinterpret: {
            } break;

            case Syntax_Node_Type::expr_call: {
            } break;

            case Syntax_Node_Type::expr_literal: {
            } break;

            case Syntax_Node_Type::expr_default: {
            } break;

            default:
                ANTON_UNREACHABLE();
        }
    }

    [[nodiscard]] static anton::Expected<Owning_Ptr<AST_Node>, Error> transform_decl_if(Allocator* const allocator, Syntax_Node const& node) {
        anton::Expected<Owning_Ptr<Expression>, Error> condition = transform_expr();
        anton::Expected<Declaration_List, Error> then_branch = transform_decl_block();
        anton::Expected<Declaration_List, Error> else_branch = transform_decl_block();
        return {anton::expected_value,
                allocate_owning<Declaration_If>(allocator, ANTON_MOV(condition), ANTON_MOV(then_branch), ANTON_MOV(else_branch), node.source_info)};
    }

    anton::Expected<Array<Owning_Ptr<AST_Node>>, Error> transform_syntax_tree_to_ast(Allocator* const allocator, Array<SNOT> const& syntax) {
        Array<Owning_Ptr<AST_Node>> ast;
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
