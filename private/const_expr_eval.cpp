#include "anton/expected.hpp"
#include <const_expr_eval.hpp>

#include <diagnostics.hpp>

namespace vush {
    bool is_implicitly_convertible_to_boolean(Expr_Value_Type) {
        // bool, i32, f32 and f64 are all convertible to bool.
        return true;
    }

    anton::Expected<bool, anton::String> is_compiletime_evaluable(Context& ctx, Expression& expression) {
        // Postfix expressions, assignment binary expressions, prefix increment and decrement are not compiletime expressions.
        // Function calls are not compiletime because functions are not compiletime (with the exception of constructor calls which are TODO).
        switch(expression.node_type) {
            // We currently do not handle floats, member access
            // or array access and report them as non-compiletime.
            case AST_Node_Type::float_literal:
            case AST_Node_Type::member_access_expression:
            case AST_Node_Type::array_access_expression:
            default:
                return {anton::expected_value, false};

            case AST_Node_Type::bool_literal:
            case AST_Node_Type::integer_literal: {
                return {anton::expected_value, true};
            }

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& expr = (Identifier_Expression&)expression;
                Symbol* symbol = find_symbol(ctx, expr.value);
                if(!symbol) {
                    return {anton::expected_error, format_undefined_symbol(ctx, expr.source_info)};
                }

                return {anton::expected_value, symbol->node_type == Symbol_Type::constant_declaration};
            }

            case AST_Node_Type::parenthesised_expression: {
                Parenthesised_Expression& expr = (Parenthesised_Expression&)expression;
                anton::Expected<bool, anton::String> res = is_compiletime_evaluable(ctx, *expr.expression);
                return res;
            }

            case AST_Node_Type::binary_expression: {
                Binary_Expression& expr = (Binary_Expression&)expression;
                anton::Expected<bool, anton::String> lhs_res = is_compiletime_evaluable(ctx, *expr.lhs);
                if(!lhs_res) {
                    return lhs_res;
                }

                anton::Expected<bool, anton::String> rhs_res = is_compiletime_evaluable(ctx, *expr.rhs);
                if(!rhs_res) {
                    return rhs_res;
                }

                return {anton::expected_value, lhs_res.value() && rhs_res.value()};
            }

            case AST_Node_Type::prefix_expression: {
                Prefix_Expression& expr = static_cast<Prefix_Expression&>(expression);
                if(expr.type == Prefix_Expression_Type::dec || expr.type == Prefix_Expression_Type::inc) {
                    return {anton::expected_value, false};
                }

                anton::Expected<bool, anton::String> res = is_compiletime_evaluable(ctx, *expr.expression);
                return res;
            }
        }
    }

    anton::Expected<Expr_Value, anton::String> evaluate_const_expr(Context& ctx, Expression& expression) {
        switch(expression.node_type) {
            // We currently do not handle floats, member access
            // or array access and reject them as non-constant expressions.
            case AST_Node_Type::float_literal:
            case AST_Node_Type::member_access_expression:
            case AST_Node_Type::array_access_expression:
            default: {
                return {anton::expected_error,
                        err_expression_is_not_constant_evaluable(ctx, expression.source_info).format(ctx.allocator, ctx.diagnostics.extended)};
            }

            case AST_Node_Type::bool_literal: {
                Bool_Literal& expr = (Bool_Literal&)expression;
                Expr_Value e;
                e.type = Expr_Value_Type::int32;
                e.boolean = expr.value;
                return {anton::expected_value, e};
            }

            case AST_Node_Type::integer_literal: {
                Integer_Literal& expr = (Integer_Literal&)expression;
                i64 const value = str_to_i64(expr.value, (u64)expr.base);
                Expr_Value e;
                switch(expr.type) {
                    case Integer_Literal_Type::i32:
                        e.type = Expr_Value_Type::int32;
                        e.int32 = value;
                        break;
                    case Integer_Literal_Type::u32:
                        e.type = Expr_Value_Type::uint32;
                        e.uint32 = value;
                        break;
                }
                return {anton::expected_value, e};
            }

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& expr = (Identifier_Expression&)expression;
                Symbol* symbol = find_symbol(ctx, expr.value);
                if(!symbol) {
                    return {anton::expected_error, format_undefined_symbol(ctx, expr.source_info)};
                }

                if(symbol->node_type != Symbol_Type::constant_declaration) {
                    return {anton::expected_error,
                            err_identifier_does_not_name_constant(ctx, expr.source_info).format(ctx.allocator, ctx.diagnostics.extended)};
                }

                Constant_Declaration* decl = (Constant_Declaration*)symbol;
                return evaluate_const_expr(ctx, *decl->initializer);
            }

            case AST_Node_Type::parenthesised_expression: {
                Parenthesised_Expression& expr = (Parenthesised_Expression&)expression;
                return evaluate_const_expr(ctx, *expr.expression);
            }

            case AST_Node_Type::binary_expression: {
                Binary_Expression& expr = (Binary_Expression&)expression;
                switch(expr.type) {
                    // Assignments are not compiletime expressions.
                    case Binary_Expression_Type::assign:
                    case Binary_Expression_Type::add_assign:
                    case Binary_Expression_Type::sub_assign:
                    case Binary_Expression_Type::mul_assign:
                    case Binary_Expression_Type::div_assign:
                    case Binary_Expression_Type::mod_assign:
                    case Binary_Expression_Type::shl_assign:
                    case Binary_Expression_Type::shr_assign:
                    case Binary_Expression_Type::band_assign:
                    case Binary_Expression_Type::bor_assign:
                    case Binary_Expression_Type::bxor_assign: {
                        return {anton::expected_error,
                                err_expression_is_not_constant_evaluable(ctx, expression.source_info).format(ctx.allocator, ctx.diagnostics.extended)};
                    }

                    // We currently do not handle bitwise binary expressions and
                    // reject them as non-constant expressions.
                    case Binary_Expression_Type::bor:
                    case Binary_Expression_Type::bxor:
                    case Binary_Expression_Type::band: {
                        return {anton::expected_error,
                                err_expression_is_not_constant_evaluable(ctx, expression.source_info).format(ctx.allocator, ctx.diagnostics.extended)};
                    }

                    case Binary_Expression_Type::lor: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                        }

                        bool lhs_val = lhs_res.value().as_boolean();
                        bool rhs_val = rhs_res.value().as_boolean();
                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = lhs_val || rhs_val;
                        return {anton::expected_value, e};
                    }

                    case Binary_Expression_Type::lxor: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                        }

                        bool lhs_val = lhs_res.value().as_boolean();
                        bool rhs_val = rhs_res.value().as_boolean();
                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = (lhs_val && !rhs_val) || (!lhs_val && rhs_val);
                        return {anton::expected_value, e};
                    }

                    case Binary_Expression_Type::land: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                        }

                        bool lhs_val = lhs_res.value().as_boolean();
                        bool rhs_val = rhs_res.value().as_boolean();
                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = lhs_val && rhs_val;
                        return {anton::expected_value, e};
                    }

                    case Binary_Expression_Type::eq: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        // TODO: Extend to actual glsl behaviour of == operator

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '==' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '==' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.boolean = lhs.as_float64() == rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.boolean = lhs.as_float32() == rhs.as_float32();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.boolean = lhs.as_uint32() == rhs.as_uint32();
                            return {anton::expected_value, e};
                        } else {
                            e.boolean = lhs.as_int32() == rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::neq: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        // TODO: Extend to actual glsl behaviour of != operator

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '!=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '!=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.boolean = lhs.as_float64() != rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.boolean = lhs.as_float32() != rhs.as_float32();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.boolean = lhs.as_uint32() != rhs.as_uint32();
                            return {anton::expected_value, e};
                        } else {
                            e.boolean = lhs.as_int32() != rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::gt: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '>' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '>' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.boolean = lhs.as_float64() > rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.boolean = lhs.as_float32() > rhs.as_float32();
                            return {anton::expected_value, e};

                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.boolean = lhs.as_uint32() > rhs.as_uint32();
                            return {anton::expected_value, e};

                        } else {
                            e.boolean = lhs.as_int32() > rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::gteq: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '>=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '>=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.boolean = lhs.as_float64() >= rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.boolean = lhs.as_float32() >= rhs.as_float32();
                            return {anton::expected_value, e};

                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.boolean = lhs.as_uint32() >= rhs.as_uint32();
                            return {anton::expected_value, e};

                        } else {
                            e.boolean = lhs.as_int32() >= rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::lt: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '<' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '<' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.boolean = lhs.as_float64() < rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.boolean = lhs.as_float32() < rhs.as_float32();
                            return {anton::expected_value, e};

                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.boolean = lhs.as_uint32() < rhs.as_uint32();
                            return {anton::expected_value, e};

                        } else {
                            e.boolean = lhs.as_int32() < rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::lteq: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '<=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '<=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.boolean = lhs.as_float64() <= rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.boolean = lhs.as_float32() <= rhs.as_float32();
                            return {anton::expected_value, e};

                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.boolean = lhs.as_uint32() <= rhs.as_uint32();
                            return {anton::expected_value, e};

                        } else {
                            e.boolean = lhs.as_int32() <= rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::shl: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return {anton::expected_error, ANTON_MOV(lhs_res.error())};
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return {anton::expected_error, ANTON_MOV(rhs_res.error())};
                        }

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::uint32 && lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column, u8"left-hand side of '<<' is not of a scalar integer type")};
                        }

                        if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column, u8"right-hand side of '<<' is not of a scalar integer type")};
                        }

                        Expr_Value e;
                        e.type = lhs.type;
                        e.uint32 = lhs.uint32;
                        if(rhs.type == Expr_Value_Type::int32) {
                            i32 shift = rhs.int32;
                            if(shift < 0) {
                                Source_Info const& src = expr.rhs->source_info;
                                return {anton::expected_error,
                                        build_error_message(src.source_path, src.line, src.column, u8"right-hand side of '<<' is negative")};
                            }

                            e.uint32 <<= shift;
                        } else {
                            e.uint32 <<= rhs.uint32;
                        }

                        return {anton::expected_value, e};
                    }

                    case Binary_Expression_Type::shr: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::uint32 && lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column, u8"left-hand side of '>>' is not of a scalar integer type")};
                        }

                        if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column, u8"right-hand side of '>>' is not of a scalar integer type")};
                        }

                        Expr_Value e;
                        e.type = lhs.type;
                        if(lhs.type == Expr_Value_Type::int32) {
                            e.int32 = lhs.int32;
                            if(rhs.type == Expr_Value_Type::int32) {
                                i32 shift = rhs.int32;
                                if(shift < 0) {
                                    Source_Info const& src = expr.rhs->source_info;
                                    return {anton::expected_error,
                                            build_error_message(src.source_path, src.line, src.column, u8"right-hand side of '>>' is negative")};
                                }

                                e.int32 >>= shift;
                            } else {
                                e.int32 >>= rhs.uint32;
                            }
                        } else {
                            e.uint32 = lhs.uint32;
                            if(rhs.type == Expr_Value_Type::int32) {
                                i32 shift = rhs.int32;
                                if(shift < 0) {
                                    Source_Info const& src = expr.rhs->source_info;
                                    return {anton::expected_error,
                                            build_error_message(src.source_path, src.line, src.column, u8"right-hand side of '>>' is negative")};
                                }

                                e.int32 >>= shift;
                            } else {
                                e.int32 >>= rhs.uint32;
                            }
                        }

                        return {anton::expected_value, e};
                    }

                    case Binary_Expression_Type::add: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        // TODO: Expand to include vectors and matrices.

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '+' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '+' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.type = Expr_Value_Type::float64;
                            e.float64 = lhs.as_float64() + rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.type = Expr_Value_Type::float32;
                            e.float32 = lhs.as_float32() + rhs.as_float32();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.type = Expr_Value_Type::uint32;
                            e.uint32 = lhs.as_uint32() + rhs.as_uint32();
                            return {anton::expected_value, e};
                        } else {
                            e.type = Expr_Value_Type::int32;
                            e.int32 = lhs.as_int32() + rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::sub: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        // TODO: Expand to include vectors and matrices.

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '-' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '-' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.type = Expr_Value_Type::float64;
                            e.float64 = lhs.as_float64() - rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.type = Expr_Value_Type::float32;
                            e.float32 = lhs.as_float32() - rhs.as_float32();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.type = Expr_Value_Type::uint32;
                            e.uint32 = lhs.as_uint32() - rhs.as_uint32();
                            return {anton::expected_value, e};
                        } else {
                            e.type = Expr_Value_Type::int32;
                            e.int32 = lhs.as_int32() - rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::mul: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        // TODO: Expand to include vectors and matrices.

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '*' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '*' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.type = Expr_Value_Type::float64;
                            e.float64 = lhs.as_float64() * rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.type = Expr_Value_Type::float32;
                            e.float32 = lhs.as_float32() * rhs.as_float32();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.type = Expr_Value_Type::uint32;
                            e.uint32 = lhs.as_uint32() * rhs.as_uint32();
                            return {anton::expected_value, e};
                        } else {
                            e.type = Expr_Value_Type::int32;
                            e.int32 = lhs.as_int32() * rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::div: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        // TODO: Expand to include vectors and matrices.

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                           lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"left-hand side of '/' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '/' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.type = Expr_Value_Type::float64;
                            e.float64 = lhs.as_float64() / rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.type = Expr_Value_Type::float32;
                            e.float32 = lhs.as_float32() / rhs.as_float32();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.type = Expr_Value_Type::uint32;
                            e.uint32 = lhs.as_uint32() / rhs.as_uint32();
                            return {anton::expected_value, e};
                        } else {
                            e.type = Expr_Value_Type::int32;
                            e.int32 = lhs.as_int32() / rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Binary_Expression_Type::mod: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        // TODO: Expand to include integer vectors.

                        Expr_Value lhs = lhs_res.value();
                        Expr_Value rhs = rhs_res.value();
                        if(lhs.type != Expr_Value_Type::uint32 && lhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column, u8"left-hand side of '%' is not of a scalar integer type")};
                        }

                        if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column, u8"right-hand side of '%' is not of a scalar integer type")};
                        }

                        Expr_Value e;
                        if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                            e.type = Expr_Value_Type::float64;
                            e.float64 = lhs.as_float64() / rhs.as_float64();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                            e.type = Expr_Value_Type::float32;
                            e.float32 = lhs.as_float32() / rhs.as_float32();
                            return {anton::expected_value, e};
                        } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                            e.type = Expr_Value_Type::uint32;
                            e.uint32 = lhs.as_uint32() / rhs.as_uint32();
                            return {anton::expected_value, e};
                        } else {
                            e.type = Expr_Value_Type::int32;
                            e.int32 = lhs.as_int32() / rhs.as_int32();
                            return {anton::expected_value, e};
                        }
                    }
                }
            }

            case AST_Node_Type::prefix_expression: {
                Prefix_Expression& expr = static_cast<Prefix_Expression&>(expression);
                switch(expr.type) {
                    case Prefix_Expression_Type::inc:
                    case Prefix_Expression_Type::dec: {
                        return {anton::expected_error,
                                err_expression_is_not_constant_evaluable(ctx, expression.source_info).format(ctx.allocator, ctx.diagnostics.extended)};
                    }

                    case Prefix_Expression_Type::plus: {
                        anton::Expected<Expr_Value, anton::String> base = evaluate_const_expr(ctx, *expr.expression);
                        return base;
                    }

                    case Prefix_Expression_Type::minus: {
                        anton::Expected<Expr_Value, anton::String> base_res = evaluate_const_expr(ctx, *expr.expression);
                        if(!base_res) {
                            return base_res;
                        }

                        // TODO: Extend to vectors and matrices.

                        Expr_Value base = base_res.value();
                        if(base.type != Expr_Value_Type::float64 && base.type != Expr_Value_Type::float32 && base.type != Expr_Value_Type::uint32 &&
                           base.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column,
                                                        u8"right-hand side of '-' is not of a scalar integer or a scalar floating-point type")};
                        }

                        Expr_Value e;
                        if(base.type == Expr_Value_Type::float64) {
                            e.type = Expr_Value_Type::float64;
                            e.float64 = -base.as_float64();
                            return {anton::expected_value, e};
                        } else if(base.type == Expr_Value_Type::float32) {
                            e.type = Expr_Value_Type::float32;
                            e.float32 = -base.as_float32();
                            return {anton::expected_value, e};
                        } else if(base.type == Expr_Value_Type::uint32) {
                            e.type = Expr_Value_Type::uint32;
                            e.uint32 = -base.as_uint32();
                            return {anton::expected_value, e};
                        } else {
                            e.type = Expr_Value_Type::int32;
                            e.int32 = -base.as_int32();
                            return {anton::expected_value, e};
                        }
                    }

                    case Prefix_Expression_Type::lnot: {
                        anton::Expected<Expr_Value, anton::String> base_res = evaluate_const_expr(ctx, *expr.expression);
                        if(!base_res) {
                            return base_res;
                        }

                        Expr_Value base = base_res.value();
                        if(base.type != Expr_Value_Type::boolean && !is_implicitly_convertible_to_boolean(base.type)) {
                            Source_Info const& src = expr.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column, u8"right-hand side of '!' is not a boolean")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = !base.as_boolean();
                        return {anton::expected_value, e};
                    }

                    case Prefix_Expression_Type::bnot: {
                        anton::Expected<Expr_Value, anton::String> base_res = evaluate_const_expr(ctx, *expr.expression);
                        if(!base_res) {
                            return {anton::expected_error, ANTON_MOV(base_res.error())};
                        }

                        // TODO: Extend to work on integer vectors.

                        Expr_Value base = base_res.value();
                        if(base.type != Expr_Value_Type::uint32 && base.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.source_path, src.line, src.column, u8"right-hand side of '~' is not a scalar integer")};
                        }

                        Expr_Value e;
                        if(base.type == Expr_Value_Type::uint32) {
                            e.type = Expr_Value_Type::uint32;
                            e.uint32 = ~base.uint32;
                        } else {
                            e.type = Expr_Value_Type::int32;
                            e.int32 = ~base.int32;
                        }
                        return {anton::expected_value, e};
                    }
                }
            }
        }
    }
} // namespace vush
