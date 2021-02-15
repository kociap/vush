#include <const_expr_eval.hpp>

#include <diagnostics.hpp>

namespace vush {
    bool is_implicitly_convertible_to_boolean(Expr_Value_Type) {
        // bool, i32, f32 and f64 are all convertible to bool.
        return true;
    }

    anton::Expected<bool, anton::String> is_compiletime_evaluable(Context& ctx, Expression& expression) {
        // postfix/prefix increment and decrement, assignment and arithmetic assignment are not compiletime because they don't operate on constants.
        // function call is not compiletime because functions are not compiletime (with the exception of constructor calls which are TODO).
        switch(expression.node_type) {
            case AST_Node_Type::bool_literal:
            case AST_Node_Type::integer_literal: {
                return {anton::expected_value, true};
            }

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& expr = (Identifier_Expression&)expression;
                Symbol* symbol = find_symbol(ctx, expr.identifier->value);
                if(!symbol) {
                    anton::String msg = u8"unknown identifier '" + expr.identifier->value + u8"'";
                    Source_Info const& src = expr.source_info;
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, msg)};
                }

                return {anton::expected_value, symbol->type == Symbol_Type::constant};
            }

            case AST_Node_Type::paren_expr: {
                Paren_Expr& expr = (Paren_Expr&)expression;
                anton::Expected<bool, anton::String> res = is_compiletime_evaluable(ctx, *expr.expression);
                return res;
            }

            case AST_Node_Type::binary_expr: {
                Binary_Expr& expr = (Binary_Expr&)expression;
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

            case AST_Node_Type::unary_expression: {
                Unary_Expression& expr = (Unary_Expression&)expression;
                anton::Expected<bool, anton::String> res = is_compiletime_evaluable(ctx, *expr.expression);
                return res;
            }

                // case AST_Node_Type::member_access_expression:

                // case AST_Node_Type::array_access_expression:

            default: {
                return {anton::expected_value, false};
            }
        }
    }

    anton::Expected<Expr_Value, anton::String> evaluate_const_expr(Context& ctx, Expression& expression) {
        switch(expression.node_type) {
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
                if(expr.type == Integer_Literal_Type::i32) {
                    e.type = Expr_Value_Type::int32;
                    e.int32 = value;
                } else if(expr.type == Integer_Literal_Type::u32) {
                    e.type = Expr_Value_Type::uint32;
                    e.uint32 = value;
                } else {
                    ANTON_UNREACHABLE();
                }
                return {anton::expected_value, e};
            }

                // case AST_Node_Type::float_literal: {}

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& expr = (Identifier_Expression&)expression;
                Symbol* symbol = find_symbol(ctx, expr.identifier->value);
                if(!symbol) {
                    anton::String msg = u8"unknown identifier '" + expr.identifier->value + u8"'";
                    Source_Info const& src = expr.source_info;
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, msg)};
                }

                if(symbol->type != Symbol_Type::constant) {
                    Source_Info const& src = expr.source_info;
                    anton::String msg = u8"identifier '" + expr.identifier->value + u8"' does not name a constant";
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, msg)};
                }

                Constant_Declaration* decl = (Constant_Declaration*)symbol->declaration;
                return evaluate_const_expr(ctx, *decl->initializer);
            }

            case AST_Node_Type::paren_expr: {
                Paren_Expr& expr = (Paren_Expr&)expression;
                return evaluate_const_expr(ctx, *expr.expression);
            }

            case AST_Node_Type::binary_expr: {
                Binary_Expr& expr = (Binary_Expr&)expression;
                switch(expr.type) {
                    case Binary_Expr_Type::logic_or: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                        }

                        bool lhs_val = lhs_res.value().as_boolean();
                        bool rhs_val = rhs_res.value().as_boolean();
                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = lhs_val || rhs_val;
                        return {anton::expected_value, e};
                    }

                    case Binary_Expr_Type::logic_xor: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                        }

                        bool lhs_val = lhs_res.value().as_boolean();
                        bool rhs_val = rhs_res.value().as_boolean();
                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = (lhs_val && !rhs_val) || (!lhs_val && rhs_val);
                        return {anton::expected_value, e};
                    }

                    case Binary_Expr_Type::logic_and: {
                        anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                        if(!lhs_res) {
                            return lhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                            Source_Info const& src = expr.lhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                        }

                        anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                        if(!rhs_res) {
                            return rhs_res;
                        }

                        if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"expression is not implicitly convertible to bool")};
                        }

                        bool lhs_val = lhs_res.value().as_boolean();
                        bool rhs_val = rhs_res.value().as_boolean();
                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = lhs_val && rhs_val;
                        return {anton::expected_value, e};
                    }

                    case Binary_Expr_Type::equal: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '==' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::unequal: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '!=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::greater_than: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '>' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::greater_equal: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '>=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::less_than: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '<' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::less_equal: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '<=' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                        // case Binary_Expr_Type::bit_or_expr: {}

                        // case Binary_Expr_Type::bit_xor_expr: {}

                        // case Binary_Expr_Type::bit_and_expr: {}

                    case Binary_Expr_Type::lshift: {
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
                                    build_error_message(src.file_path, src.line, src.column, u8"left-hand side of '<<' is not of a scalar integer type")};
                        }

                        if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"right-hand side of '<<' is not of a scalar integer type")};
                        }

                        Expr_Value e;
                        e.type = lhs.type;
                        e.uint32 = lhs.uint32;
                        if(rhs.type == Expr_Value_Type::int32) {
                            i32 shift = rhs.int32;
                            if(shift < 0) {
                                Source_Info const& src = expr.rhs->source_info;
                                return {anton::expected_error,
                                        build_error_message(src.file_path, src.line, src.column, u8"right-hand side of '<<' is negative")};
                            }

                            e.uint32 <<= shift;
                        } else {
                            e.uint32 <<= rhs.uint32;
                        }

                        return {anton::expected_value, e};
                    }

                    case Binary_Expr_Type::rshift: {
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
                                    build_error_message(src.file_path, src.line, src.column, u8"left-hand side of '>>' is not of a scalar integer type")};
                        }

                        if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"right-hand side of '>>' is not of a scalar integer type")};
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
                                            build_error_message(src.file_path, src.line, src.column, u8"right-hand side of '>>' is negative")};
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
                                            build_error_message(src.file_path, src.line, src.column, u8"right-hand side of '>>' is negative")};
                                }

                                e.int32 >>= shift;
                            } else {
                                e.int32 >>= rhs.uint32;
                            }
                        }

                        return {anton::expected_value, e};
                    }

                    case Binary_Expr_Type::add: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '+' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::sub: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '-' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::mul: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '*' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::div: {
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
                                    build_error_message(src.file_path, src.line, src.column,
                                                        u8"left-hand side of '/' is not of a scalar integer or a scalar floating-point type")};
                        }

                        if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                           rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Binary_Expr_Type::mod: {
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
                                    build_error_message(src.file_path, src.line, src.column, u8"left-hand side of '%' is not of a scalar integer type")};
                        }

                        if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.rhs->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"right-hand side of '%' is not of a scalar integer type")};
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

                    default: {
                        Source_Info const& src = expression.source_info;
                        return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"non-constant expression")};
                    }
                }
            }

            case AST_Node_Type::unary_expression: {
                Unary_Expression& expr = (Unary_Expression&)expression;
                switch(expr.type) {
                    case Unary_Type::plus: {
                        anton::Expected<Expr_Value, anton::String> base = evaluate_const_expr(ctx, *expr.expression);
                        return base;
                    }

                    case Unary_Type::minus: {
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
                                    build_error_message(src.file_path, src.line, src.column,
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

                    case Unary_Type::logic_not: {
                        anton::Expected<Expr_Value, anton::String> base_res = evaluate_const_expr(ctx, *expr.expression);
                        if(!base_res) {
                            return base_res;
                        }

                        Expr_Value base = base_res.value();
                        if(base.type != Expr_Value_Type::boolean && !is_implicitly_convertible_to_boolean(base.type)) {
                            Source_Info const& src = expr.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"right-hand side of '!' is not a boolean")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = !base.as_boolean();
                        return {anton::expected_value, e};
                    }

                    case Unary_Type::bit_not: {
                        anton::Expected<Expr_Value, anton::String> base_res = evaluate_const_expr(ctx, *expr.expression);
                        if(!base_res) {
                            return {anton::expected_error, ANTON_MOV(base_res.error())};
                        }

                        // TODO: Extend to work on integer vectors.

                        Expr_Value base = base_res.value();
                        if(base.type != Expr_Value_Type::uint32 && base.type != Expr_Value_Type::int32) {
                            Source_Info const& src = expr.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"right-hand side of '~' is not a scalar integer")};
                        }

                        Expr_Value e;
                        if(base.type == Expr_Value_Type::uint32) {
                            e.type = Expr_Value_Type::uint32;
                            e.uint32 = !base.uint32;
                        } else {
                            e.type = Expr_Value_Type::int32;
                            e.int32 = !base.int32;
                        }
                        return {anton::expected_value, e};
                    }
                }
            }

                // case AST_Node_Type::member_access_expression: {}

                // case AST_Node_Type::array_access_expression: {}

            default: {
                Source_Info const& src = expression.source_info;
                return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"non-constant expression")};
            }
        }
    }
} // namespace vush
