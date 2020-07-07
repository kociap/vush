#include <const_expr_eval.hpp>

#include <utility.hpp>

namespace vush {
    bool is_implicitly_convertible_to_boolean(Expr_Value_Type) {
        // bool, i32, f32 and f64 are all convertible to bool.
        return true;
    }

    anton::Expected<Expr_Value, anton::String> evaluate_const_expr(Context& ctx, Expression& expression) {
        switch(expression.node_type) {
            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& expr = (Identifier_Expression&)expression;
                Symbol* symbol = find_symbol(ctx, expr.identifier->identifier);
                if(!symbol) {
                    anton::String msg = u8"unknown identifier '" + expr.identifier->identifier + u8"'";
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.source_info.line, expr.source_info.column, msg)};
                }

                if(symbol->type != Symbol_Type::constant) {
                    anton::String msg = u8"identifier '" + expr.identifier->identifier + u8"' does not name a constant";
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.source_info.line, expr.source_info.column, msg)};
                }

                Constant_Declaration* decl = (Constant_Declaration*)symbol->declaration;
                return evaluate_const_expr(ctx, *decl->initializer);
            }

            case AST_Node_Type::logic_or_expr: {
                Logic_Or_Expr& expr = (Logic_Or_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"expression is not implicitly convertible to bool")};
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                       u8"expression is not implicitly convertible to bool")};
                }

                bool lhs_val = lhs_res.value().as_boolean();
                bool rhs_val = rhs_res.value().as_boolean();
                Expr_Value e;
                e.type = Expr_Value_Type::boolean;
                e.boolean = lhs_val || rhs_val;
                return {anton::expected_value, e};
            }

            case AST_Node_Type::logic_xor_expr: {
                Logic_Or_Expr& expr = (Logic_Or_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"expression is not implicitly convertible to bool")};
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                       u8"expression is not implicitly convertible to bool")};
                }

                bool lhs_val = lhs_res.value().as_boolean();
                bool rhs_val = rhs_res.value().as_boolean();
                Expr_Value e;
                e.type = Expr_Value_Type::boolean;
                e.boolean = (lhs_val && !rhs_val) || (!lhs_val && rhs_val);
                return {anton::expected_value, e};
            }

            case AST_Node_Type::logic_and_expr: {
                Logic_Or_Expr& expr = (Logic_Or_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"expression is not implicitly convertible to bool")};
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                       u8"expression is not implicitly convertible to bool")};
                }

                bool lhs_val = lhs_res.value().as_boolean();
                bool rhs_val = rhs_res.value().as_boolean();
                Expr_Value e;
                e.type = Expr_Value_Type::boolean;
                e.boolean = lhs_val && rhs_val;
                return {anton::expected_value, e};
            }

            case AST_Node_Type::relational_equality_expression: {
                Relational_Equality_Expression& expr = (Relational_Equality_Expression&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                // TODO: Extend to actual glsl behaviour of == and != operators

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                   lhs.type != Expr_Value_Type::int32) {
                    anton::String stringified_relational = expr.is_equality ? u8"==" : u8"!=";
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '" + stringified_relational +
                                                                           "' is not of a scalar integer or a scalar floating-point type")};
                }

                if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                   rhs.type != Expr_Value_Type::int32) {
                    anton::String stringified_relational = expr.is_equality ? u8"==" : u8"!=";
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                       u8"right-hand side of '" + stringified_relational +
                                                                           "' is not of a scalar integer or a scalar floating-point type")};
                }

                Expr_Value e;
                e.type = Expr_Value_Type::boolean;
                if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                    if(expr.is_equality) {
                        e.boolean = lhs.as_float64() == rhs.as_float64();
                        return {anton::expected_value, e};
                    } else {
                        e.boolean = lhs.as_float64() != rhs.as_float64();
                        return {anton::expected_value, e};
                    }
                } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                    if(expr.is_equality) {
                        e.boolean = lhs.as_float32() == rhs.as_float32();
                        return {anton::expected_value, e};
                    } else {
                        e.boolean = lhs.as_float32() != rhs.as_float32();
                        return {anton::expected_value, e};
                    }
                } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                    if(expr.is_equality) {
                        e.boolean = lhs.as_uint32() == rhs.as_uint32();
                        return {anton::expected_value, e};
                    } else {
                        e.boolean = lhs.as_uint32() != rhs.as_uint32();
                        return {anton::expected_value, e};
                    }
                } else {
                    if(expr.is_equality) {
                        e.boolean = lhs.as_int32() == rhs.as_int32();
                        return {anton::expected_value, e};
                    } else {
                        e.boolean = lhs.as_int32() != rhs.as_int32();
                        return {anton::expected_value, e};
                    }
                }
            }

            case AST_Node_Type::relational_expression: {
                Relational_Expression& expr = (Relational_Expression&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                   lhs.type != Expr_Value_Type::int32) {
                    anton::String stringified_relational;
                    switch(expr.type) {
                        case Relational_Type::greater_than:
                            stringified_relational = u8">";
                            break;
                        case Relational_Type::less_than:
                            stringified_relational = u8"<";
                            break;
                        case Relational_Type::greater_equal:
                            stringified_relational = u8">=";
                            break;
                        case Relational_Type::less_equal:
                            stringified_relational = u8"<=";
                            break;
                    }

                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '" + stringified_relational +
                                                                           "' is not of a scalar integer or a scalar floating-point type")};
                }

                if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                   rhs.type != Expr_Value_Type::int32) {
                    anton::String stringified_relational;
                    switch(expr.type) {
                        case Relational_Type::greater_than:
                            stringified_relational = u8">";
                            break;
                        case Relational_Type::less_than:
                            stringified_relational = u8"<";
                            break;
                        case Relational_Type::greater_equal:
                            stringified_relational = u8">=";
                            break;
                        case Relational_Type::less_equal:
                            stringified_relational = u8"<=";
                            break;
                    }

                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                       u8"right-hand side of '" + stringified_relational +
                                                                           "' is not of a scalar integer or a scalar floating-point type")};
                }

                Expr_Value e;
                e.type = Expr_Value_Type::boolean;
                if(lhs.type == Expr_Value_Type::float64 || rhs.type == Expr_Value_Type::float64) {
                    switch(expr.type) {
                        case Relational_Type::greater_than:
                            e.boolean = lhs.as_float64() > rhs.as_float64();
                            return {anton::expected_value, e};
                        case Relational_Type::less_than:
                            e.boolean = lhs.as_float64() < rhs.as_float64();
                            return {anton::expected_value, e};
                        case Relational_Type::greater_equal:
                            e.boolean = lhs.as_float64() >= rhs.as_float64();
                            return {anton::expected_value, e};
                        case Relational_Type::less_equal:
                            e.boolean = lhs.as_float64() <= rhs.as_float64();
                            return {anton::expected_value, e};
                    }
                } else if(lhs.type == Expr_Value_Type::float32 || rhs.type == Expr_Value_Type::float32) {
                    switch(expr.type) {
                        case Relational_Type::greater_than:
                            e.boolean = lhs.as_float32() > rhs.as_float32();
                            return {anton::expected_value, e};
                        case Relational_Type::less_than:
                            e.boolean = lhs.as_float32() < rhs.as_float32();
                            return {anton::expected_value, e};
                        case Relational_Type::greater_equal:
                            e.boolean = lhs.as_float32() >= rhs.as_float32();
                            return {anton::expected_value, e};
                        case Relational_Type::less_equal:
                            e.boolean = lhs.as_float32() <= rhs.as_float32();
                            return {anton::expected_value, e};
                    }
                } else if(lhs.type == Expr_Value_Type::uint32 || rhs.type == Expr_Value_Type::uint32) {
                    switch(expr.type) {
                        case Relational_Type::greater_than:
                            e.boolean = lhs.as_uint32() > rhs.as_uint32();
                            return {anton::expected_value, e};
                        case Relational_Type::less_than:
                            e.boolean = lhs.as_uint32() < rhs.as_uint32();
                            return {anton::expected_value, e};
                        case Relational_Type::greater_equal:
                            e.boolean = lhs.as_uint32() >= rhs.as_uint32();
                            return {anton::expected_value, e};
                        case Relational_Type::less_equal:
                            e.boolean = lhs.as_uint32() <= rhs.as_uint32();
                            return {anton::expected_value, e};
                    }
                } else {
                    switch(expr.type) {
                        case Relational_Type::greater_than:
                            e.boolean = lhs.as_int32() > rhs.as_int32();
                            return {anton::expected_value, e};
                        case Relational_Type::less_than:
                            e.boolean = lhs.as_int32() < rhs.as_int32();
                            return {anton::expected_value, e};
                        case Relational_Type::greater_equal:
                            e.boolean = lhs.as_int32() >= rhs.as_int32();
                            return {anton::expected_value, e};
                        case Relational_Type::less_equal:
                            e.boolean = lhs.as_int32() <= rhs.as_int32();
                            return {anton::expected_value, e};
                    }
                }
            }

                // case AST_Node_Type::bit_or_expr: {}

                // case AST_Node_Type::bit_xor_expr: {}

                // case AST_Node_Type::bit_and_expr: {}

            case AST_Node_Type::lshift_expr: {
                LShift_Expr& expr = (LShift_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return {anton::expected_error, anton::move(lhs_res.error())};
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return {anton::expected_error, anton::move(rhs_res.error())};
                }

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::uint32 && lhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '<<' is not of a scalar integer type")};
                }

                if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                       u8"right-hand side of '<<' is not of a scalar integer type")};
                }

                Expr_Value e;
                e.type = lhs.type;
                e.uint32 = lhs.uint32;
                if(rhs.type == Expr_Value_Type::int32) {
                    i32 shift = rhs.int32;
                    if(shift < 0) {
                        return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                           u8"right-hand side of '<<' is negative")};
                    }

                    e.uint32 <<= shift;
                } else {
                    e.uint32 <<= rhs.uint32;
                }

                return {anton::expected_value, e};
            }

            case AST_Node_Type::rshift_expr: {
                RShift_Expr& expr = (RShift_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::uint32 && lhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '>>' is not of a scalar integer type")};
                }

                if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                       u8"right-hand side of '>>' is not of a scalar integer type")};
                }

                Expr_Value e;
                e.type = lhs.type;
                if(lhs.type == Expr_Value_Type::int32) {
                    e.int32 = lhs.int32;
                    if(rhs.type == Expr_Value_Type::int32) {
                        i32 shift = rhs.int32;
                        if(shift < 0) {
                            return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                               u8"right-hand side of '>>' is negative")};
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
                            return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                               u8"right-hand side of '>>' is negative")};
                        }

                        e.int32 >>= shift;
                    } else {
                        e.int32 >>= rhs.uint32;
                    }
                }

                return {anton::expected_value, e};
            }

            case AST_Node_Type::add_expr: {
                Add_Expr& expr = (Add_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                // TODO: Expand to include vectors and matrices.

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                   lhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '+' is not of a scalar integer or a scalar floating-point type")};
                }

                if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                   rhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
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

            case AST_Node_Type::sub_expr: {
                Sub_Expr& expr = (Sub_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                // TODO: Expand to include vectors and matrices.

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                   lhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '-' is not of a scalar integer or a scalar floating-point type")};
                }

                if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                   rhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
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

            case AST_Node_Type::mul_expr: {
                Mul_Expr& expr = (Mul_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                // TODO: Expand to include vectors and matrices.

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                   lhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '*' is not of a scalar integer or a scalar floating-point type")};
                }

                if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                   rhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
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

            case AST_Node_Type::div_expr: {
                Div_Expr& expr = (Div_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                // TODO: Expand to include vectors and matrices.

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::float64 && lhs.type != Expr_Value_Type::float32 && lhs.type != Expr_Value_Type::uint32 &&
                   lhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '/' is not of a scalar integer or a scalar floating-point type")};
                }

                if(rhs.type != Expr_Value_Type::float64 && rhs.type != Expr_Value_Type::float32 && rhs.type != Expr_Value_Type::uint32 &&
                   rhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
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

            case AST_Node_Type::mod_expr: {
                Add_Expr& expr = (Add_Expr&)expression;
                anton::Expected<Expr_Value, anton::String> lhs_res = evaluate_const_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return anton::move(lhs_res);
                }

                anton::Expected<Expr_Value, anton::String> rhs_res = evaluate_const_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return anton::move(rhs_res);
                }

                // TODO: Expand to include integer vectors.

                Expr_Value lhs = lhs_res.value();
                Expr_Value rhs = rhs_res.value();
                if(lhs.type != Expr_Value_Type::uint32 && lhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.lhs->source_info.line, expr.lhs->source_info.column,
                                                                       u8"left-hand side of '%' is not of a scalar integer type")};
                }

                if(rhs.type != Expr_Value_Type::uint32 && rhs.type != Expr_Value_Type::int32) {
                    return {anton::expected_error, build_error_message(*ctx.current_file, expr.rhs->source_info.line, expr.rhs->source_info.column,
                                                                       u8"right-hand side of '%' is not of a scalar integer type")};
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

            case AST_Node_Type::unary_expression: {
                Unary_Expression& expr = (Unary_Expression&)expression;
                switch(expr.type) {
                    case Unary_Type::plus: {
                        anton::Expected<Expr_Value, anton::String> base = evaluate_const_expr(ctx, *expr.expression);
                        return anton::move(base);
                    }

                    case Unary_Type::minus: {
                        anton::Expected<Expr_Value, anton::String> base_res = evaluate_const_expr(ctx, *expr.expression);
                        if(!base_res) {
                            return anton::move(base_res);
                        }

                        // TODO: Extend to vectors and matrices.

                        Expr_Value base = base_res.value();
                        if(base.type != Expr_Value_Type::float64 && base.type != Expr_Value_Type::float32 && base.type != Expr_Value_Type::uint32 &&
                           base.type != Expr_Value_Type::int32) {
                            return {anton::expected_error,
                                    build_error_message(*ctx.current_file, expr.source_info.line, expr.source_info.column,
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
                            return anton::move(base_res);
                        }

                        Expr_Value base = base_res.value();
                        if(base.type != Expr_Value_Type::boolean && !is_implicitly_convertible_to_boolean(base.type)) {
                            return {anton::expected_error, build_error_message(*ctx.current_file, expr.source_info.line, expr.source_info.column,
                                                                               u8"right-hand side of '!' is not a boolean")};
                        }

                        Expr_Value e;
                        e.type = Expr_Value_Type::boolean;
                        e.boolean = !base.as_boolean();
                        return {anton::expected_value, e};
                    }

                    case Unary_Type::bit_not: {
                        anton::Expected<Expr_Value, anton::String> base_res = evaluate_const_expr(ctx, *expr.expression);
                        if(!base_res) {
                            return {anton::expected_error, anton::move(base_res.error())};
                        }

                        // TODO: Extend to work on integer vectors.

                        Expr_Value base = base_res.value();
                        if(base.type != Expr_Value_Type::uint32 && base.type != Expr_Value_Type::int32) {
                            return {anton::expected_error, build_error_message(*ctx.current_file, expr.source_info.line, expr.source_info.column,
                                                                               u8"right-hand side of '~' is not a scalar integer")};
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

            case AST_Node_Type::bool_literal: {
                Bool_Literal& expr = (Bool_Literal&)expression;
                Expr_Value e;
                e.type = Expr_Value_Type::int32;
                e.boolean = expr.value;
                return {anton::expected_value, e};
            }

            case AST_Node_Type::integer_literal: {
                Integer_Literal& expr = (Integer_Literal&)expression;
                i64 value = str_to_i64(expr.value);
                Expr_Value e;
                e.type = Expr_Value_Type::int32;
                e.int32 = value;
                return {anton::expected_value, e};
            }

                // case AST_Node_Type::float_literal: {}

            default: {
                return {anton::expected_error,
                        build_error_message(*ctx.current_file, expression.source_info.line, expression.source_info.column, u8"non-constant expression")};
            }
        }
    }
} // namespace vush
