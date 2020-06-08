#include <const_expr_eval.hpp>

#include <utility.hpp>

namespace vush {
    bool is_implicitly_convertible_to_boolean(Expr_Value_Type) {
        // Both i32 and boolean are convertible to bool.
        return true;
    }

    Expected<Const_Expr_Value, String> evaluate_expr(Context& ctx, Expression& expression) {
        switch(expression.node_type) {
            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& expr = (Identifier_Expression&)expression;
                auto iter = ctx.global_symbols.find(expr.identifier->identifier);
                if(iter == ctx.global_symbols.end()) {
                    std::string msg = u8"unknown identifier '" + expr.identifier->identifier + u8"'";
                    return {expected_error, build_error_message(*ctx.current_file, 0, 0, msg)};
                }

                Symbol const& symbol = iter->second;
                if(symbol.type != Symbol_Type::constant) {
                    std::string msg = u8"identifier '" + expr.identifier->identifier + u8"' does not name a constant";
                    return {expected_error, build_error_message(*ctx.current_file, 0, 0, msg)};
                }

                Constant_Declaration* decl = (Constant_Declaration*)symbol.declaration;
                return evaluate_expr(ctx, *decl->initializer);
            }

                // case AST_Node_Type::assignment_expression: {}

                // case AST_Node_Type::arithmetic_assignment_expression: {}

            case AST_Node_Type::logic_or_expr: {
                Logic_Or_Expr& expr = (Logic_Or_Expr&)expression;
                Expected<Const_Expr_Value, String> lhs_res = evaluate_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return {expected_error, std::move(lhs_res.error())};
                }

                if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                    return {expected_error, build_error_message(*ctx.current_file, 0, 0, u8"expression is not implicitly convertible to bool")};
                }

                Expected<Const_Expr_Value, String> rhs_res = evaluate_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return {expected_error, std::move(rhs_res.error())};
                }

                if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                    return {expected_error, build_error_message(*ctx.current_file, 0, 0, u8"expression is not implicitly convertible to bool")};
                }

                bool lhs_val = lhs_res.value().as_boolean();
                bool rhs_val = rhs_res.value().as_boolean();
                Const_Expr_Value e;
                e.type = Expr_Value_Type::boolean;
                e.boolean = lhs_val || rhs_val;
                return {expected_value, e};
            }

            case AST_Node_Type::logic_xor_expr: {
                Logic_Or_Expr& expr = (Logic_Or_Expr&)expression;
                Expected<Const_Expr_Value, String> lhs_res = evaluate_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return {expected_error, std::move(lhs_res.error())};
                }

                if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                    return {expected_error, build_error_message(*ctx.current_file, 0, 0, u8"expression is not implicitly convertible to bool")};
                }

                Expected<Const_Expr_Value, String> rhs_res = evaluate_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return {expected_error, std::move(rhs_res.error())};
                }

                if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                    return {expected_error, build_error_message(*ctx.current_file, 0, 0, u8"expression is not implicitly convertible to bool")};
                }

                bool lhs_val = lhs_res.value().as_boolean();
                bool rhs_val = rhs_res.value().as_boolean();
                Const_Expr_Value e;
                e.type = Expr_Value_Type::boolean;
                e.boolean = (lhs_val && !rhs_val) || (!lhs_val && rhs_val);
                return {expected_value, e};
            }

            case AST_Node_Type::logic_and_expr: {
                Logic_Or_Expr& expr = (Logic_Or_Expr&)expression;
                Expected<Const_Expr_Value, String> lhs_res = evaluate_expr(ctx, *expr.lhs);
                if(!lhs_res) {
                    return {expected_error, std::move(lhs_res.error())};
                }

                if(!is_implicitly_convertible_to_boolean(lhs_res.value().type)) {
                    return {expected_error, build_error_message(*ctx.current_file, 0, 0, u8"expression is not implicitly convertible to bool")};
                }

                Expected<Const_Expr_Value, String> rhs_res = evaluate_expr(ctx, *expr.rhs);
                if(!rhs_res) {
                    return {expected_error, std::move(rhs_res.error())};
                }

                if(!is_implicitly_convertible_to_boolean(rhs_res.value().type)) {
                    return {expected_error, build_error_message(*ctx.current_file, 0, 0, u8"expression is not implicitly convertible to bool")};
                }

                bool lhs_val = lhs_res.value().as_boolean();
                bool rhs_val = rhs_res.value().as_boolean();
                Const_Expr_Value e;
                e.type = Expr_Value_Type::boolean;
                e.boolean = lhs_val && rhs_val;
                return {expected_value, e};
            }

                // case AST_Node_Type::relational_equality_expression: {}

                // case AST_Node_Type::relational_expression: {}

                // case AST_Node_Type::bit_or_expr: {}

                // case AST_Node_Type::bit_xor_expr: {}

                // case AST_Node_Type::bit_and_expr: {}

                // case AST_Node_Type::lshift_expr: {}

                // case AST_Node_Type::rshift_expr: {}

                // case AST_Node_Type::add_expr: {}

                // case AST_Node_Type::sub_expr: {}

                // case AST_Node_Type::mul_expr: {}

                // case AST_Node_Type::div_expr: {}

                // case AST_Node_Type::mod_expr: {}

                // case AST_Node_Type::unary_expression: {}

                // case AST_Node_Type::prefix_inc_expr: {}

                // case AST_Node_Type::prefix_dec_expr: {}

                // case AST_Node_Type::function_call_expression: {}

                // case AST_Node_Type::member_access_expression: {}

                // case AST_Node_Type::array_access_expression: {}

                // case AST_Node_Type::postfix_inc_expr: {}

                // case AST_Node_Type::postfix_dec_expr: {}

            case AST_Node_Type::bool_literal: {
                Bool_Literal& expr = (Bool_Literal&)expression;
                Const_Expr_Value e;
                e.type = Expr_Value_Type::int32;
                e.boolean = expr.value;
                return {expected_value, e};
            }

            case AST_Node_Type::integer_literal: {
                Integer_Literal& expr = (Integer_Literal&)expression;
                i64 value = str_to_i64(expr.value);
                Const_Expr_Value e;
                e.type = Expr_Value_Type::int32;
                e.int32 = value;
                return {expected_value, e};
            }

                // case AST_Node_Type::float_literal: {}

            default: {
                return {expected_error, build_error_message(*ctx.current_file, 0, 0, u8"non-constant expression")};
            }
        }
    }
} // namespace vush
