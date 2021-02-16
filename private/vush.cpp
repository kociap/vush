#include <vush/vush.hpp>

#include <anton/algorithm.hpp>
#include <anton/filesystem.hpp>
#include <anton/iterators.hpp>
#include <anton/string_stream.hpp>
#include <codegen.hpp>
#include <const_expr_eval.hpp>
#include <context.hpp>
#include <diagnostics.hpp>
#include <parser.hpp>

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
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }

                if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                    Source_Info const& src = node.condition->source_info;
                    return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                }

                if(result.value().as_boolean()) {
                    if(node.true_param) {
                        params[i] = ANTON_MOV(node.true_param);
                    } else {
                        params.erase(params.begin() + i, params.begin() + i + 1);
                    }
                } else {
                    if(node.false_param) {
                        params[i] = ANTON_MOV(node.false_param);
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
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }

                if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                    Source_Info const& src = node.condition->source_info;
                    return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                }

                if(result.value().as_boolean()) {
                    if(node.true_param) {
                        params[i] = ANTON_MOV(node.true_param);
                    } else {
                        params.erase(params.begin() + i, params.begin() + i + 1);
                    }
                } else {
                    if(node.false_param) {
                        params[i] = ANTON_MOV(node.false_param);
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

    // validate_function_attributes
    // Validate function attributes present on the function according to the following requirements:
    //  - compute stage might have the workgroup attribute (at most 1)
    //  - other stages must not have any attributes
    //  - ordinary functions must not have any attributes
    //
    static anton::Expected<void, anton::String> validate_function_attributes([[maybe_unused]] Context& ctx, Function_Declaration const& fn) {
        for(auto& attribute: fn.attributes) {
            Source_Info const& src = attribute->source_info;
            return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"illegal attribute")};
        }

        return {anton::expected_value};
    }

    static anton::Expected<void, anton::String> validate_function_attributes([[maybe_unused]] Context& ctx, Pass_Stage_Declaration const& fn) {
        switch(fn.stage) {
            case Stage_Type::compute: {
                bool has_workgroup = false;
                for(auto& attribute: fn.attributes) {
                    if(attribute->node_type == AST_Node_Type::workgroup_attribute) {
                        if(!has_workgroup) {
                            has_workgroup = true;
                        } else {
                            Source_Info const& src = attribute->source_info;
                            return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"duplicate workgroup attribute")};
                        }
                    } else {
                        Source_Info const& src = attribute->source_info;
                        return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"illegal attribute")};
                    }
                }
            } break;

            default: {
                for(auto& attribute: fn.attributes) {
                    Source_Info const& src = attribute->source_info;
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"illegal attribute")};
                }
            } break;
        }

        return {anton::expected_value};
    }

    static anton::Expected<void, anton::String> process_expression(Context& ctx, Owning_Ptr<Expression>& expression) {
        // TODO: Add other expression types
        switch(expression->node_type) {
            case AST_Node_Type::integer_literal: {
                Owning_Ptr<Integer_Literal>& node = (Owning_Ptr<Integer_Literal>&)expression;
                // Section 4.1.3 of The OpenGL Shading Language 4.60.7 states that integer literals must require at most 32 bits.
                switch(node->base) {
                    case Integer_Literal_Base::hex: {
                        // The max number of digits in a 32 bit hexadecimal number is 8, which corresponds to 0xFFFFFFFF (excluding the prefix)
                        if(node->value.size_bytes() > 8) {
                            return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                        } else {
                            return {anton::expected_value};
                        }
                    } break;

                    case Integer_Literal_Base::oct: {
                        // The max number of digits in a 32 bit octal number is 13, which corresponds to 0777777777777
                        if(node->value.size_bytes() > 13) {
                            return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                        } else {
                            // The max allowed value is 0377777777777, which corresponds to 4294967295
                            i64 const v = anton::str_to_i64(node->value, 8);
                            if(v <= 4294967295) {
                                return {anton::expected_value};
                            } else {
                                return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                            }
                        }
                    } break;

                    case Integer_Literal_Base::dec: {
                        // The max number of digits in a 32 bit decimal number is 10, which corresponds to 9999999999
                        if(node->value.size_bytes() > 13) {
                            return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                        } else {
                            // The max allowed value is 4294967295
                            i64 const v = anton::str_to_i64(node->value);
                            if(v <= 4294967295) {
                                return {anton::expected_value};
                            } else {
                                return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                            }
                        }
                    } break;
                }
                return {anton::expected_value};
            }

            case AST_Node_Type::identifier_expression: {
                Owning_Ptr<Identifier_Expression>& node = (Owning_Ptr<Identifier_Expression>&)expression;
                Symbol const* const symbol = find_symbol(ctx, node->identifier->value);
                if(!symbol) {
                    return {anton::expected_error, format_undefined_symbol(ctx, node->identifier->source_info)};
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::assignment_expression: {
                Owning_Ptr<Assignment_Expression>& node = (Owning_Ptr<Assignment_Expression>&)expression;
                anton::Expected<void, anton::String> lhs_res = process_expression(ctx, node->lhs);
                if(!lhs_res) {
                    return lhs_res;
                }

                anton::Expected<void, anton::String> rhs_res = process_expression(ctx, node->rhs);
                if(!rhs_res) {
                    return rhs_res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::arithmetic_assignment_expression: {
                Owning_Ptr<Arithmetic_Assignment_Expression>& node = (Owning_Ptr<Arithmetic_Assignment_Expression>&)expression;
                anton::Expected<void, anton::String> lhs_res = process_expression(ctx, node->lhs);
                if(!lhs_res) {
                    return lhs_res;
                }

                anton::Expected<void, anton::String> rhs_res = process_expression(ctx, node->rhs);
                if(!rhs_res) {
                    return rhs_res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::elvis_expr: {
                Owning_Ptr<Elvis_Expr>& node = (Owning_Ptr<Elvis_Expr>&)expression;
                anton::Expected<void, anton::String> condition_res = process_expression(ctx, node->condition);
                if(!condition_res) {
                    return condition_res;
                }

                anton::Expected<void, anton::String> true_expr_res = process_expression(ctx, node->true_expr);
                if(!true_expr_res) {
                    return true_expr_res;
                }

                anton::Expected<void, anton::String> false_expr_res = process_expression(ctx, node->false_expr);
                if(!false_expr_res) {
                    return false_expr_res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::unary_expression: {
                Owning_Ptr<Unary_Expression>& node = (Owning_Ptr<Unary_Expression>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::prefix_inc_expr: {
                Owning_Ptr<Prefix_Inc_Expr>& node = (Owning_Ptr<Prefix_Inc_Expr>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::prefix_dec_expr: {
                Owning_Ptr<Prefix_Dec_Expr>& node = (Owning_Ptr<Prefix_Dec_Expr>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::postfix_inc_expr: {
                Owning_Ptr<Postfix_Inc_Expr>& node = (Owning_Ptr<Postfix_Inc_Expr>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::postfix_dec_expr: {
                Owning_Ptr<Postfix_Dec_Expr>& node = (Owning_Ptr<Postfix_Dec_Expr>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::function_call_expression: {
                Owning_Ptr<Function_Call_Expression>& node = (Owning_Ptr<Function_Call_Expression>&)expression;
                for(Owning_Ptr<Expression>& arg: node->arg_list->arguments) {
                    anton::Expected<void, anton::String> res = process_expression(ctx, arg);
                    if(!res) {
                        return res;
                    }
                }

                Symbol const* const symbol = find_symbol(ctx, node->identifier->value);
                if(!symbol) {
                    return {anton::expected_error, format_undefined_symbol(ctx, node->identifier->source_info)};
                }

                if(symbol->type != Symbol_Type::function) {
                    return {anton::expected_error, format_called_symbol_does_not_name_function(ctx, node->identifier->source_info)};
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::member_access_expression: {
                Owning_Ptr<Member_Access_Expression>& node = (Owning_Ptr<Member_Access_Expression>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->base);
                if(!res) {
                    return res;
                }

                // TODO: Validate member exists

                return {anton::expected_value};
            }

            case AST_Node_Type::array_access_expression: {
                Owning_Ptr<Array_Access_Expression>& node = (Owning_Ptr<Array_Access_Expression>&)expression;
                anton::Expected<void, anton::String> base_res = process_expression(ctx, node->base);
                if(!base_res) {
                    return base_res;
                }

                anton::Expected<void, anton::String> index_res = process_expression(ctx, node->index);
                if(!index_res) {
                    return index_res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::paren_expr: {
                Owning_Ptr<Paren_Expr>& node = (Owning_Ptr<Paren_Expr>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::binary_expr: {
                Owning_Ptr<Binary_Expr>& node = (Owning_Ptr<Binary_Expr>&)expression;
                if(anton::Expected<void, anton::String> lhs = process_expression(ctx, node->lhs); !lhs) {
                    return lhs;
                }

                if(anton::Expected<void, anton::String> rhs = process_expression(ctx, node->rhs); !rhs) {
                    return rhs;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::expression_if: {
                Owning_Ptr<Expression_If>& node = (Owning_Ptr<Expression_If>&)expression;
                anton::Expected<void, anton::String> expr_res = process_expression(ctx, node->condition);
                if(!expr_res) {
                    return expr_res;
                }

                anton::Expected<bool, anton::String> compiletime_res = is_compiletime_evaluable(ctx, *node->condition);
                if(!compiletime_res) {
                    return {anton::expected_error, ANTON_MOV(compiletime_res.error())};
                }

                anton::Expected<Expr_Value, anton::String> result = evaluate_const_expr(ctx, *node->condition);
                if(!result) {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }

                if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                    Source_Info const& src = node->condition->source_info;
                    return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                }

                if(result.value().as_boolean()) {
                    expression = ANTON_MOV(node->true_expr);
                } else {
                    if(node->false_expr->node_type == AST_Node_Type::expression_if) {
                        anton::Expected<void, anton::String> res = process_expression(ctx, node->false_expr);
                        if(!res) {
                            return res;
                        }
                    }

                    expression = ANTON_MOV(node->false_expr);
                }
                return {anton::expected_value};
            }

                // TODO: Add reinterpret_expr

            default:
                return {anton::expected_value};
        }
    }

    static anton::Expected<void, anton::String> process_statements(Context& ctx, anton::Array<Owning_Ptr<Statement>>& statements) {
        // We push new scope and pop it only at the end of the function. We do not pop the scope when we fail
        // because an error always leads to termination.
        push_scope(ctx);

        for(i64 i = 0; i < statements.size();) {
            bool should_advance = true;
            switch(statements[i]->node_type) {
                case AST_Node_Type::declaration_statement: {
                    Declaration_Statement& node = (Declaration_Statement&)*statements[i];
                    ANTON_ASSERT(node.declaration->node_type == AST_Node_Type::variable_declaration ||
                                     node.declaration->node_type == AST_Node_Type::constant_declaration,
                                 u8"invalid ast node type");
                    if(node.declaration->node_type == AST_Node_Type::variable_declaration) {
                        Variable_Declaration& decl = (Variable_Declaration&)*node.declaration;
                        Symbol symbol{Symbol_Type::variable, &decl};
                        add_symbol(ctx, decl.identifier->value, symbol);
                        if(decl.initializer) {
                            anton::Expected<void, anton::String> res = process_expression(ctx, decl.initializer);
                            if(!res) {
                                return res;
                            }
                        }
                    } else {
                        Constant_Declaration& decl = (Constant_Declaration&)*node.declaration;
                        Symbol symbol{Symbol_Type::variable, &decl};
                        add_symbol(ctx, decl.identifier->value, symbol);
                        if(decl.initializer) {
                            anton::Expected<void, anton::String> res = process_expression(ctx, decl.initializer);
                            if(!res) {
                                return res;
                            }
                        }
                    }
                } break;

                case AST_Node_Type::block_statement: {
                    Block_Statement& node = (Block_Statement&)*statements[i];
                    anton::Expected<void, anton::String> res = process_statements(ctx, node.statements);
                    if(!res) {
                        return res;
                    }
                } break;

                case AST_Node_Type::if_statement: {
                    Owning_Ptr<If_Statement>& node = (Owning_Ptr<If_Statement>&)statements[i];
                    anton::Expected<void, anton::String> expr_res = process_expression(ctx, node->condition);
                    if(!expr_res) {
                        return expr_res;
                    }

                    anton::Expected<bool, anton::String> compiletime_res = is_compiletime_evaluable(ctx, *node->condition);
                    if(!compiletime_res) {
                        return {anton::expected_error, ANTON_MOV(compiletime_res.error())};
                    }

                    if(compiletime_res.value()) {
                        should_advance = false;

                        anton::Expected<Expr_Value, anton::String> eval_res = evaluate_const_expr(ctx, *node->condition);
                        if(!eval_res) {
                            return {anton::expected_error, ANTON_MOV(eval_res.error())};
                        }

                        if(!is_implicitly_convertible_to_boolean(eval_res.value().type)) {
                            Source_Info const& src = node->condition->source_info;
                            return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                        }

                        // We have to keep the if_statement object around until we no longer need the branch statements
                        Owning_Ptr owner{node.release()};
                        // Remove the if_statement node from ast and insert the statements corresponding to the true or false branch
                        statements.erase(statements.begin() + i, statements.begin() + i + 1);
                        Statement_List& insert_statements = (eval_res.value().as_boolean() ? owner->true_statements : owner->false_statements);
                        anton::Move_Iterator begin{insert_statements.begin()};
                        anton::Move_Iterator end{insert_statements.end()};
                        statements.insert(i, begin, end);
                    } else {
                        if(anton::Expected<void, anton::String> res = process_statements(ctx, node->true_statements); !res) {
                            return res;
                        }

                        if(anton::Expected<void, anton::String> res = process_statements(ctx, node->false_statements); !res) {
                            return res;
                        }
                    }
                } break;

                case AST_Node_Type::for_statement: {
                    For_Statement& node = (For_Statement&)*statements[i];
                    if(node.declaration) {
                        anton::Expected<void, anton::String> res = process_expression(ctx, node.declaration->initializer);
                        if(!res) {
                            return res;
                        }
                    }

                    if(node.condition) {
                        anton::Expected<void, anton::String> res = process_expression(ctx, node.condition);
                        if(!res) {
                            return res;
                        }
                    }

                    if(node.post_expression) {
                        anton::Expected<void, anton::String> res = process_expression(ctx, node.post_expression);
                        if(!res) {
                            return res;
                        }
                    }

                    anton::Expected<void, anton::String> res = process_statements(ctx, node.statements);
                    if(!res) {
                        return res;
                    }
                } break;

                case AST_Node_Type::while_statement: {
                    While_Statement& node = (While_Statement&)*statements[i];
                    anton::Expected<void, anton::String> cond_res = process_expression(ctx, node.condition);
                    if(!cond_res) {
                        return cond_res;
                    }

                    anton::Expected<void, anton::String> res = process_statements(ctx, node.statements);
                    if(!res) {
                        return res;
                    }
                } break;

                case AST_Node_Type::do_while_statement: {
                    Do_While_Statement& node = (Do_While_Statement&)*statements[i];
                    anton::Expected<void, anton::String> cond_res = process_expression(ctx, node.condition);
                    if(!cond_res) {
                        return cond_res;
                    }

                    anton::Expected<void, anton::String> res = process_statements(ctx, node.statements);
                    if(!res) {
                        return res;
                    }
                } break;

                case AST_Node_Type::switch_statement: {
                    Switch_Statement& node = (Switch_Statement&)*statements[i];
                    anton::Expected<void, anton::String> switch_expr_res = process_expression(ctx, node.match_expr);
                    if(!switch_expr_res) {
                        return switch_expr_res;
                    }

                    for(auto& switch_case: node.cases) {
                        ANTON_ASSERT(switch_case->node_type == AST_Node_Type::case_statement || switch_case->node_type == AST_Node_Type::default_case_statement,
                                     u8"invalid ast node type");
                        if(switch_case->node_type == AST_Node_Type::case_statement) {
                            Case_Statement& s = (Case_Statement&)*switch_case;
                            // Ensure that the label is an integer literal
                            anton::Expected<void, anton::String> label_res = process_expression(ctx, s.condition);
                            if(!label_res) {
                                return label_res;
                            }

                            if(s.condition->node_type != AST_Node_Type::integer_literal) {
                                Source_Info const& src = s.condition->source_info;
                                return {anton::expected_error,
                                        build_error_message(src.file_path, src.line, src.column, u8"case label is not an integer literal")};
                            }

                            anton::Expected<void, anton::String> res = process_statements(ctx, s.statements);
                            if(!res) {
                                return res;
                            }
                        } else {
                            Default_Case_Statement& s = (Default_Case_Statement&)*switch_case;
                            anton::Expected<void, anton::String> res = process_statements(ctx, s.statements);
                            if(!res) {
                                return res;
                            }
                        }
                    }

                } break;

                case AST_Node_Type::return_statement: {
                    Return_Statement& node = (Return_Statement&)*statements[i];
                    if(node.return_expr) {
                        anton::Expected<void, anton::String> res = process_expression(ctx, node.return_expr);
                        if(!res) {
                            return res;
                        }
                    }
                } break;

                case AST_Node_Type::expression_statement: {
                    Expression_Statement& node = (Expression_Statement&)*statements[i];
                    anton::Expected<void, anton::String> res = process_expression(ctx, node.expr);
                    if(!res) {
                        return res;
                    }
                } break;

                default:
                    break;
            }

            i += should_advance;
        }

        pop_scope(ctx);
        return {anton::expected_value};
    }

    // process_functions
    // Validates function attributes, processes parameter lists and function bodies resolving any compiletime ifs.
    // Does not add any symbols to the top level scope.
    //
    static anton::Expected<void, anton::String> process_functions(Context& ctx, Declaration_List& ast) {
        for(auto& ast_node: ast) {
            if(ast_node->node_type == AST_Node_Type::function_declaration) {
                Function_Declaration& fn = static_cast<Function_Declaration&>(*ast_node);
                if(anton::Expected<void, anton::String> res = validate_function_attributes(ctx, fn); !res) {
                    return res;
                }

                if(anton::Expected<void, anton::String> res = process_fn_param_list(ctx, fn); !res) {
                    return res;
                }

                if(anton::Expected<void, anton::String> res = process_statements(ctx, fn.body); !res) {
                    return res;
                }
            } else if(ast_node->node_type == AST_Node_Type::pass_stage_declaration) {
                Pass_Stage_Declaration& fn = static_cast<Pass_Stage_Declaration&>(*ast_node);
                // Validate the return types
                switch(fn.stage) {
                    case Stage_Type::compute: {
                        // Return type of a compute stage must always be void
                        Type& return_type = *fn.return_type;
                        bool const return_type_is_void =
                            return_type.node_type == AST_Node_Type::builtin_type && ((Builtin_Type&)return_type).type == Builtin_GLSL_Type::glsl_void;
                        if(!return_type_is_void) {
                            Source_Info const& src = return_type.source_info;
                            return {anton::expected_error, format_compute_return_type_must_be_void(ctx, src)};
                        }
                    } break;

                    default:
                        // TODO: Add symbol lookup to ensure the return types actually exist
                        break;
                }

                if(anton::Expected<void, anton::String> res = validate_function_attributes(ctx, fn); !res) {
                    return res;
                }

                if(anton::Expected<void, anton::String> res = process_fn_param_list(ctx, fn); !res) {
                    return res;
                }

                if(anton::Expected<void, anton::String> res = process_statements(ctx, fn.body); !res) {
                    return res;
                }
            }
        }

        return {anton::expected_value};
    }

    // resolve_imports_and_declaration_ifs
    // Processes top-level ast to resolve import declarations and declaration ifs. Modifies ast.
    //
    static anton::Expected<void, anton::String> resolve_imports_and_declaration_ifs(Context& ctx, Declaration_List& ast) {
        // TODO: Currently constants cannot be used in declaration ifs because they are not added to the symbol table as
        //       we process the ast. Should we allow constants in declaration ifs?
        for(i64 i = 0; i < ast.size();) {
            bool const should_advance = ast[i]->node_type != AST_Node_Type::import_decl && ast[i]->node_type != AST_Node_Type::declaration_if;
            if(ast[i]->node_type == AST_Node_Type::import_decl) {
                Owning_Ptr<Import_Decl> node{static_cast<Import_Decl*>(ast[i].release())};
                ast.erase(ast.begin() + i, ast.begin() + i + 1);

                anton::Expected<Source_Request_Result, anton::String> source_request_res =
                    ctx.source_request_cb(node->path->value, ctx.source_request_user_data);
                if(!source_request_res) {
                    return {anton::expected_error, format_source_import_failed(ctx, *node, source_request_res.error())};
                }

                Source_Request_Result& request_res = source_request_res.value();
                // Ensure we're not importing the same source multiple times
                auto iter = anton::find_if(ctx.imported_sources.begin(), ctx.imported_sources.end(),
                                           [&source_name = request_res.source_name](Owning_Ptr<anton::String> const& v) { return *v == source_name; });
                if(iter == ctx.imported_sources.end()) {
                    ctx.source_registry.emplace(request_res.source_name, request_res.data);
                    anton::Input_String_Stream stream{ANTON_MOV(request_res.data)};
                    Owning_Ptr<anton::String> const& current_source_name =
                        ctx.imported_sources.emplace_back(Owning_Ptr{new anton::String{ANTON_MOV(request_res.source_name)}});
                    anton::Expected<Declaration_List, Parse_Error> parse_result = parse_source(stream, *current_source_name);
                    if(!parse_result) {
                        Parse_Error const& error = parse_result.error();
                        anton::String error_msg = build_error_message(*current_source_name, error.line, error.column, error.message);
                        return {anton::expected_error, ANTON_MOV(error_msg)};
                    }

                    // Insert the result of parsing into the ast
                    Declaration_List& decls = parse_result.value();
                    anton::Move_Iterator begin(decls.begin());
                    anton::Move_Iterator end(decls.end());
                    ast.insert(i, begin, end);
                }
            } else if(ast[i]->node_type == AST_Node_Type::declaration_if) {
                Owning_Ptr<Declaration_If> node{static_cast<Declaration_If*>(ast[i].release())};
                ast.erase(ast.begin() + i, ast.begin() + i + 1);

                anton::Expected<Expr_Value, anton::String> result = evaluate_const_expr(ctx, *node->condition);
                if(!result) {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }

                if(!is_implicitly_convertible_to_boolean(result.value().type)) {
                    Source_Info const& src = node->condition->source_info;
                    return {anton::expected_error, format_expression_not_implicitly_convertible_to_bool(ctx, src)};
                }

                // Insert one of the branches into the ast
                Declaration_List& decls = (result.value().as_boolean() ? node->true_declarations : node->false_declarations);
                anton::Move_Iterator begin(decls.begin());
                anton::Move_Iterator end(decls.end());
                ast.insert(i, begin, end);
            }

            i += should_advance;
        }

        return {anton::expected_value};
    }

    static anton::Expected<void, anton::String> process_objects_and_settings_and_populate_symbol_table(Context& ctx, Declaration_List& ast,
                                                                                                       anton::Array<Pass_Settings>& settings) {
        for(auto& ast_node: ast) {
            switch(ast_node->node_type) {
                case AST_Node_Type::struct_decl: {
                    Struct_Decl& node = static_cast<Struct_Decl&>(*ast_node);
                    if(node.members.size() == 0) {
                        return {anton::expected_error, format_empty_struct(node.source_info)};
                    }

                    ctx.symbols[0].emplace(node.name->value, Symbol{Symbol_Type::struct_decl, &node});
                } break;

                case AST_Node_Type::variable_declaration: {
                    Variable_Declaration& node = static_cast<Variable_Declaration&>(*ast_node);
                    return {anton::expected_error, format_variable_declaration_in_global_scope(ctx, node.source_info)};
                } break;

                case AST_Node_Type::constant_declaration: {
                    Constant_Declaration& node = static_cast<Constant_Declaration&>(*ast_node);
                    ctx.symbols[0].emplace(node.identifier->value, Symbol{Symbol_Type::constant, &node});
                    if(!node.initializer) {
                        return {anton::expected_error, format_constant_missing_initializer(ctx, node.source_info)};
                    }

                    if(anton::Expected<void, anton::String> res = process_expression(ctx, node.initializer); !res) {
                        return res;
                    }
                } break;

                case AST_Node_Type::function_declaration: {
                    Function_Declaration& node = static_cast<Function_Declaration&>(*ast_node);
                    ctx.symbols[0].emplace(node.name->value, Symbol{Symbol_Type::function, &node});
                } break;

                case AST_Node_Type::settings_decl: {
                    Settings_Decl& decl = static_cast<Settings_Decl&>(*ast_node);
                    Pass_Settings* pass_iter = anton::find_if(
                        settings.begin(), settings.end(), [&pass_name = decl.pass_name->value](Pass_Settings const& v) { return v.pass_name == pass_name; });
                    if(pass_iter == settings.end()) {
                        Pass_Settings& v = settings.emplace_back(Pass_Settings{decl.pass_name->value, {}});
                        pass_iter = &v;
                    }

                    anton::Array<Setting_Key_Value>& pass_settings = pass_iter->settings;
                    // N^2 loop to overwrite duplicates in the order of occurence
                    for(Setting_Key_Value const& kv_new: decl.settings) {
                        auto end = pass_settings.end();
                        auto i = anton::find_if(pass_settings.begin(), end, [&kv_new](Setting_Key_Value const& v) { return kv_new.key == v.key; });
                        if(i != end) {
                            i->value = kv_new.value;
                        } else {
                            pass_settings.emplace_back(kv_new);
                        }
                    }
                } break;
            }
        }

        return {anton::expected_value};
    }

    // build_ast_from_sources
    // Parse sources, process imports and declaration ifs, build top-level symbol table, validate functions, fold constants, extract settings
    //
    static anton::Expected<Declaration_List, anton::String> build_ast_from_sources(Context& ctx, anton::String const& path,
                                                                                   anton::Array<Pass_Settings>& settings) {
        Declaration_List ast;
        // Parse the entry source
        {
            anton::Expected<Source_Request_Result, anton::String> source_request_res = ctx.source_request_cb(path, ctx.source_request_user_data);
            if(!source_request_res) {
                return {anton::expected_error, u8"error: " + source_request_res.error()};
            }

            Source_Request_Result& request_res = source_request_res.value();
            ctx.source_registry.emplace(request_res.source_name, request_res.data);
            anton::Input_String_Stream stream{ANTON_MOV(request_res.data)};
            Owning_Ptr<anton::String> const& current_source_name =
                ctx.imported_sources.emplace_back(Owning_Ptr{new anton::String{ANTON_MOV(request_res.source_name)}});
            anton::Expected<Declaration_List, Parse_Error> parse_result = parse_source(stream, *current_source_name);
            if(parse_result) {
                ast = ANTON_MOV(parse_result.value());
            } else {
                Parse_Error const& error = parse_result.error();
                anton::String error_msg = build_error_message(path, error.line, error.column, error.message);
                return {anton::expected_error, ANTON_MOV(error_msg)};
            }
        }

        if(anton::Expected<void, anton::String> res = resolve_imports_and_declaration_ifs(ctx, ast); !res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        if(anton::Expected<void, anton::String> res = process_objects_and_settings_and_populate_symbol_table(ctx, ast, settings); !res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        if(anton::Expected<void, anton::String> res = process_functions(ctx, ast); !res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        return {anton::expected_value, ANTON_MOV(ast)};
    } // namespace vush

    static void populate_builtin_glsl_variables(Context& ctx, anton::Array<Owning_Ptr<Declaration>>& storage) {
        struct Builtin_Variable {
            anton::String_View name;
            Builtin_GLSL_Type type;
        };

        Builtin_Variable builtin_variables[] = {// Vertex Shader
                                                {"gl_VertexID", Builtin_GLSL_Type::glsl_int},
                                                {"gl_InstanceID", Builtin_GLSL_Type::glsl_int},
                                                {"gl_VertexIndex", Builtin_GLSL_Type::glsl_int},
                                                {"gl_InstanceIndex", Builtin_GLSL_Type::glsl_int},
                                                {"gl_DrawID", Builtin_GLSL_Type::glsl_int},
                                                {"gl_BaseVertex", Builtin_GLSL_Type::glsl_int},
                                                {"gl_BaseInstance", Builtin_GLSL_Type::glsl_int},
                                                {"gl_Position", Builtin_GLSL_Type::glsl_vec4},
                                                // TODO: Add vertex shader gl_PointSize, gl_ClipDistance, gl_CullDistance
                                                // TODO: Add tessellation shader variables and geometry shader variables
                                                // Fragment Shader
                                                {"gl_FragCoord", Builtin_GLSL_Type::glsl_vec4},
                                                {"gl_FrontFacing", Builtin_GLSL_Type::glsl_bool},
                                                // TODO: Add fragment shader gl_ClipDistance, gl_CullDistance
                                                {"gl_PointCoord", Builtin_GLSL_Type::glsl_vec2},
                                                {"gl_PrimitiveID", Builtin_GLSL_Type::glsl_int},
                                                {"gl_SampleID", Builtin_GLSL_Type::glsl_int},
                                                {"gl_SamplePosition", Builtin_GLSL_Type::glsl_vec2},
                                                // TODO: Add fragment shader gl_SampleMaskIn
                                                {"gl_Layer", Builtin_GLSL_Type::glsl_int},
                                                {"gl_ViewportIndex", Builtin_GLSL_Type::glsl_int},
                                                {"gl_HelperInvocation", Builtin_GLSL_Type::glsl_int},
                                                {"gl_FragDepth", Builtin_GLSL_Type::glsl_float},
                                                // TODO: Add fragment shader gl_SampleMask
                                                // Compute Shader
                                                {"gl_NumWorkGroups", Builtin_GLSL_Type::glsl_uvec3},
                                                {"gl_WorkgroupSize", Builtin_GLSL_Type::glsl_uvec3},
                                                {"gl_WorkGroupID", Builtin_GLSL_Type::glsl_uvec3},
                                                {"gl_LocalInvocationID", Builtin_GLSL_Type::glsl_uvec3},
                                                {"gl_GlobalInvocationID", Builtin_GLSL_Type::glsl_uvec3},
                                                {"gl_LocalInvocationIndex", Builtin_GLSL_Type::glsl_uint}};
        // Populate storage and symbols
        constexpr i64 variable_count = sizeof(builtin_variables) / sizeof(Builtin_Variable);
        for(i64 i = 0; i < variable_count; ++i) {
            Builtin_Variable const& var = builtin_variables[i];
            Variable_Declaration* decl = new Variable_Declaration(Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_int, {u8"<GLSL Builtin>", 0, 0, 0})},
                                                                  Owning_Ptr{new Identifier(anton::String(var.name), {u8"<GLSL Builtin>", 0, 0, 0})}, nullptr,
                                                                  {u8"<GLSL Builtin>", 0, 0, 0});
            Symbol symbol{Symbol_Type::variable, decl};
            ctx.symbols[0].emplace(var.name, symbol);
            storage.emplace_back(decl);
        }
    }

    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config, source_request_callback callback, void* user_data) {
        Context ctx = {};
        ctx.source_request_cb = callback;
        ctx.source_request_user_data = user_data;
        ctx.source_definition_cb = config.source_definition_cb;
        ctx.source_definition_user_data = config.source_definition_user_data;
        // Add global scope
        ctx.symbols.emplace_back();
        // Create symbols for the constant defines passed via config
        anton::Array<Owning_Ptr<Declaration>> constant_decls;
        for(Constant_Define const& define: config.defines) {
            Symbol symbol;
            symbol.type = Symbol_Type::constant;
            Constant_Declaration* decl = new Constant_Declaration(Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_int, {config.source_name, 0, 0, 0})},
                                                                  Owning_Ptr{new Identifier(anton::String(define.name), {config.source_name, 0, 0, 0})},
                                                                  Owning_Ptr{new Integer_Literal(anton::to_string(define.value), Integer_Literal_Type::i32,
                                                                                                 Integer_Literal_Base::dec, {config.source_name, 0, 0, 0})},
                                                                  {config.source_name, 0, 0, 0});
            symbol.declaration = decl;
            ctx.symbols[0].emplace(define.name, symbol);
            constant_decls.emplace_back(decl);
        }
        // Create symbols for the builtin glsl variables
        anton::Array<Owning_Ptr<Declaration>> builtin_variables;
        populate_builtin_glsl_variables(ctx, builtin_variables);

        anton::Array<Pass_Settings> settings;
        anton::Expected<Declaration_List, anton::String> parse_res = build_ast_from_sources(ctx, config.source_name, settings);
        if(!parse_res) {
            return {anton::expected_error, ANTON_MOV(parse_res.error())};
        }

        Declaration_List& ast = parse_res.value();
        anton::Expected<anton::Array<Pass_Data>, anton::String> codegen_res = generate_glsl(ctx, ast, config.format, config.extensions, settings);
        if(!codegen_res) {
            return {anton::expected_error, ANTON_MOV(codegen_res.error())};
        }

        return {anton::expected_value, Build_Result{ANTON_MOV(settings), ANTON_MOV(codegen_res.value())}};
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
                    out_path = ANTON_MOV(resolved_path);
                } else {
                    return {anton::expected_error, anton::String{u8"ambiguous import path"}};
                }
            }
        }

        if(found) {
            return {anton::expected_value, ANTON_MOV(out_path)};
        } else {
            return {anton::expected_error, anton::String{u8"could not resolve import path"}};
        }
    }

    static anton::Expected<Source_Request_Result, anton::String> file_read_callback(anton::String const& path, void* user_data) {
        anton::Slice<anton::String const>& import_directories = *reinterpret_cast<anton::Slice<anton::String const>*>(user_data);
        anton::Expected<anton::String, anton::String> res = resolve_import_path(path, import_directories);
        if(!res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        anton::fs::Input_File_Stream file;
        if(!file.open(res.value())) {
            return {anton::expected_error, u8"could not open \"" + res.value() + u8"\" for reading"};
        }

        file.seek(anton::Seek_Dir::end, 0);
        i64 size = file.tell();
        file.seek(anton::Seek_Dir::beg, 0);
        anton::String file_contents{anton::reserve, size};
        file_contents.force_size(size);
        file.read(file_contents.data(), size);
        return {anton::expected_value, Source_Request_Result{ANTON_MOV(res.value()), ANTON_MOV(file_contents)}};
    }

    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config) {
        anton::Slice<anton::String const> import_directories{config.import_directories};
        return compile_to_glsl(config, file_read_callback, &import_directories);
    }
} // namespace vush
