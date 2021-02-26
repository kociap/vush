#include <vush/vush.hpp>

#include <anton/algorithm.hpp>
#include <anton/filesystem.hpp>
#include <anton/flat_hash_set.hpp>
#include <anton/iterators.hpp>
#include <anton/string_stream.hpp>
#include <codegen.hpp>
#include <const_expr_eval.hpp>
#include <context.hpp>
#include <diagnostics.hpp>
#include <parser.hpp>

namespace vush {
    // process_fn_param_list
    // Resolves any function parameter ifs and adds the parameters to the symbol table.
    // Validates that the following requirements are met:
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
            ANTON_ASSERT(param->node_type == AST_Node_Type::ordinary_function_param || param->node_type == AST_Node_Type::sourced_function_param ||
                             param->node_type == AST_Node_Type::vertex_input_param,
                         u8"unknown parameter type");
            if(param->node_type == AST_Node_Type::ordinary_function_param) {
                Ordinary_Function_Param& node = (Ordinary_Function_Param&)*param;
                add_symbol(ctx, node.identifier->value, &node);

                // TODO: Remove unsized array parameters from parameter list because they are global. We have them to only provide a symbol.
                // if(is_unsized_array(type)) {
                // }

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
                        return {anton::expected_error, format_ordinary_parameter_not_allowed_on_stage(ctx, src, Stage_Type::vertex)};
                    } else if(param->node_type == AST_Node_Type::sourced_function_param) {
                        Sourced_Function_Param& node = (Sourced_Function_Param&)*param;
                        add_symbol(ctx, node.identifier->value, &node);
                    } else if(param->node_type == AST_Node_Type::vertex_input_param) {
                        Vertex_Input_Param& node = (Vertex_Input_Param&)*param;
                        add_symbol(ctx, node.identifier->value, &node);

                        Type& type = *node.type;
                        // Vertex input parameters must not be arrays (we do not support them yet)
                        if(type.node_type == AST_Node_Type::array_type) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"vertex input parameters must not be arrays")};
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
                        add_symbol(ctx, node.identifier->value, &node);

                        // TODO: Is this validation correct?
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
                        Source_Info const& src = param->source_info;
                        if(param->node_type == AST_Node_Type::sourced_function_param) {
                            Sourced_Function_Param& node = (Sourced_Function_Param&)*param;
                            add_symbol(ctx, node.identifier->value, &node);
                        } else if(param->node_type == AST_Node_Type::ordinary_function_param) {
                            return {anton::expected_error, format_ordinary_parameter_not_allowed_on_stage(ctx, src, Stage_Type::fragment)};
                        } else if(param->node_type == AST_Node_Type::vertex_input_param) {
                            return {anton::expected_error, format_vertex_input_not_allowed_on_stage(ctx, src, Stage_Type::fragment)};
                        } else {
                            return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"unknown parameter type")};
                        }
                    }
                }
            } break;

            case Stage_Type::compute: {
                for(auto& param: params) {
                    Source_Info const& src = param->source_info;
                    if(param->node_type == AST_Node_Type::ordinary_function_param) {
                        return {anton::expected_error, format_ordinary_parameter_not_allowed_on_stage(ctx, src, Stage_Type::compute)};
                    } else if(param->node_type == AST_Node_Type::sourced_function_param) {
                        Sourced_Function_Param& node = (Sourced_Function_Param&)*param;
                        add_symbol(ctx, node.identifier->value, &node);
                    } else if(param->node_type == AST_Node_Type::vertex_input_param) {
                        return {anton::expected_error, format_vertex_input_not_allowed_on_stage(ctx, src, Stage_Type::compute)};
                    } else {
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

    [[nodiscard]] static bool is_builtin_function_name(anton::String_View const name) {
        static constexpr anton::String_View builtin_function_names[] = {
            "radians",
            "degrees",
            "sin",
            "cos",
            "tan",
            "asin",
            "acos",
            "atan",
            "sinh",
            "cosh",
            "tanh",
            "asinh",
            "acosh",
            "atanh",
            "pow",
            "exp",
            "log",
            "exp2",
            "log2",
            "sqrt",
            "inversesqrt",
            "abs",
            "sign",
            "floor",
            "trunc",
            "round",
            "roundEven",
            "ceil",
            "fract",
            "mod",
            "modf",
            "min",
            "max",
            "clamp",
            "mix",
            "step",
            "smoothstep",
            "isnan",
            "isinf",
            "floatBitsToInt",
            "floatBitsToUint",
            "intBitstoFloat",
            "uintBitsToFloat",
            "fma",
            "frexp",
            "ldexp",
            "length",
            "distance",
            "dot",
            "cross",
            "normalize",
            "faceforward",
            "reflect",
            "refract",
            "matrixCompMult",
            "outerProduct",
            "transpose",
            "determinant",
            "inverse",
            "lessThan",
            "lessThanEqual",
            "greaterThan",
            "greaterThanEqual",
            "equal",
            "notEqual",
            "any",
            "all",
            "not",
            "uaddCarry",
            "usubBorrow",
            "umulExtended",
            "imulExtended",
            "bitfieldExtract",
            "bitfieldInsert",
            "bitfieldReverse",
            "bitCount",
            "findLSB",
            "findMSB",
            "textureSize",
            "textureQueryLod",
            "textureQueryLevels",
            "textureSamples",
            "texture",
            "textureProj",
            "textureLod",
            "textureOffset",
            "texelFetch",
            "texelFetchOffset",
            "textureProjOffset",
            "textureLodOffset",
            "textureProjLod",
            "textureProjLodOffset",
            "textureGrad",
            "textureGradOffset",
            "textureProjGrad",
            "textureProjGradOffset",
            "textureGather",
            "textureGatherOffset",
            "texture1D",
            "texture1DProj",
            "texture1DLod",
            "texture1DLod",
            "texture1DProjLod",
            "texture2D",
            "texture2DProj",
            "texture2DLod",
            "texture2DProjLod",
            "texture3D",
            "texture3DProj",
            "texture3DLod",
            "texture3DProjLod",
            "textureCube",
            "textureCubeLod",
            "shadow1D",
            "shadow2D",
            "shadow1DProj",
            "shadow2dProj",
            "shadow1DLod",
            "shadow2DLod",
            "shadow1DProjLod",
            "shadow2DProjLod",
            "atomicCounterIncrement",
            "atomicCounterDecrement",
            "atomicCounter",
            "atomicCounterAdd",
            "atomicCounterSubtract",
            "atomicCounterMin",
            "atomicCounterMax",
            "atomicCounterAnd",
            "atomicCounterOr",
            "atomicCounterXor",
            "atomicCounterExchange",
            "atomicCounterCompSwap",
            "atomicAdd",
            "atomicMin",
            "atomicMax",
            "atomicAnd",
            "atomicOr",
            "atomicXor",
            "atomicExchange",
            "atomicCompSwap",
            "imageSize",
            "imageSamples",
            "imageLoad",
            "imageStore",
            "imageAtomicAdd",
            "imageAtomicMin",
            "imageAtomicMax",
            "imageAtomicAnd",
            "imageAtomicOr",
            "imageAtomicXor",
            "imageAtomicExchange",
            "imageAtomicCompSwap",
            "dFdx",
            "dFdy",
            "dFdxFine",
            "dFdyFine",
            "dFdxCoarse",
            "dFdyCoarse",
            "fwidth",
            "fwidthFine",
            "fwidthCoarse",
            "barrier",
        };

        constexpr i64 array_size = sizeof(builtin_function_names) / sizeof(anton::String_View);
        for(i64 i = 0; i < array_size; ++i) {
            if(builtin_function_names[i] == name) {
                return true;
            }
        }
        return false;
    }

    static anton::Expected<void, anton::String> process_expression(Context& ctx, Owning_Ptr<Expression>& expression) {
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
                for(Owning_Ptr<Expression>& arg: node->arguments) {
                    anton::Expected<void, anton::String> res = process_expression(ctx, arg);
                    if(!res) {
                        return res;
                    }
                }

                // If the identifier is a builtin glsl type, it's a constructor call
                if(anton::Optional<Builtin_GLSL_Type> res = enumify_builtin_glsl_type(node->identifier->value); res) {
                    return {anton::expected_value};
                }

                // TODO: Temporarily check whether it's the name of a builtin function
                if(is_builtin_function_name(node->identifier->value)) {
                    return {anton::expected_value};
                }

                // Otherwise we look up the symbol to verify that it exists
                Symbol const* const symbol = find_symbol(ctx, node->identifier->value);
                if(!symbol) {
                    return {anton::expected_error, format_undefined_symbol(ctx, node->identifier->source_info)};
                }

                if(symbol->node_type != Symbol_Type::struct_decl && symbol->node_type != Symbol_Type::function_declaration) {
                    // Not a user defined type constructor call and not a function call
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

    static anton::Expected<void, anton::String> process_statements(Context& ctx, Statement_List& statements) {
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
                        add_symbol(ctx, decl.identifier->value, &decl);
                        if(decl.initializer) {
                            anton::Expected<void, anton::String> res = process_expression(ctx, decl.initializer);
                            if(!res) {
                                return res;
                            }
                        }
                    } else {
                        Constant_Declaration& decl = (Constant_Declaration&)*node.declaration;
                        add_symbol(ctx, decl.identifier->value, &decl);
                        if(!decl.initializer) {
                            return {anton::expected_error, format_constant_missing_initializer(ctx, decl.source_info)};
                        }

                        if(anton::Expected<void, anton::String> res = process_expression(ctx, decl.initializer); !res) {
                            return res;
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
                    if(anton::Expected<void, anton::String> res = process_expression(ctx, node->condition); !res) {
                        return res;
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
                    push_scope(ctx);
                    if(node.declaration) {
                        add_symbol(ctx, node.declaration->identifier->value, node.declaration.get());
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

                    pop_scope(ctx);
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
                    anton::Expected<void, anton::String> res = process_expression(ctx, node.expression);
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

                // Push new scope for the function body and parameters
                push_scope(ctx);
                if(anton::Expected<void, anton::String> res = process_fn_param_list(ctx, fn); !res) {
                    return res;
                }

                if(anton::Expected<void, anton::String> res = process_statements(ctx, fn.body); !res) {
                    return res;
                }

                pop_scope(ctx);
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

                // Push new scope for the function body and parameters
                push_scope(ctx);
                if(anton::Expected<void, anton::String> res = process_fn_param_list(ctx, fn); !res) {
                    return res;
                }

                if(anton::Expected<void, anton::String> res = process_statements(ctx, fn.body); !res) {
                    return res;
                }

                pop_scope(ctx);
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
                    return {anton::expected_error, format_source_import_failed(ctx, node->source_info, source_request_res.error())};
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

                    ctx.symbols[0].emplace(node.identifier->value, &node);
                } break;

                case AST_Node_Type::variable_declaration: {
                    Variable_Declaration& node = static_cast<Variable_Declaration&>(*ast_node);
                    return {anton::expected_error, format_variable_declaration_in_global_scope(ctx, node.source_info)};
                } break;

                case AST_Node_Type::constant_declaration: {
                    Constant_Declaration& node = static_cast<Constant_Declaration&>(*ast_node);
                    ctx.symbols[0].emplace(node.identifier->value, &node);
                    if(!node.initializer) {
                        return {anton::expected_error, format_constant_missing_initializer(ctx, node.source_info)};
                    }

                    if(anton::Expected<void, anton::String> res = process_expression(ctx, node.initializer); !res) {
                        return res;
                    }
                } break;

                case AST_Node_Type::function_declaration: {
                    Function_Declaration& node = static_cast<Function_Declaration&>(*ast_node);
                    ctx.symbols[0].emplace(node.identifier->value, &node);
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

                default:
                    break;
            }
        }

        return {anton::expected_value};
    }

    // walk_nodes_and_aggregate_function_calls
    //
    static void walk_nodes_and_aggregate_function_calls(anton::Array<Function_Call_Expression*>& function_calls, AST_Node& node) {
        switch(node.node_type) {
            case AST_Node_Type::function_call_expression: {
                Function_Call_Expression& n = (Function_Call_Expression&)node;
                for(auto& arg: n.arguments) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *arg);
                }
                function_calls.emplace_back(&n);
            } break;

            case AST_Node_Type::variable_declaration: {
                Variable_Declaration& n = (Variable_Declaration&)node;
                if(n.initializer) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *n.initializer);
                }
            } break;

            case AST_Node_Type::constant_declaration: {
                Constant_Declaration& n = (Constant_Declaration&)node;
                if(n.initializer) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *n.initializer);
                }
            } break;

            case AST_Node_Type::function_declaration: {
                Function_Declaration& n = (Function_Declaration&)node;
                for(auto& statement: n.body) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::pass_stage_declaration: {
                Pass_Stage_Declaration& n = (Pass_Stage_Declaration&)node;
                for(auto& statement: n.body) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::assignment_expression: {
                Assignment_Expression& n = (Assignment_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.lhs);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.rhs);
            } break;

            case AST_Node_Type::arithmetic_assignment_expression: {
                Arithmetic_Assignment_Expression& n = (Arithmetic_Assignment_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.lhs);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.rhs);
            } break;

            case AST_Node_Type::elvis_expr: {
                Elvis_Expr& n = (Elvis_Expr&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.condition);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.true_expr);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.false_expr);
            } break;

            case AST_Node_Type::binary_expr: {
                Binary_Expr& n = (Binary_Expr&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.lhs);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.rhs);
            } break;

            case AST_Node_Type::unary_expression: {
                Unary_Expression& n = (Unary_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::prefix_inc_expr: {
                Prefix_Inc_Expr& n = (Prefix_Inc_Expr&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::prefix_dec_expr: {
                Prefix_Dec_Expr& n = (Prefix_Dec_Expr&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::array_access_expression: {
                Array_Access_Expression& n = (Array_Access_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.base);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.index);
            } break;

            case AST_Node_Type::postfix_inc_expr: {
                Postfix_Inc_Expr& n = (Postfix_Inc_Expr&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::postfix_dec_expr: {
                Postfix_Dec_Expr& n = (Postfix_Dec_Expr&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::paren_expr: {
                Paren_Expr& n = (Paren_Expr&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::reinterpret_expr: {
                Reinterpret_Expr& n = (Reinterpret_Expr&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.index);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.source);
            } break;

            case AST_Node_Type::block_statement: {
                Block_Statement& n = (Block_Statement&)node;
                for(auto& statement: n.statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::if_statement: {
                If_Statement& n = (If_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.condition);
                for(auto& statement: n.true_statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }

                for(auto& statement: n.false_statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::case_statement: {
                Case_Statement& n = (Case_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.condition);
                for(auto& statement: n.statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::default_case_statement: {
                Default_Case_Statement& n = (Default_Case_Statement&)node;
                for(auto& statement: n.statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::switch_statement: {
                Switch_Statement& n = (Switch_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.match_expr);
                for(auto& case_statement: n.cases) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *case_statement);
                }
            } break;

            case AST_Node_Type::for_statement: {
                For_Statement& n = (For_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.declaration);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.condition);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.post_expression);
                for(auto& statement: n.statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::while_statement: {
                While_Statement& n = (While_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.condition);
                for(auto& statement: n.statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::do_while_statement: {
                Do_While_Statement& n = (Do_While_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.condition);
                for(auto& statement: n.statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::return_statement: {
                Return_Statement& n = (Return_Statement&)node;
                if(n.return_expr) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *n.return_expr);
                }
            } break;

            case AST_Node_Type::declaration_statement: {
                Declaration_Statement& n = (Declaration_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.declaration);
            } break;

            case AST_Node_Type::expression_statement: {
                Expression_Statement& n = (Expression_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            default:
                break;
        }
    }

    struct Replacement_Rule {
        anton::String_View identifier;
        Expression* replacement;
    };

    static void replace_identifier_expressions(Owning_Ptr<AST_Node>& node, anton::Slice<Replacement_Rule const> replacements) {
        switch(node->node_type) {
            case AST_Node_Type::variable_declaration: {
                Owning_Ptr<Variable_Declaration>& n = (Owning_Ptr<Variable_Declaration>&)node;
                if(n->initializer) {
                    replace_identifier_expressions(n->initializer, replacements);
                }
            } break;

            case AST_Node_Type::constant_declaration: {
                Owning_Ptr<Constant_Declaration>& n = (Owning_Ptr<Constant_Declaration>&)node;
                if(n->initializer) {
                    replace_identifier_expressions(n->initializer, replacements);
                }
            } break;

            case AST_Node_Type::function_declaration: {
                Owning_Ptr<Function_Declaration>& n = (Owning_Ptr<Function_Declaration>&)node;
                for(auto& statement: n->body) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::pass_stage_declaration: {
                Owning_Ptr<Pass_Stage_Declaration>& n = (Owning_Ptr<Pass_Stage_Declaration>&)node;
                for(auto& statement: n->body) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::identifier_expression: {
                Owning_Ptr<Identifier_Expression>& n = (Owning_Ptr<Identifier_Expression>&)node;
                anton::String_View identifier = n->identifier->value;
                auto iter = anton::find_if(replacements.begin(), replacements.end(),
                                           [identifier](Replacement_Rule const& rule) { return rule.identifier == identifier; });
                if(iter != replacements.end()) {
                    node = iter->replacement->clone();
                }
            } break;

            case AST_Node_Type::assignment_expression: {
                Owning_Ptr<Assignment_Expression>& n = (Owning_Ptr<Assignment_Expression>&)node;
                replace_identifier_expressions(n->lhs, replacements);
                replace_identifier_expressions(n->rhs, replacements);
            } break;

            case AST_Node_Type::arithmetic_assignment_expression: {
                Owning_Ptr<Arithmetic_Assignment_Expression>& n = (Owning_Ptr<Arithmetic_Assignment_Expression>&)node;
                replace_identifier_expressions(n->lhs, replacements);
                replace_identifier_expressions(n->rhs, replacements);
            } break;

            case AST_Node_Type::elvis_expr: {
                Owning_Ptr<Elvis_Expr>& n = (Owning_Ptr<Elvis_Expr>&)node;
                replace_identifier_expressions(n->condition, replacements);
                replace_identifier_expressions(n->true_expr, replacements);
                replace_identifier_expressions(n->false_expr, replacements);
            } break;

            case AST_Node_Type::binary_expr: {
                Owning_Ptr<Binary_Expr>& n = (Owning_Ptr<Binary_Expr>&)node;
                replace_identifier_expressions(n->lhs, replacements);
                replace_identifier_expressions(n->rhs, replacements);
            } break;

            case AST_Node_Type::unary_expression: {
                Owning_Ptr<Unary_Expression>& n = (Owning_Ptr<Unary_Expression>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::prefix_inc_expr: {
                Owning_Ptr<Prefix_Inc_Expr>& n = (Owning_Ptr<Prefix_Inc_Expr>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::prefix_dec_expr: {
                Owning_Ptr<Prefix_Dec_Expr>& n = (Owning_Ptr<Prefix_Dec_Expr>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::member_access_expression: {
                Owning_Ptr<Member_Access_Expression>& n = (Owning_Ptr<Member_Access_Expression>&)node;
                replace_identifier_expressions(n->base, replacements);
            } break;

            case AST_Node_Type::array_access_expression: {
                Owning_Ptr<Array_Access_Expression>& n = (Owning_Ptr<Array_Access_Expression>&)node;
                replace_identifier_expressions(n->base, replacements);
                replace_identifier_expressions(n->index, replacements);
            } break;

            case AST_Node_Type::function_call_expression: {
                Owning_Ptr<Function_Call_Expression>& n = (Owning_Ptr<Function_Call_Expression>&)node;
                for(auto& arg: n->arguments) {
                    replace_identifier_expressions(arg, replacements);
                }
            } break;

            case AST_Node_Type::postfix_inc_expr: {
                Owning_Ptr<Postfix_Inc_Expr>& n = (Owning_Ptr<Postfix_Inc_Expr>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::postfix_dec_expr: {
                Owning_Ptr<Postfix_Dec_Expr>& n = (Owning_Ptr<Postfix_Dec_Expr>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::paren_expr: {
                Owning_Ptr<Paren_Expr>& n = (Owning_Ptr<Paren_Expr>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::reinterpret_expr: {
                Owning_Ptr<Reinterpret_Expr>& n = (Owning_Ptr<Reinterpret_Expr>&)node;
                replace_identifier_expressions(n->index, replacements);
                replace_identifier_expressions(n->source, replacements);
            } break;

            case AST_Node_Type::block_statement: {
                Owning_Ptr<Block_Statement>& n = (Owning_Ptr<Block_Statement>&)node;
                for(auto& statement: n->statements) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::if_statement: {
                Owning_Ptr<If_Statement>& n = (Owning_Ptr<If_Statement>&)node;
                replace_identifier_expressions(n->condition, replacements);
                for(auto& statement: n->true_statements) {
                    replace_identifier_expressions(statement, replacements);
                }

                for(auto& statement: n->false_statements) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::case_statement: {
                Owning_Ptr<Case_Statement>& n = (Owning_Ptr<Case_Statement>&)node;
                replace_identifier_expressions(n->condition, replacements);
                for(auto& statement: n->statements) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::default_case_statement: {
                Owning_Ptr<Default_Case_Statement>& n = (Owning_Ptr<Default_Case_Statement>&)node;
                for(auto& statement: n->statements) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::switch_statement: {
                Owning_Ptr<Switch_Statement>& n = (Owning_Ptr<Switch_Statement>&)node;
                replace_identifier_expressions(n->match_expr, replacements);
                for(auto& case_statement: n->cases) {
                    replace_identifier_expressions(case_statement, replacements);
                }
            } break;

            case AST_Node_Type::for_statement: {
                Owning_Ptr<For_Statement>& n = (Owning_Ptr<For_Statement>&)node;
                replace_identifier_expressions(n->declaration, replacements);
                replace_identifier_expressions(n->condition, replacements);
                replace_identifier_expressions(n->post_expression, replacements);
                for(auto& statement: n->statements) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::while_statement: {
                Owning_Ptr<While_Statement>& n = (Owning_Ptr<While_Statement>&)node;
                replace_identifier_expressions(n->condition, replacements);
                for(auto& statement: n->statements) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::do_while_statement: {
                Owning_Ptr<Do_While_Statement>& n = (Owning_Ptr<Do_While_Statement>&)node;
                replace_identifier_expressions(n->condition, replacements);
                for(auto& statement: n->statements) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::return_statement: {
                Owning_Ptr<Return_Statement>& n = (Owning_Ptr<Return_Statement>&)node;
                if(n->return_expr) {
                    replace_identifier_expressions(n->return_expr, replacements);
                }
            } break;

            case AST_Node_Type::declaration_statement: {
                Owning_Ptr<Declaration_Statement>& n = (Owning_Ptr<Declaration_Statement>&)node;
                replace_identifier_expressions(n->declaration, replacements);
            } break;

            case AST_Node_Type::expression_statement: {
                Owning_Ptr<Expression_Statement>& n = (Owning_Ptr<Expression_Statement>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            default:
                break;
        }
    }

    // perform_function_instantiations
    // Searches the entire ast for Function_Call_Expression nodes that have unsized array arguments
    // and creates instances of the corresponding functions with those parameters removed and
    // all corresponding identifiers replaced.
    // Removes unsized array arguments from the Function_Call_Expression nodes.
    //
    // IMPORTANT:
    // This function performs 2 transformations on the ast that do NOT preserve the symbol table:
    //  - Original functions are removed from the ast and replaced with instantiations.
    //  - Unsized array parameters are removed from the instantiated functions.
    // After calling this function it is not safe to lookup non-global or function symbols anymore!
    //
    static void perform_function_instantiations(Context& ctx, Declaration_List& ast) {
        anton::Array<Owning_Ptr<Function_Declaration>> functions;
        anton::Array<Function_Call_Expression*> function_calls;
        for(i64 i = 0; i < ast.size(); ++i) {
            Owning_Ptr<AST_Node>& node = ast[i];
            if(node->node_type == AST_Node_Type::function_declaration || node->node_type == AST_Node_Type::pass_stage_declaration) {
                walk_nodes_and_aggregate_function_calls(function_calls, *node);
            }

            if(node->node_type == AST_Node_Type::function_declaration) {
                Owning_Ptr<Function_Declaration>& fn = (Owning_Ptr<Function_Declaration>&)node;
                // Only functions with unsized array parameters must be removed and instantiated.
                // Check whether the function has anu unsized array parameters.
                bool requires_instantiation = false;
                for(auto& parameter: fn->params) {
                    Ordinary_Function_Param& param = (Ordinary_Function_Param&)*parameter;
                    if(is_unsized_array(*param.type)) {
                        requires_instantiation = true;
                        break;
                    }
                }

                if(requires_instantiation) {
                    // Pull the function out of the ast into a different storage.
                    // We don't want it to be codegened later on, but we need it for instantiation.
                    functions.emplace_back(ANTON_MOV(fn));
                    ast.erase(ast.begin() + i, ast.begin() + i + 1);
                    --i;
                }
            }
        }

        anton::Flat_Hash_Set<u64> instantiated_functions;
        for(i64 i = 0; i < function_calls.size(); ++i) {
            Function_Call_Expression& function_call = *function_calls[i];
            Symbol* symbol = find_symbol(ctx, function_call.identifier->value);
            // Builtin types do not have a symbol, but their constructors still produce function calls
            if(!symbol || symbol->node_type != Symbol_Type::function_declaration) {
                continue;
            }

            Function_Declaration& fn_template = (Function_Declaration&)*symbol;
            anton::String stringified_signature = stringify_type(*fn_template.return_type) + fn_template.identifier->value;
            anton::String instance_name = fn_template.identifier->value;
            // Check whether the function requires instantiation, aka has any unsized array parameters.
            // Stringify the signature and generate instance name.
            bool requires_instantiation = false;
            for(i64 i = 0; i < fn_template.params.size(); ++i) {
                Ordinary_Function_Param& param = (Ordinary_Function_Param&)*fn_template.params[i];
                stringified_signature += stringify_type(*param.type);
                if(is_unsized_array(*param.type)) {
                    ANTON_ASSERT(function_call.arguments[i]->node_type == AST_Node_Type::identifier_expression,
                                 "unsized array argument must be an identifier expression");
                    Identifier_Expression& expr = (Identifier_Expression&)*function_call.arguments[i];
                    instance_name += "_";
                    instance_name += expr.identifier->value;
                    requires_instantiation = true;
                }
            }

            if(!requires_instantiation) {
                continue;
            }

            // Rename the function call
            function_call.identifier->value = instance_name;

            // Guard against multiple instantiations
            u64 const signature_hash = anton::hash(stringified_signature);
            auto iter = instantiated_functions.find(signature_hash);
            if(iter != instantiated_functions.end()) {
                // Function already instantiated.
                // Remove the unsized array arguments from the function call.
                for(i64 i = 0, j = 0; i < fn_template.params.size(); ++i) {
                    Ordinary_Function_Param& param = (Ordinary_Function_Param&)*fn_template.params[i];
                    if(is_unsized_array(*param.type)) {
                        auto arg_begin = function_call.arguments.begin();
                        function_call.arguments.erase(arg_begin + j, arg_begin + j + 1);
                    } else {
                        ++j;
                    }
                }

                continue;
            }

            instantiated_functions.emplace(signature_hash);

            Owning_Ptr<Function_Declaration> instance = fn_template.clone();
            // Rename the instance
            instance->identifier->value = instance_name;
            // Build replacements table
            anton::Array<Replacement_Rule> replacements;
            for(i64 i = 0; i < instance->params.size(); ++i) {
                Ordinary_Function_Param& param = (Ordinary_Function_Param&)*instance->params[i];
                if(is_unsized_array(*param.type)) {
                    ANTON_ASSERT(function_call.arguments[i]->node_type == AST_Node_Type::identifier_expression,
                                 "unsized array argument must be an identifier expression");
                    Identifier_Expression* argument = (Identifier_Expression*)function_call.arguments[i].get();
                    replacements.emplace_back(param.identifier->value, argument);
                }
            }

            replace_identifier_expressions(instance, replacements);

            // Remove the unsized array parameters and arguments from the instance and the function call
            for(i64 i = 0; i < instance->params.size();) {
                Ordinary_Function_Param& param = (Ordinary_Function_Param&)*instance->params[i];
                if(is_unsized_array(*param.type)) {
                    auto arg_begin = function_call.arguments.begin();
                    function_call.arguments.erase(arg_begin + i, arg_begin + i + 1);
                    auto param_begin = instance->params.begin();
                    instance->params.erase(param_begin + i, param_begin + i + 1);
                } else {
                    ++i;
                }
            }

            walk_nodes_and_aggregate_function_calls(function_calls, *instance);
            ast.emplace_back(ANTON_MOV(instance));
        }
    }

    static void populate_builtin_glsl_variables(Context& ctx, Declaration_List& storage) {
        struct Builtin_Variable {
            anton::String_View name;
            Type* type;
        };

        Source_Info const src_info{u8"<GLSL Builtin>", 0, 0, 0};
        Builtin_Variable builtin_variables[] = {
            // Vertex Shader
            {"gl_VertexID", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_InstanceID", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_VertexIndex", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_InstanceIndex", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_DrawID", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_BaseVertex", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_BaseInstance", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_Position", new Builtin_Type(Builtin_GLSL_Type::glsl_vec4, src_info)},
            {"gl_PointSize", new Builtin_Type(Builtin_GLSL_Type::glsl_float, src_info)},
            {"gl_ClipDistance", new Array_Type(Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_float, src_info)}, nullptr, src_info)},
            {"gl_CullDistance", new Array_Type(Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_float, src_info)}, nullptr, src_info)},
            // TODO: Add tessellation shader variables and geometry shader variables
            // Fragment Shader
            {"gl_FragCoord", new Builtin_Type(Builtin_GLSL_Type::glsl_vec4, src_info)},
            {"gl_FrontFacing", new Builtin_Type(Builtin_GLSL_Type::glsl_bool, src_info)},
            // gl_ClipDistance, gl_CullDistance already declared above in the vertex shader section
            {"gl_PointCoord", new Builtin_Type(Builtin_GLSL_Type::glsl_vec2, src_info)},
            {"gl_PrimitiveID", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_SampleID", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_SamplePosition", new Builtin_Type(Builtin_GLSL_Type::glsl_vec2, src_info)},
            {"gl_SampleMaskIn", new Array_Type(Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)}, nullptr, src_info)},
            {"gl_Layer", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_ViewportIndex", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_HelperInvocation", new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)},
            {"gl_FragDepth", new Builtin_Type(Builtin_GLSL_Type::glsl_float, src_info)},
            {"gl_SampleMask", new Array_Type(Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_int, src_info)}, nullptr, src_info)},
            // Compute Shader
            {"gl_NumWorkGroups", new Builtin_Type(Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_WorkgroupSize", new Builtin_Type(Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_WorkGroupID", new Builtin_Type(Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_LocalInvocationID", new Builtin_Type(Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_GlobalInvocationID", new Builtin_Type(Builtin_GLSL_Type::glsl_uvec3, src_info)},
            {"gl_LocalInvocationIndex", new Builtin_Type(Builtin_GLSL_Type::glsl_uint, src_info)}};

        // Populate storage and symbols
        constexpr i64 variable_count = sizeof(builtin_variables) / sizeof(Builtin_Variable);
        for(i64 i = 0; i < variable_count; ++i) {
            Builtin_Variable const& var = builtin_variables[i];
            Variable_Declaration* decl =
                new Variable_Declaration(Owning_Ptr{var.type}, Owning_Ptr{new Identifier(anton::String(var.name), src_info)}, nullptr, src_info);
            ctx.symbols[0].emplace(var.name, decl);
            storage.emplace_back(decl);
        }
    }

    // build_ast_from_sources
    // Parse sources, process imports and declaration ifs, validate functions, fold constants, extract settings
    //
    [[nodiscard]] static anton::Expected<Declaration_List, anton::String> build_ast_from_sources(Context& ctx, anton::String const& path,
                                                                                                 anton::Array<Pass_Settings>& settings) {
        // Create symbols for the builtin glsl variables
        anton::Array<Owning_Ptr<Declaration>> builtin_variables;
        populate_builtin_glsl_variables(ctx, builtin_variables);

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

        perform_function_instantiations(ctx, ast);

        return {anton::expected_value, ANTON_MOV(ast)};
    }

    // aggregate
    // Aggregates data for codegen and extracts unsized and opaque Sourced_Function_Param out of stages.
    // Does not fill the specialized Sourced_Data buffers in Pass_Context's, i.e. variables, opaque_variables and unsized_variables.
    // Performs validation of stages ensuring there are no duplicate stages.
    //
    [[nodiscard]] static anton::Expected<void, anton::String> aggregate(Declaration_List& ast, anton::Array<Declaration*>& structs_and_constants,
                                                                        anton::Array<Function_Declaration*>& functions, anton::Array<Pass_Context>& passes,
                                                                        anton::Array<Owning_Ptr<Sourced_Function_Param>>& sourced_params) {
        for(auto& node: ast) {
            switch(node->node_type) {
                case AST_Node_Type::struct_decl:
                case AST_Node_Type::constant_declaration: {
                    Declaration* decl = node.get();
                    structs_and_constants.emplace_back(decl);
                } break;

                case AST_Node_Type::function_declaration: {
                    Function_Declaration* fn = (Function_Declaration*)node.get();
                    functions.emplace_back(fn);
                } break;

                case AST_Node_Type::pass_stage_declaration: {
                    Pass_Stage_Declaration* pass_decl = (Pass_Stage_Declaration*)node.get();
                    Pass_Context* pass =
                        anton::find_if(passes.begin(), passes.end(), [pass_decl](Pass_Context const& v) { return v.name == pass_decl->pass->value; });
                    if(pass == passes.end()) {
                        Pass_Context& v = passes.emplace_back(Pass_Context{pass_decl->pass->value, {}});
                        pass = &v;
                    }

                    // Ensure there is only 1 stage of each type
                    switch(pass_decl->stage) {
                        case Stage_Type::vertex: {
                            if(pass->vertex_stage) {
                                Source_Info const& src1 = pass->vertex_stage->source_info;
                                Source_Info const& src2 = pass_decl->source_info;
                                return {anton::expected_error, format_duplicate_pass_stage_error(src1, src2, pass->name, Stage_Type::vertex)};
                            }

                            pass->vertex_stage = pass_decl;
                        } break;

                        case Stage_Type::fragment: {
                            if(pass->fragment_stage) {
                                Source_Info const& src1 = pass->vertex_stage->source_info;
                                Source_Info const& src2 = pass_decl->source_info;
                                return {anton::expected_error, format_duplicate_pass_stage_error(src1, src2, pass->name, Stage_Type::fragment)};
                            }

                            pass->fragment_stage = pass_decl;
                        } break;

                        case Stage_Type::compute: {
                            if(pass->compute_stage) {
                                Source_Info const& src1 = pass->vertex_stage->source_info;
                                Source_Info const& src2 = pass_decl->source_info;
                                return {anton::expected_error, format_duplicate_pass_stage_error(src1, src2, pass->name, Stage_Type::compute)};
                            }

                            pass->compute_stage = pass_decl;
                        } break;
                    }

                    Parameter_List& params = pass_decl->params;
                    for(i64 i = 0; i < params.size(); ++i) {
                        Owning_Ptr<Function_Param>& node = params[i];
                        if(node->node_type == AST_Node_Type::sourced_function_param) {
                            Sourced_Function_Param& param = (Sourced_Function_Param&)*node;
                            auto iter = pass->sourced_data.find_or_emplace(param.source->value);
                            iter->value.all.emplace_back(&param);
                            if(is_unsized_array(*param.type) || is_opaque_type(*param.type)) {
                                sourced_params.emplace_back(ANTON_MOV((Owning_Ptr<Sourced_Function_Param>&)params[i]));
                                auto begin = params.begin();
                                params.erase(begin + i, begin + i + 1);
                                --i;
                            }
                        }
                    }
                } break;

                default:
                    break;
            }
        }

        return {anton::expected_value};
    }

    struct Layout_Info {
        i64 alignment;
        i64 size;
    };

    [[nodiscard]] static Layout_Info calculate_type_layout_info(Context const& ctx, Type const& type) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type ||
                         type.node_type == AST_Node_Type::array_type,
                     u8"unknown ast node type");
        if(type.node_type == AST_Node_Type::builtin_type) {
            Builtin_Type const& t = (Builtin_Type const&)type;
            switch(t.type) {
                case Builtin_GLSL_Type::glsl_void:
                    return {0, 0};

                case Builtin_GLSL_Type::glsl_bool:
                case Builtin_GLSL_Type::glsl_int:
                case Builtin_GLSL_Type::glsl_uint:
                case Builtin_GLSL_Type::glsl_float:
                    return {4, 4};

                case Builtin_GLSL_Type::glsl_double:
                    return {8, 8};

                case Builtin_GLSL_Type::glsl_vec2:
                case Builtin_GLSL_Type::glsl_bvec2:
                case Builtin_GLSL_Type::glsl_ivec2:
                case Builtin_GLSL_Type::glsl_uvec2:
                    return {8, 8};

                case Builtin_GLSL_Type::glsl_vec3:
                case Builtin_GLSL_Type::glsl_vec4:
                case Builtin_GLSL_Type::glsl_bvec3:
                case Builtin_GLSL_Type::glsl_bvec4:
                case Builtin_GLSL_Type::glsl_ivec3:
                case Builtin_GLSL_Type::glsl_ivec4:
                case Builtin_GLSL_Type::glsl_uvec3:
                case Builtin_GLSL_Type::glsl_uvec4:
                    return {16, 16};

                case Builtin_GLSL_Type::glsl_dvec2:
                    return {16, 16};

                case Builtin_GLSL_Type::glsl_dvec3:
                case Builtin_GLSL_Type::glsl_dvec4:
                    return {32, 32};

                case Builtin_GLSL_Type::glsl_mat2:
                case Builtin_GLSL_Type::glsl_mat2x3:
                case Builtin_GLSL_Type::glsl_mat2x4:
                    return {16, 32};

                case Builtin_GLSL_Type::glsl_mat3x2:
                case Builtin_GLSL_Type::glsl_mat3:
                case Builtin_GLSL_Type::glsl_mat3x4:
                    return {16, 48};

                case Builtin_GLSL_Type::glsl_mat4x2:
                case Builtin_GLSL_Type::glsl_mat4x3:
                case Builtin_GLSL_Type::glsl_mat4:
                    return {16, 64};

                case Builtin_GLSL_Type::glsl_dmat2:
                    return {16, 32};

                case Builtin_GLSL_Type::glsl_dmat2x3:
                case Builtin_GLSL_Type::glsl_dmat2x4:
                    return {32, 64};

                case Builtin_GLSL_Type::glsl_dmat3x2:
                    return {16, 48};

                case Builtin_GLSL_Type::glsl_dmat3:
                case Builtin_GLSL_Type::glsl_dmat3x4:
                    return {32, 96};

                case Builtin_GLSL_Type::glsl_dmat4x2:
                    return {16, 64};

                case Builtin_GLSL_Type::glsl_dmat4x3:
                case Builtin_GLSL_Type::glsl_dmat4:
                    return {32, 128};

                default:
                    return {0, 0};
            }
        } else if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& t = (User_Defined_Type const&)type;
            Symbol const* symbol = find_symbol(ctx, t.name);
            Struct_Decl const* struct_decl = (Struct_Decl const*)symbol;
            i64 max_alignment = 0;
            i64 offset = 0;
            for(auto& member: struct_decl->members) {
                Layout_Info const info = calculate_type_layout_info(ctx, *member->type);
                max_alignment = anton::math::max(max_alignment, info.alignment);
                // Realign offset if necessary
                i64 const misalignment = offset % info.alignment;
                if(misalignment != 0) {
                    offset += info.alignment - misalignment;
                }
                offset += info.size;
            }
            // Round the alignment up to a multiple of vec4's alignment
            i64 const alignment = ((max_alignment + 15) / 16) * 16;
            return {alignment, offset};
        } else if(type.node_type == AST_Node_Type::array_type) {
            Array_Type const& t = (Array_Type const&)type;
            i64 array_size = 0;
            if(!is_unsized_array(t)) {
                array_size = anton::str_to_i64(t.size->value);
            }
            Layout_Info const info = calculate_type_layout_info(ctx, *t.base);
            // Round the alignment up to a multiple of vec4's alignment
            i64 const alignment = ((info.alignment + 15) / 16) * 16;
            // If the adjusted alignment forces padding, add
            i64 const misalignment = info.size % alignment;
            i64 size = info.size;
            if(misalignment != 0) {
                size += alignment - misalignment;
            }
            return {alignment, size * array_size};
        } else {
            ANTON_UNREACHABLE();
        }
    }

    // validate_and_optimize_passes
    // Checks whether a pass has either a compute stage or a vertex stage and optionally a fragment stage.
    // Ensures there are no different-type-same-name parameters in any of the passes.
    // Removes same-type-same-name duplicates.
    // Copies Sourced_Data to specialized buffers (all to variables/opaque_variables/unsized_variables).
    // Optimizes layout of variables.
    //
    [[nodiscard]] static anton::Expected<void, anton::String> validate_and_optimize_passes(Context const& ctx, anton::Slice<Pass_Context> const passes) {
        for(Pass_Context& pass: passes) {
            // Validate that a pass has either a compute stage or a vertex stage and optionally a fragment stage
            if(!pass.compute_stage) {
                if(!pass.vertex_stage) {
                    return {anton::expected_error, format_missing_vertex_stage_error(pass.name)};
                }
            } else {
                if(pass.vertex_stage || pass.fragment_stage) {
                    return {anton::expected_error, format_vertex_and_compute_stages_error(pass.name)};
                }
            }

            // Remove duplicates, validate there is no different-type-same-name sourced parameters, optimize layout
            for(auto& [source_name, data]: pass.sourced_data) {
                // We use stable sort to preserve the order and report duplicates in the correct order
                anton::merge_sort(data.all.begin(), data.all.end(), [](Sourced_Function_Param const* lhs, Sourced_Function_Param const* rhs) {
                    anton::String const& lhs_str = lhs->identifier->value;
                    anton::String const& rhs_str = rhs->identifier->value;
                    return anton::compare(lhs_str, rhs_str) == -1;
                });

                // Ensure there are no name duplicates with different types
                for(auto i = data.all.begin(), j = data.all.begin() + 1, end = data.all.end(); j != end; ++i, ++j) {
                    Sourced_Function_Param const& i_param = **i;
                    Sourced_Function_Param const& j_param = **j;
                    anton::String const i_type = stringify_type(*i_param.type);
                    anton::String const j_type = stringify_type(*j_param.type);
                    if(i_param.identifier->value == j_param.identifier->value && i_type != j_type) {
                        return {anton::expected_error, format_duplicate_sourced_parameter(ctx, i_param.source_info, i_param.type->source_info,
                                                                                          j_param.source_info, j_param.type->source_info)};
                    }
                }

                // Remove type duplicates
                auto end = anton::unique(data.all.begin(), data.all.end(), [](Sourced_Function_Param const* lhs, Sourced_Function_Param const* rhs) {
                    anton::String const i_type = stringify_type(*lhs->type);
                    anton::String const j_type = stringify_type(*rhs->type);
                    return i_type == j_type && lhs->identifier->value == rhs->identifier->value;
                });

                data.all.erase(end, data.all.end());

                // Copy Sourced_Function_Param's to specialized buffers
                for(Sourced_Function_Param const* const p: data.all) {
                    bool const opaque = is_opaque_type(*p->type);
                    bool const unsized = is_unsized_array(*p->type);
                    if(!opaque && !unsized) {
                        data.variables.emplace_back(p);
                    } else if(opaque && !unsized) {
                        data.opaque_variables.emplace_back(p);
                    } else {
                        data.unsized_variables.emplace_back(p);
                    }
                }

                // Optimize the layout of the variables
                i64 const variables_count = data.variables.size();
                anton::Array<Layout_Info> layout_info{anton::reserve, variables_count};
                for(Sourced_Function_Param const* p: data.variables) {
                    Layout_Info info = calculate_type_layout_info(ctx, *p->type);
                    layout_info.emplace_back(info);
                }
                // Create a permutation that will sort by alignment
                anton::Array<i64> indices{variables_count, 0};
                anton::fill_with_consecutive(indices.begin(), indices.end(), 0);
                anton::quick_sort(indices.begin(), indices.end(),
                                  [&layout_info](i64 const lhs, i64 const rhs) { return layout_info[lhs].alignment > layout_info[rhs].alignment; });
                // Apply the permutation
                {
                    anton::Array<Sourced_Function_Param const*> perm_data{variables_count};
                    anton::Array<Layout_Info> perm_layout_info{variables_count};
                    for(i64 i = 0; i < variables_count; ++i) {
                        i64 const index = indices[i];
                        perm_data[i] = data.variables[index];
                        perm_layout_info[i] = layout_info[index];
                    }
                    data.variables = ANTON_MOV(perm_data);
                    layout_info = ANTON_MOV(perm_layout_info);
                }
            }
        }

        return {anton::expected_value};
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
            Constant_Declaration* decl = new Constant_Declaration(Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_int, {config.source_name, 0, 0, 0})},
                                                                  Owning_Ptr{new Identifier(anton::String(define.name), {config.source_name, 0, 0, 0})},
                                                                  Owning_Ptr{new Integer_Literal(anton::to_string(define.value), Integer_Literal_Type::i32,
                                                                                                 Integer_Literal_Base::dec, {config.source_name, 0, 0, 0})},
                                                                  {config.source_name, 0, 0, 0});
            ctx.symbols[0].emplace(define.name, decl);
            constant_decls.emplace_back(decl);
        }

        anton::Array<Pass_Settings> settings;
        anton::Expected<Declaration_List, anton::String> build_res = build_ast_from_sources(ctx, config.source_name, settings);
        if(!build_res) {
            return {anton::expected_error, ANTON_MOV(build_res.error())};
        }

        Declaration_List& ast = build_res.value();
        anton::Array<Function_Declaration*> functions;
        anton::Array<Declaration*> structs_and_constants;
        anton::Array<Pass_Context> passes;
        anton::Array<Owning_Ptr<Sourced_Function_Param>> sourced_params;
        if(anton::Expected<void, anton::String> res = aggregate(ast, structs_and_constants, functions, passes, sourced_params); !res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        if(anton::Expected<void, anton::String> res = validate_and_optimize_passes(ctx, passes); !res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        Codegen_Data codegen_data{config.extensions, settings, passes, functions, structs_and_constants};
        anton::Expected<anton::Array<Pass_Data>, anton::String> codegen_res = generate_glsl(ctx, config.format, codegen_data);
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
