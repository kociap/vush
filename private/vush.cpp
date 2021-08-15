#include <vush/vush.hpp>

#include <anton/algorithm.hpp>
#include <anton/filesystem.hpp>
#include <anton/flat_hash_set.hpp>
#include <anton/format.hpp>
#include <anton/iterators.hpp>
#include <anton/string_stream.hpp>
#include <codegen.hpp>
#include <const_expr_eval.hpp>
#include <context.hpp>
#include <diagnostics.hpp>
#include <parser.hpp>

namespace vush {
    using namespace anton::literals;

    // check_and_add_symbol
    // Checks whether a symbol already exists and if not, adds it to the symbol table.
    // Otherwise returns an error diagnostic.
    //
    static anton::Expected<void, anton::String> check_and_add_symbol(Context& ctx, anton::String_View name, Symbol* symbol) {
        Symbol* original_symbol = find_symbol(ctx, name);
        if(original_symbol != nullptr) {
            auto get_symbol_name = [](Symbol* symbol) -> Source_Info {
                switch(symbol->node_type) {
                    case Symbol_Type::variable_declaration: {
                        Variable_Declaration* v = (Variable_Declaration*)symbol;
                        return v->identifier->source_info;
                    }

                    case Symbol_Type::constant_declaration: {
                        Constant_Declaration* v = (Constant_Declaration*)symbol;
                        return v->identifier->source_info;
                    }

                    case Symbol_Type::function_parameter: {
                        Function_Parameter* v = (Function_Parameter*)symbol;
                        return v->identifier->source_info;
                    }

                    default:
                        ANTON_FAIL(false, "unknown symbol type");
                }
            };

            Source_Info const first_name = get_symbol_name(original_symbol);
            Source_Info const second_name = get_symbol_name(symbol);
            return {anton::expected_error, format_symbol_redefinition(ctx, first_name, second_name)};
        }

        add_symbol(ctx, name, symbol);
        return {anton::expected_value};
    }

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
        Parameter_List& params = function.params;
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
            ANTON_ASSERT(param->node_type == AST_Node_Type::function_parameter, u8"unknown parameter type");
            Function_Parameter& p = (Function_Parameter&)*param;
            if(is_sourced_parameter(p)) {
                Source_Info const& src = p.source_info;
                return {anton::expected_error,
                        build_error_message(src.file_path, src.line, src.column, u8"sourced parameters are not allowed on ordinary functions")};
            }

            if(is_vertex_input_parameter(p)) {
                Source_Info const& src = p.source_info;
                return {anton::expected_error,
                        build_error_message(src.file_path, src.line, src.column, u8"vertex input parameters are not allowed on ordinary functions")};
            }

            if(p.image_layout) {
                Source_Info const& qualifier_src = p.image_layout->source_info;
                Source_Info const& p_identifier_src = p.identifier->source_info;
                return {anton::expected_error, format_illegal_image_layout_qualifier_on_non_sourced_parameter(ctx, qualifier_src, p_identifier_src)};
            }

            anton::Expected<void, anton::String> symbol_res = check_and_add_symbol(ctx, p.identifier->value, &p);
            if(!symbol_res) {
                return symbol_res;
            }
        }

        return {anton::expected_value};
    }

    static anton::Expected<void, anton::String> process_fn_param_list(Context& ctx, Pass_Stage_Declaration& function) {
        Parameter_List& params = function.params;
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

        switch(function.stage_type) {
            case Stage_Type::vertex: {
                for(auto& param: params) {
                    ANTON_ASSERT(param->node_type == AST_Node_Type::function_parameter, u8"unknown parameter type");
                    Function_Parameter& p = (Function_Parameter&)*param;
                    if(!is_sourced_parameter(p)) {
                        Source_Info const& src = p.source_info;
                        return {anton::expected_error, format_ordinary_parameter_not_allowed_on_stage(ctx, src, Stage_Type::vertex)};
                    }

                    if(is_vertex_input_parameter(p)) {
                        Type& type = *p.type;
                        // Vertex input parameters must not be opaque types
                        if(is_opaque_type(type)) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"vertex input parameters must not be opaque types")};
                        }

                        // Vertex input parameters must not be arrays (we do not support them yet)
                        if(type.node_type == AST_Node_Type::array_type) {
                            Source_Info const& src = type.source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"vertex input parameters must not be arrays")};
                        }
                    }

                    if(p.image_layout) {
                        Type& type = *p.type;
                        if(!is_image_type(type)) {
                            Source_Info const& qualifier_src = p.image_layout->source_info;
                            Source_Info const& type_src = type.source_info;
                            return {anton::expected_error, format_illegal_image_layout_qualifier_on_non_image_type(ctx, qualifier_src, type_src)};
                        }
                    }

                    anton::Expected<void, anton::String> symbol_res = check_and_add_symbol(ctx, p.identifier->value, &p);
                    if(!symbol_res) {
                        return symbol_res;
                    }
                }
            } break;

            case Stage_Type::fragment: {
                bool const has_ordinary_parameter = params.size() > 0 && !is_sourced_parameter((Function_Parameter const&)*params[0]);
                if(has_ordinary_parameter) {
                    Function_Parameter& p = (Function_Parameter&)*params[0];

                    // TODO: Is this validation correct?
                    Type& type = *p.type;
                    // Stage input parameters that are arrays must be sized
                    if(is_unsized_array(type)) {
                        Source_Info const& src = type.source_info;
                        return {anton::expected_error,
                                build_error_message(src.file_path, src.line, src.column, u8"stage input parameters must not be unsized arrays")};
                    }

                    // Stage input parameters must not be opaque
                    if(is_opaque_type(type)) {
                        Source_Info const& src = type.source_info;
                        return {anton::expected_error,
                                build_error_message(src.file_path, src.line, src.column,
                                                    u8"stage input parameters must be of non-opaque type (non-opaque builtin type or user "
                                                    u8"defined type) or an array of non-opaque type")};
                    }

                    if(p.image_layout) {
                        Source_Info const& qualifier_src = p.image_layout->source_info;
                        Source_Info const& p_identifier_src = p.identifier->source_info;
                        return {anton::expected_error, format_illegal_image_layout_qualifier_on_non_sourced_parameter(ctx, qualifier_src, p_identifier_src)};
                    }

                    anton::Expected<void, anton::String> symbol_res = check_and_add_symbol(ctx, p.identifier->value, &p);
                    if(!symbol_res) {
                        return symbol_res;
                    }
                }

                for(i64 i = has_ordinary_parameter; i < params.size(); ++i) {
                    Function_Parameter& p = (Function_Parameter&)*params[i];
                    if(is_vertex_input_parameter(p)) {
                        Source_Info const& src = p.source_info;
                        return {anton::expected_error, format_vertex_input_not_allowed_on_stage(ctx, src, Stage_Type::fragment)};
                    }

                    if(!is_sourced_parameter(p)) {
                        Source_Info const& src = p.source_info;
                        return {anton::expected_error, format_ordinary_parameter_not_allowed_on_stage(ctx, src, Stage_Type::fragment)};
                    }

                    if(p.image_layout) {
                        Type& type = *p.type;
                        if(!is_image_type(type)) {
                            Source_Info const& qualifier_src = p.image_layout->source_info;
                            Source_Info const& type_src = type.source_info;
                            return {anton::expected_error, format_illegal_image_layout_qualifier_on_non_image_type(ctx, qualifier_src, type_src)};
                        }
                    }

                    anton::Expected<void, anton::String> symbol_res = check_and_add_symbol(ctx, p.identifier->value, &p);
                    if(!symbol_res) {
                        return symbol_res;
                    }
                }
            } break;

            case Stage_Type::compute: {
                for(auto& param: params) {
                    Function_Parameter& p = (Function_Parameter&)*param;
                    if(is_vertex_input_parameter(p)) {
                        Source_Info const& src = p.source_info;
                        return {anton::expected_error, format_vertex_input_not_allowed_on_stage(ctx, src, Stage_Type::fragment)};
                    }

                    if(!is_sourced_parameter(p)) {
                        Source_Info const& src = p.source_info;
                        return {anton::expected_error, format_ordinary_parameter_not_allowed_on_stage(ctx, src, Stage_Type::fragment)};
                    }

                    if(p.image_layout) {
                        Type& type = *p.type;
                        if(!is_image_type(type)) {
                            Source_Info const& qualifier_src = p.image_layout->source_info;
                            Source_Info const& type_src = type.source_info;
                            return {anton::expected_error, format_illegal_image_layout_qualifier_on_non_image_type(ctx, qualifier_src, type_src)};
                        }
                    }

                    anton::Expected<void, anton::String> symbol_res = check_and_add_symbol(ctx, p.identifier->value, &p);
                    if(!symbol_res) {
                        return symbol_res;
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
        switch(fn.stage_type) {
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
                    case Integer_Literal_Base::dec: {
                        anton::String const& value = node->value;
                        // The max number of digits in a 32 bit decimal number is 10, which corresponds to 9999999999
                        if(value.size_bytes() > 10) {
                            return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                        } else {
                            // Ensure that the decimal literals have no leading zeros
                            if(value.size_bytes() > 1 && value.data()[0] == '0') {
                                return {anton::expected_error, format_integer_literal_leading_zeros(ctx, node->source_info)};
                            }

                            i64 const v = anton::str_to_i64(value);
                            // The max allowed value is 4294967295
                            if(v <= 4294967295) {
                                return {anton::expected_value};
                            } else {
                                return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                            }
                        }
                    } break;

                    case Integer_Literal_Base::bin: {
                        // The max number of digits in a 32 bit binary number is 32 (excluding the prefix)
                        if(node->value.size_bytes() > 32) {
                            return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                        } else {
                            return {anton::expected_value};
                        }
                    } break;

                    case Integer_Literal_Base::oct: {
                        // The max number of digits in a 32 bit octal number is 12, which corresponds to 0777777777777 (excluding the prefix)
                        if(node->value.size_bytes() > 12) {
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

                    case Integer_Literal_Base::hex: {
                        // The max number of digits in a 32 bit hexadecimal number is 8, which corresponds to 0xFFFFFFFF (excluding the prefix)
                        if(node->value.size_bytes() > 8) {
                            return {anton::expected_error, format_integer_literal_overflow(ctx, node->source_info)};
                        } else {
                            return {anton::expected_value};
                        }
                    } break;
                }
                return {anton::expected_value};
            }

            case AST_Node_Type::identifier_expression: {
                Owning_Ptr<Identifier_Expression>& node = (Owning_Ptr<Identifier_Expression>&)expression;
                Symbol const* const symbol = find_symbol(ctx, node->value);
                if(!symbol) {
                    return {anton::expected_error, format_undefined_symbol(ctx, node->source_info)};
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

            case AST_Node_Type::elvis_expression: {
                Owning_Ptr<Elvis_Expression>& node = (Owning_Ptr<Elvis_Expression>&)expression;
                anton::Expected<void, anton::String> condition_res = process_expression(ctx, node->condition);
                if(!condition_res) {
                    return condition_res;
                }

                anton::Expected<void, anton::String> true_expr_res = process_expression(ctx, node->true_expression);
                if(!true_expr_res) {
                    return true_expr_res;
                }

                anton::Expected<void, anton::String> false_expr_res = process_expression(ctx, node->false_expression);
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

            case AST_Node_Type::prefix_increment_expression: {
                Owning_Ptr<Prefix_Increment_Expression>& node = (Owning_Ptr<Prefix_Increment_Expression>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::prefix_decrement_expression: {
                Owning_Ptr<Prefix_Decrement_Expression>& node = (Owning_Ptr<Prefix_Decrement_Expression>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::postfix_increment_expression: {
                Owning_Ptr<Postfix_Increment_Expression>& node = (Owning_Ptr<Postfix_Increment_Expression>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::postfix_decrement_expression: {
                Owning_Ptr<Postfix_Decrement_Expression>& node = (Owning_Ptr<Postfix_Decrement_Expression>&)expression;
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

                if(symbol->node_type != Symbol_Type::struct_declaration && symbol->node_type != Symbol_Type::function_declaration) {
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

            case AST_Node_Type::parenthesised_expression: {
                Owning_Ptr<Parenthesised_Expression>& node = (Owning_Ptr<Parenthesised_Expression>&)expression;
                anton::Expected<void, anton::String> res = process_expression(ctx, node->expression);
                if(!res) {
                    return res;
                }

                return {anton::expected_value};
            }

            case AST_Node_Type::binary_expression: {
                Owning_Ptr<Binary_Expression>& node = (Owning_Ptr<Binary_Expression>&)expression;
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
                    expression = ANTON_MOV(node->true_expression);
                } else {
                    if(node->false_expression->node_type == AST_Node_Type::expression_if) {
                        anton::Expected<void, anton::String> res = process_expression(ctx, node->false_expression);
                        if(!res) {
                            return res;
                        }
                    }

                    expression = ANTON_MOV(node->false_expression);
                }
                return {anton::expected_value};
            }

            case AST_Node_Type::reinterpret_expression: {
                Owning_Ptr<Reinterpret_Expression>& node = (Owning_Ptr<Reinterpret_Expression>&)expression;
                anton::Expected<void, anton::String> index_res = process_expression(ctx, node->index);
                if(!index_res) {
                    return {anton::expected_error, ANTON_MOV(index_res.error())};
                }

                anton::Expected<void, anton::String> source_res = process_expression(ctx, node->source);
                if(!source_res) {
                    return {anton::expected_error, ANTON_MOV(source_res.error())};
                }

                return {anton::expected_value};
            }

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
                        anton::Expected<void, anton::String> symbol_res = check_and_add_symbol(ctx, decl.identifier->value, &decl);
                        if(!symbol_res) {
                            return symbol_res;
                        }

                        if(decl.initializer) {
                            anton::Expected<void, anton::String> res = process_expression(ctx, decl.initializer);
                            if(!res) {
                                return res;
                            }
                        }
                    } else {
                        Constant_Declaration& decl = (Constant_Declaration&)*node.declaration;
                        anton::Expected<void, anton::String> symbol_res = check_and_add_symbol(ctx, decl.identifier->value, &decl);
                        if(!symbol_res) {
                            return symbol_res;
                        }

                        if(!decl.initializer) {
                            return {anton::expected_error, format_constant_missing_initializer(ctx, decl.source_info)};
                        }

                        anton::Expected<void, anton::String> res = process_expression(ctx, decl.initializer);
                        if(!res) {
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
                        anton::Expected<void, anton::String> symbol_res =
                            check_and_add_symbol(ctx, node.declaration->identifier->value, node.declaration.get());
                        if(!symbol_res) {
                            return symbol_res;
                        }

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
                    anton::Expected<void, anton::String> switch_expr_res = process_expression(ctx, node.match_expression);
                    if(!switch_expr_res) {
                        return switch_expr_res;
                    }

                    struct Label {
                        Integer_Literal* node;
                        i64 value;
                    };

                    Default_Expression* default_label = nullptr;
                    anton::Array<Label> labels;
                    for(auto& s: node.cases) {
                        for(auto& label: s->labels) {
                            anton::Expected<void, anton::String> label_res = process_expression(ctx, label);
                            if(!label_res) {
                                return label_res;
                            }

                            if(label->node_type == AST_Node_Type::default_expression) {
                                // Ensure that the default label is unique
                                if(default_label != nullptr) {
                                    return {anton::expected_error, format_duplicate_default_label(ctx, default_label->source_info, label->source_info)};
                                }

                                default_label = (Default_Expression*)label.get();
                            } else if(label->node_type == AST_Node_Type::integer_literal) {
                                Integer_Literal* literal = (Integer_Literal*)label.get();
                                i64 const value = str_to_i64(literal->value, (u64)literal->base);
                                labels.emplace_back(literal, value);
                            } else {
                                Source_Info const& src = label->source_info;
                                return {anton::expected_error,
                                        build_error_message(src.file_path, src.line, src.column, u8"case label is not an integer literal")};
                            }
                        }

                        // TODO: prevent break from being used within switch

                        anton::Expected<void, anton::String> res = process_statements(ctx, s->statements);
                        if(!res) {
                            return res;
                        }
                    }

                    // Ensure there are no duplicate labels
                    if(labels.size() > 0) {
                        anton::merge_sort(labels.begin(), labels.end(), [](Label const& v1, Label const v2) { return v1.value < v2.value; });
                        for(auto i = labels.begin(), j = labels.begin() + 1, e = labels.end(); j != e; ++i, ++j) {
                            if(i->value == j->value) {
                                Source_Info const& src1 = i->node->source_info;
                                Source_Info const& src2 = j->node->source_info;
                                return {anton::expected_error, format_duplicate_label(ctx, src1, src2)};
                            }
                        }
                    }
                } break;

                case AST_Node_Type::return_statement: {
                    Return_Statement& node = (Return_Statement&)*statements[i];
                    if(node.return_expression) {
                        anton::Expected<void, anton::String> res = process_expression(ctx, node.return_expression);
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

    // process_function
    // Validates function attributes, processes the parameter list and function body resolving any compiletime ifs.
    // Does not add any symbols to the top level scope.
    //
    static anton::Expected<void, anton::String> process_function(Context& ctx, Function_Declaration& fn) {
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

        return {anton::expected_value};
    }

    // process_function
    // Validates function attributes, processes the parameter list and function body resolving any compiletime ifs.
    // Does not add any symbols to the top level scope.
    //
    static anton::Expected<void, anton::String> process_function(Context& ctx, Pass_Stage_Declaration& fn) {
        // Validate the return types
        switch(fn.stage_type) {
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

        return {anton::expected_value};
    }

    // resolve_imports_and_declaration_ifs
    // Processes top-level ast to resolve import declarations and declaration ifs. Modifies ast.
    //
    static anton::Expected<void, anton::String> resolve_imports_and_declaration_ifs(Context& ctx, Declaration_List& ast) {
        // TODO: Currently constants cannot be used in declaration ifs because they are not added to the symbol table as
        //       we process the ast. Should we allow constants in declaration ifs?
        for(i64 i = 0; i < ast.size();) {
            bool const should_advance = ast[i]->node_type != AST_Node_Type::import_declaration && ast[i]->node_type != AST_Node_Type::declaration_if;
            if(ast[i]->node_type == AST_Node_Type::import_declaration) {
                Owning_Ptr<Import_Declaration> node{static_cast<Import_Declaration*>(ast[i].release())};
                ast.erase(ast.begin() + i, ast.begin() + i + 1);

                anton::Expected<Source_Request_Result, anton::String> source_request_res =
                    ctx.source_request_cb(node->path->value, ctx.source_request_user_data);
                if(!source_request_res) {
                    return {anton::expected_error, format_source_import_failed(ctx, node->source_info, source_request_res.error())};
                }

                Source_Request_Result& request_res = source_request_res.value();
                // Ensure we're not importing the same source multiple times
                auto iter = ctx.source_registry.find(request_res.source_name);
                if(iter == ctx.source_registry.end()) {
                    Source_Data source{Owning_Ptr{new anton::String{ANTON_MOV(request_res.source_name)}},
                                       Owning_Ptr{new anton::String{ANTON_MOV(request_res.data)}}};
                    anton::Expected<Declaration_List, Parse_Error> parse_result = parse_source(*source.name, *source.data);
                    if(!parse_result) {
                        Parse_Error const& error = parse_result.error();
                        anton::String error_msg = build_error_message(*source.name, error.line, error.column, error.message);
                        return {anton::expected_error, ANTON_MOV(error_msg)};
                    }

                    ctx.source_registry.emplace(*source.name, ANTON_MOV(source));

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

    static void gather_settings(anton::Array<Pass_Settings>& settings, anton::Slice<Owning_Ptr<Settings_Declaration> const> setting_declarations) {
        for(auto& declaration: setting_declarations) {
            Pass_Settings* pass_iter = anton::find_if(
                settings.begin(), settings.end(), [&pass_name = declaration->pass_name->value](Pass_Settings const& v) { return v.pass_name == pass_name; });
            if(pass_iter == settings.end()) {
                Pass_Settings& v = settings.emplace_back(Pass_Settings{declaration->pass_name->value, {}});
                pass_iter = &v;
            }

            anton::Array<Setting_Key_Value>& pass_settings = pass_iter->settings;
            // N^2 loop to overwrite duplicates in the order of occurence
            for(Setting_Key_Value const& kv_new: declaration->settings) {
                auto end = pass_settings.end();
                auto i = anton::find_if(pass_settings.begin(), end, [&kv_new](Setting_Key_Value const& v) { return kv_new.key == v.key; });
                if(i != end) {
                    i->value = kv_new.value;
                } else {
                    pass_settings.emplace_back(kv_new);
                }
            }
        }
    }

    static anton::Expected<void, anton::String> process_objects_and_populate_symbol_table(Context& ctx, Declaration_List& ast) {
        for(auto& ast_node: ast) {
            switch(ast_node->node_type) {
                case AST_Node_Type::struct_declaration: {
                    Struct_Declaration& node = static_cast<Struct_Declaration&>(*ast_node);
                    if(node.members.size() == 0) {
                        return {anton::expected_error, format_empty_struct(ctx, node.identifier->source_info)};
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

            case AST_Node_Type::elvis_expression: {
                Elvis_Expression& n = (Elvis_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.condition);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.true_expression);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.false_expression);
            } break;

            case AST_Node_Type::binary_expression: {
                Binary_Expression& n = (Binary_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.lhs);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.rhs);
            } break;

            case AST_Node_Type::unary_expression: {
                Unary_Expression& n = (Unary_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::prefix_increment_expression: {
                Prefix_Increment_Expression& n = (Prefix_Increment_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::prefix_decrement_expression: {
                Prefix_Decrement_Expression& n = (Prefix_Decrement_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::array_access_expression: {
                Array_Access_Expression& n = (Array_Access_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.base);
                walk_nodes_and_aggregate_function_calls(function_calls, *n.index);
            } break;

            case AST_Node_Type::postfix_increment_expression: {
                Postfix_Increment_Expression& n = (Postfix_Increment_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::postfix_decrement_expression: {
                Postfix_Decrement_Expression& n = (Postfix_Decrement_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::parenthesised_expression: {
                Parenthesised_Expression& n = (Parenthesised_Expression&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.expression);
            } break;

            case AST_Node_Type::reinterpret_expression: {
                Reinterpret_Expression& n = (Reinterpret_Expression&)node;
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
                for(auto& statement: n.statements) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *statement);
                }
            } break;

            case AST_Node_Type::switch_statement: {
                Switch_Statement& n = (Switch_Statement&)node;
                walk_nodes_and_aggregate_function_calls(function_calls, *n.match_expression);
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
                if(n.return_expression) {
                    walk_nodes_and_aggregate_function_calls(function_calls, *n.return_expression);
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
                anton::String_View identifier = n->value;
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

            case AST_Node_Type::elvis_expression: {
                Owning_Ptr<Elvis_Expression>& n = (Owning_Ptr<Elvis_Expression>&)node;
                replace_identifier_expressions(n->condition, replacements);
                replace_identifier_expressions(n->true_expression, replacements);
                replace_identifier_expressions(n->false_expression, replacements);
            } break;

            case AST_Node_Type::binary_expression: {
                Owning_Ptr<Binary_Expression>& n = (Owning_Ptr<Binary_Expression>&)node;
                replace_identifier_expressions(n->lhs, replacements);
                replace_identifier_expressions(n->rhs, replacements);
            } break;

            case AST_Node_Type::unary_expression: {
                Owning_Ptr<Unary_Expression>& n = (Owning_Ptr<Unary_Expression>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::prefix_increment_expression: {
                Owning_Ptr<Prefix_Increment_Expression>& n = (Owning_Ptr<Prefix_Increment_Expression>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::prefix_decrement_expression: {
                Owning_Ptr<Prefix_Decrement_Expression>& n = (Owning_Ptr<Prefix_Decrement_Expression>&)node;
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

            case AST_Node_Type::postfix_increment_expression: {
                Owning_Ptr<Postfix_Increment_Expression>& n = (Owning_Ptr<Postfix_Increment_Expression>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::postfix_decrement_expression: {
                Owning_Ptr<Postfix_Decrement_Expression>& n = (Owning_Ptr<Postfix_Decrement_Expression>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::parenthesised_expression: {
                Owning_Ptr<Parenthesised_Expression>& n = (Owning_Ptr<Parenthesised_Expression>&)node;
                replace_identifier_expressions(n->expression, replacements);
            } break;

            case AST_Node_Type::reinterpret_expression: {
                Owning_Ptr<Reinterpret_Expression>& n = (Owning_Ptr<Reinterpret_Expression>&)node;
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
                for(auto& statement: n->statements) {
                    replace_identifier_expressions(statement, replacements);
                }
            } break;

            case AST_Node_Type::switch_statement: {
                Owning_Ptr<Switch_Statement>& n = (Owning_Ptr<Switch_Statement>&)node;
                replace_identifier_expressions(n->match_expression, replacements);
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
                if(n->return_expression) {
                    replace_identifier_expressions(n->return_expression, replacements);
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
    // Searches all passes for Function_Call_Expression nodes that have unsized array arguments
    // and creates instances of the corresponding functions with those parameters removed and
    // all corresponding identifiers replaced.
    // Removes unsized array arguments from the Function_Call_Expression nodes.
    //
    // IMPORTANT:
    // This function performs 2 transformations on the ast that do NOT preserve the symbol table:
    //  - functions (instances) with symbols (the unsized arguments) that are undefined are created.
    //  - function call expressions are renamed, but the corresponding functions are not added
    //    to the symbol table.
    // After calling this function it is not safe to lookup non-global or function symbols anymore!
    //
    static void perform_function_instantiations(Context& ctx, anton::Slice<Pass_Context> const passes,
                                                anton::Array<Owning_Ptr<Function_Declaration>>& functions) {
        anton::Array<Function_Call_Expression*> function_calls;
        anton::Flat_Hash_Set<u64> instantiated_functions;
        for(Pass_Context& pass: passes) {
            if(pass.vertex_stage) {
                walk_nodes_and_aggregate_function_calls(function_calls, *pass.vertex_stage);
            }

            if(pass.fragment_stage) {
                walk_nodes_and_aggregate_function_calls(function_calls, *pass.fragment_stage);
            }

            if(pass.compute_stage) {
                walk_nodes_and_aggregate_function_calls(function_calls, *pass.compute_stage);
            }

            for(i64 i = 0; i < function_calls.size(); ++i) {
                Function_Call_Expression& function_call = *function_calls[i];
                Symbol* symbol = find_symbol(ctx, function_call.identifier->value);
                // Builtin types do not have a symbol, but their constructors still produce function calls.
                // Builtin functions do not have a symbol.
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
                    Function_Parameter& p = (Function_Parameter&)*fn_template.params[i];
                    stringified_signature += stringify_type(*p.type);
                    if(is_unsized_array(*p.type)) {
                        ANTON_ASSERT(function_call.arguments[i]->node_type == AST_Node_Type::identifier_expression,
                                     "unsized array argument must be an identifier expression");
                        Identifier_Expression& expr = (Identifier_Expression&)*function_call.arguments[i];
                        instance_name += "_";
                        instance_name += expr.value;
                        requires_instantiation = true;
                    }
                }

                if(!requires_instantiation) {
                    pass.functions.emplace_back(&fn_template);
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
                        Function_Parameter& p = (Function_Parameter&)*fn_template.params[i];
                        if(is_unsized_array(*p.type)) {
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
                    Function_Parameter& p = (Function_Parameter&)*instance->params[i];
                    if(is_unsized_array(*p.type)) {
                        ANTON_ASSERT(function_call.arguments[i]->node_type == AST_Node_Type::identifier_expression,
                                     "unsized array argument must be an identifier expression");
                        Identifier_Expression* argument = (Identifier_Expression*)function_call.arguments[i].get();
                        replacements.emplace_back(p.identifier->value, argument);
                    }
                }

                replace_identifier_expressions(instance, replacements);

                // Remove the unsized array parameters and arguments from the instance and the function call
                for(i64 i = 0; i < instance->params.size();) {
                    Function_Parameter& p = (Function_Parameter&)*instance->params[i];
                    if(is_unsized_array(*p.type)) {
                        auto arg_begin = function_call.arguments.begin();
                        function_call.arguments.erase(arg_begin + i, arg_begin + i + 1);
                        auto param_begin = instance->params.begin();
                        instance->params.erase(param_begin + i, param_begin + i + 1);
                    } else {
                        ++i;
                    }
                }

                walk_nodes_and_aggregate_function_calls(function_calls, *instance);
                pass.functions.emplace_back(instance.get());
                functions.emplace_back(ANTON_MOV(instance));
            }

            function_calls.clear();
            // Passes are independent
            instantiated_functions.clear();
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

    // partition_ast
    // Moves nodes based on their type into arrays.
    //
    static void partition_ast(Declaration_List& ast, anton::Array<Owning_Ptr<Pass_Stage_Declaration>>& stages,
                              anton::Array<Owning_Ptr<Function_Declaration>>& functions, anton::Array<Owning_Ptr<Declaration>>& structs_and_constants,
                              anton::Array<Owning_Ptr<Settings_Declaration>>& settings) {
        for(auto& ast_node: ast) {
            switch(ast_node->node_type) {
                case AST_Node_Type::struct_declaration:
                case AST_Node_Type::constant_declaration: {
                    Owning_Ptr<Declaration>& node = (Owning_Ptr<Declaration>&)ast_node;
                    structs_and_constants.emplace_back(ANTON_MOV(node));
                } break;

                case AST_Node_Type::function_declaration: {
                    Owning_Ptr<Function_Declaration>& node = (Owning_Ptr<Function_Declaration>&)ast_node;
                    functions.emplace_back(ANTON_MOV(node));
                } break;

                case AST_Node_Type::pass_stage_declaration: {
                    Owning_Ptr<Pass_Stage_Declaration>& node = (Owning_Ptr<Pass_Stage_Declaration>&)ast_node;
                    stages.emplace_back(ANTON_MOV(node));
                } break;

                case AST_Node_Type::settings_declaration: {
                    Owning_Ptr<Settings_Declaration>& node = (Owning_Ptr<Settings_Declaration>&)ast_node;
                    settings.emplace_back(ANTON_MOV(node));
                }

                default:
                    break;
            }
        }
    }

    struct AST_Build_Result {
        anton::Array<Pass_Settings> settings;
        anton::Array<Owning_Ptr<Pass_Stage_Declaration>> stages;
        anton::Array<Owning_Ptr<Function_Declaration>> functions;
        anton::Array<Owning_Ptr<Declaration>> structs_and_constants;
    };

    // build_ast_from_sources
    // Parse sources, process imports and declaration ifs, validate functions, fold constants, extract settings.
    // Builds top-level symbol table.
    //
    [[nodiscard]] static anton::Expected<AST_Build_Result, anton::String> build_ast_from_sources(Context& ctx, anton::String const& path) {
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
            Source_Data source{Owning_Ptr{new anton::String{ANTON_MOV(request_res.source_name)}}, Owning_Ptr{new anton::String{ANTON_MOV(request_res.data)}}};
            anton::Expected<Declaration_List, Parse_Error> parse_result = parse_source(*source.name, *source.data);
            if(!parse_result) {
                Parse_Error const& error = parse_result.error();
                anton::String error_msg = build_error_message(*source.name, error.line, error.column, error.message);
                return {anton::expected_error, ANTON_MOV(error_msg)};
            }

            ctx.source_registry.emplace(*source.name, ANTON_MOV(source));
            ast = ANTON_MOV(parse_result.value());
        }

        if(anton::Expected<void, anton::String> res = resolve_imports_and_declaration_ifs(ctx, ast); !res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        if(anton::Expected<void, anton::String> res = process_objects_and_populate_symbol_table(ctx, ast); !res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        AST_Build_Result result;
        anton::Array<Owning_Ptr<Settings_Declaration>> settings;
        partition_ast(ast, result.stages, result.functions, result.structs_and_constants, settings);
        gather_settings(result.settings, settings);

        for(auto& fn: result.functions) {
            anton::Expected<void, anton::String> res = process_function(ctx, *fn);
            if(!res) {
                return {anton::expected_error, ANTON_MOV(res.error())};
            }
        }

        for(auto& fn: result.stages) {
            anton::Expected<void, anton::String> res = process_function(ctx, *fn);
            if(!res) {
                return {anton::expected_error, ANTON_MOV(res.error())};
            }
        }

        return {anton::expected_value, ANTON_MOV(result)};
    }

    [[nodiscard]] static anton::Expected<anton::Array<Pass_Context>, anton::String>
    build_pass_contexts(Context& ctx, anton::Slice<Owning_Ptr<Pass_Stage_Declaration> const> const stage_declarations) {
        anton::Array<Pass_Context> passes;
        for(auto const& stage_declaration: stage_declarations) {
            anton::String const& pass_name = stage_declaration->pass_name->value;
            Pass_Context* pass = anton::find_if(passes.begin(), passes.end(), [&pass_name](Pass_Context const& v) { return v.name == pass_name; });
            if(pass == passes.end()) {
                Pass_Context& v = passes.emplace_back(Pass_Context{pass_name, {}, nullptr, nullptr, nullptr, {}, {}});
                pass = &v;
            }

            // Ensure there is only 1 stage of each type
            switch(stage_declaration->stage_type) {
                case Stage_Type::vertex: {
                    if(pass->vertex_stage) {
                        Source_Info const& src1 = pass->vertex_stage->source_info;
                        Source_Info const& src2 = stage_declaration->source_info;
                        return {anton::expected_error, format_duplicate_pass_stage_error(ctx, src1, src2, pass->name, Stage_Type::vertex)};
                    }

                    pass->vertex_stage = stage_declaration.get();
                } break;

                case Stage_Type::fragment: {
                    if(pass->fragment_stage) {
                        Source_Info const& src1 = pass->vertex_stage->source_info;
                        Source_Info const& src2 = stage_declaration->source_info;
                        return {anton::expected_error, format_duplicate_pass_stage_error(ctx, src1, src2, pass->name, Stage_Type::fragment)};
                    }

                    pass->fragment_stage = stage_declaration.get();
                } break;

                case Stage_Type::compute: {
                    if(pass->compute_stage) {
                        Source_Info const& src1 = pass->vertex_stage->source_info;
                        Source_Info const& src2 = stage_declaration->source_info;
                        return {anton::expected_error, format_duplicate_pass_stage_error(ctx, src1, src2, pass->name, Stage_Type::compute)};
                    }

                    pass->compute_stage = stage_declaration.get();
                } break;
            }
        }

        return {anton::expected_value, ANTON_MOV(passes)};
    }

    // extract_sourced_parameters
    // Removes unsized and opaque sourced parameters from the parameter list of all Pass_Stage_Declarations of all passes
    // and moves them to a different storage.
    //
    // Returns:
    // Array of the extracted unsized and opaque sourced parameters.
    //
    [[nodiscard]] static anton::Array<Owning_Ptr<Function_Parameter>> extract_sourced_parameters(anton::Slice<Pass_Context> const passes) {
        auto extract = [](anton::Array<Owning_Ptr<Function_Parameter>>& sourced_params, Pass_Context& pass, Pass_Stage_Declaration& stage_declaration) {
            Parameter_List& params = stage_declaration.params;
            for(i64 i = 0; i < params.size(); ++i) {
                Function_Parameter& p = (Function_Parameter&)*params[i];
                if(is_sourced_parameter(p) && !is_vertex_input_parameter(p)) {
                    auto iter = pass.sourced_data.find_or_emplace(p.source->value);
                    iter->value.all.emplace_back(&p);
                    if(is_unsized_array(*p.type) || is_opaque_type(*p.type)) {
                        sourced_params.emplace_back(ANTON_MOV((Owning_Ptr<Function_Parameter>&)params[i]));
                        auto begin = params.begin();
                        params.erase(begin + i, begin + i + 1);
                        --i;
                    }
                }
            }
        };

        anton::Array<Owning_Ptr<Function_Parameter>> sourced_params;
        for(Pass_Context& pass: passes) {
            if(pass.vertex_stage) {
                extract(sourced_params, pass, *pass.vertex_stage);
            }

            if(pass.fragment_stage) {
                extract(sourced_params, pass, *pass.fragment_stage);
            }

            if(pass.compute_stage) {
                extract(sourced_params, pass, *pass.compute_stage);
            }
        }

        return sourced_params;
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
            Symbol const* symbol = find_symbol(ctx, t.identifier);
            Struct_Declaration const* struct_declaration = (Struct_Declaration const*)symbol;
            i64 max_alignment = 0;
            i64 offset = 0;
            for(auto& member: struct_declaration->members) {
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
                    return {anton::expected_error, format_missing_vertex_stage_error(ctx, pass.name)};
                }
            } else {
                if(pass.vertex_stage || pass.fragment_stage) {
                    return {anton::expected_error, format_vertex_and_compute_stages_error(ctx, pass.name)};
                }
            }

            // Remove duplicates, validate there is no different-type-same-name sourced parameters, optimize layout
            for(auto& [source_name, data]: pass.sourced_data) {
                // We use stable sort to preserve the order and report duplicates in the correct order
                anton::merge_sort(data.all.begin(), data.all.end(), [](Function_Parameter const* lhs, Function_Parameter const* rhs) {
                    anton::String const& lhs_str = lhs->identifier->value;
                    anton::String const& rhs_str = rhs->identifier->value;
                    return anton::compare(lhs_str, rhs_str) == -1;
                });

                // Ensure there are no name duplicates with different types
                for(auto i = data.all.begin(), j = data.all.begin() + 1, end = data.all.end(); j != end; ++i, ++j) {
                    Function_Parameter const& i_param = **i;
                    Function_Parameter const& j_param = **j;
                    anton::String const i_type = stringify_type(*i_param.type);
                    anton::String const j_type = stringify_type(*j_param.type);
                    if(i_param.identifier->value == j_param.identifier->value && i_type != j_type) {
                        return {anton::expected_error, format_duplicate_sourced_parameter(ctx, i_param.source_info, i_param.type->source_info,
                                                                                          j_param.source_info, j_param.type->source_info)};
                    }
                }

                // Remove type duplicates
                auto end = anton::unique(data.all.begin(), data.all.end(), [](Function_Parameter const* lhs, Function_Parameter const* rhs) {
                    anton::String const i_type = stringify_type(*lhs->type);
                    anton::String const j_type = stringify_type(*rhs->type);
                    return i_type == j_type && lhs->identifier->value == rhs->identifier->value;
                });

                data.all.erase(end, data.all.end());

                // Copy sourced Function_Parameter's to specialized buffers
                for(Function_Parameter const* const p: data.all) {
                    bool const opaque = is_opaque_type(*p->type);
                    if(!opaque) {
                        data.variables.emplace_back(p);
                    } else {
                        data.opaque_variables.emplace_back(p);
                    }
                }

                // Optimize the layout of the variables
                i64 const variables_count = data.variables.size();
                anton::Array<Layout_Info> layout_info{anton::reserve, variables_count};
                for(Function_Parameter const* p: data.variables) {
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
                    anton::Array<Function_Parameter const*> perm_data{variables_count};
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
        ctx.diagnostics = config.diagnostics;
        // Add global scope
        ctx.symbols.emplace_back();
        // Create symbols for the constant defines passed via config
        anton::Array<Owning_Ptr<Declaration>> constant_defines;
        for(Constant_Define const& define: config.defines) {
            Constant_Declaration* decl = new Constant_Declaration(Owning_Ptr{new Builtin_Type(Builtin_GLSL_Type::glsl_int, {config.source_name, 0, 0, 0})},
                                                                  Owning_Ptr{new Identifier(anton::String(define.name), {config.source_name, 0, 0, 0})},
                                                                  Owning_Ptr{new Integer_Literal(anton::to_string(define.value), Integer_Literal_Type::i32,
                                                                                                 Integer_Literal_Base::dec, {config.source_name, 0, 0, 0})},
                                                                  {config.source_name, 0, 0, 0});
            ctx.symbols[0].emplace(define.name, decl);
            constant_defines.emplace_back(decl);
        }

        anton::Expected<AST_Build_Result, anton::String> build_res = build_ast_from_sources(ctx, config.source_name);
        if(!build_res) {
            return {anton::expected_error, ANTON_MOV(build_res.error())};
        }

        AST_Build_Result& ast = build_res.value();
        anton::Expected<anton::Array<Pass_Context>, anton::String> pass_build_res = build_pass_contexts(ctx, ast.stages);
        if(!pass_build_res) {
            return {anton::expected_error, ANTON_MOV(pass_build_res.error())};
        }

        anton::Array<Pass_Context>& passes = pass_build_res.value();
        // BREAKS THE SYMBOL TABLE!
        // Make sure everything that requires the symbol table is done at this point.
        perform_function_instantiations(ctx, passes, ast.functions);

        // TODO: Temporary
        // Copy structs and constants over to each pass regardless of whether they are used or not.
        for(Pass_Context& pass: passes) {
            for(auto& node: ast.structs_and_constants) {
                pass.structs_and_constants.emplace_back(node.get());
            }
        }

        // We want to remove the sourced parameters from all passes so that they are not
        // passed down to codegen which doesn't know about them!.
        // Store the sourced parameters so that they are valid until we generate glsl
        // because the pointers are referenced by Pass_Context's sourced_data.
        [[maybe_unused]] anton::Array<Owning_Ptr<Function_Parameter>> const sourced_params = extract_sourced_parameters(passes);

        if(anton::Expected<void, anton::String> res = validate_and_optimize_passes(ctx, passes); !res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        Codegen_Data codegen_data{config.extensions, ast.settings, passes};
        anton::Expected<anton::Array<Pass_Data>, anton::String> codegen_res = generate_glsl(ctx, codegen_data);
        if(!codegen_res) {
            return {anton::expected_error, ANTON_MOV(codegen_res.error())};
        }

        return {anton::expected_value, Build_Result{ANTON_MOV(ast.settings), ANTON_MOV(codegen_res.value())}};
    }

    [[nodiscard]] static anton::Expected<anton::String, anton::String> resolve_import_path(anton::String const& source_name,
                                                                                           anton::Slice<anton::String const> const import_directories) {
        bool found = false;
        anton::String out_path;
        for(anton::String const& path: import_directories) {
            anton::String resolved_path = anton::fs::concat_paths(path, source_name);
            bool exists = anton::fs::exists(resolved_path);
            if(exists) {
                if(!found) {
                    found = true;
                    out_path = ANTON_MOV(resolved_path);
                } else {
                    anton::String message = anton::format(u8"ambiguous source '{}' matches '{}' and '{}'", source_name, out_path, resolved_path);
                    return {anton::expected_error, ANTON_MOV(message)};
                }
            }
        }

        if(found) {
            return {anton::expected_value, ANTON_MOV(out_path)};
        } else {
            anton::String message = anton::format(u8"could not find the source file '{}'", source_name);
            return {anton::expected_error, ANTON_MOV(message)};
        }
    }

    [[nodiscard]] static anton::Expected<Source_Request_Result, anton::String> file_read_callback(anton::String const& source_name, void* user_data) {
        anton::Slice<anton::String const> const& import_directories = *(anton::Slice<anton::String const> const*)user_data;
        anton::Expected<anton::String, anton::String> res = resolve_import_path(source_name, import_directories);
        if(!res) {
            return {anton::expected_error, ANTON_MOV(res.error())};
        }

        anton::String const& path = res.value();
        anton::fs::Input_File_Stream file;
        if(!file.open(path)) {
            anton::String message = anton::format(u8"could not open '{}' for reading", path);
            return {anton::expected_error, ANTON_MOV(message)};
        }

        file.seek(anton::Seek_Dir::end, 0);
        i64 size = file.tell();
        file.seek(anton::Seek_Dir::beg, 0);
        anton::String file_contents{anton::reserve, size};
        file_contents.force_size(size);
        file.read(file_contents.data(), size);
        return {anton::expected_value, Source_Request_Result{ANTON_MOV(res.value()), ANTON_MOV(file_contents)}};
    }

    anton::Expected<Build_Result, anton::String> compile_to_glsl(Configuration const& config, anton::Slice<anton::String const> const& import_directories) {
        return compile_to_glsl(config, file_read_callback, (void*)&import_directories);
    }
} // namespace vush
