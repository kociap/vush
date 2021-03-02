#include <codegen.hpp>

#include <anton/algorithm.hpp>
#include <anton/assert.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/format.hpp>
#include <anton/intrinsics.hpp>
#include <anton/math/math.hpp>
#include <anton/slice.hpp>
#include <ast.hpp>
#include <diagnostics.hpp>

namespace vush {
    struct Codegen_Context {
        Context const& ctx;
        i64 indent;
    };

    static void write_indent(anton::String& out, i64 indent) {
        for(i64 i = 0; i < indent; ++i) {
            out += u8"    ";
        }
    }

    struct Reinterpret_Context {
        anton::String source_expr;
        anton::String index_expr;
        i64 offset = 0;
    };

    static void stringify_type_reinterpret(anton::String& out, Type& type, Codegen_Context& codegen_ctx, Reinterpret_Context& reinterpret_ctx) {
        switch(type.node_type) {
            case AST_Node_Type::user_defined_type: {
                User_Defined_Type& t = (User_Defined_Type&)type;
                out += t.name;
                out += u8"(";
                // We made sure that the symbol exists during validation stage
                Symbol const* symbol = find_symbol(codegen_ctx.ctx, t.name);
                Struct_Decl const& struct_decl = (Struct_Decl const&)*symbol;
                for(i64 i = 0; i < struct_decl.members.size(); ++i) {
                    if(i != 0) {
                        out += u8", ";
                    }

                    auto& member = struct_decl.members[i];
                    stringify_type_reinterpret(out, *member->type, codegen_ctx, reinterpret_ctx);
                }
                out += u8")";
            } break;

            case AST_Node_Type::builtin_type: {
                Builtin_Type& t = (Builtin_Type&)type;
                out += stringify(t.type);
                out += u8"(";
                switch(t.type) {
                    case Builtin_GLSL_Type::glsl_bool:
                    case Builtin_GLSL_Type::glsl_int: {
                        out += u8"floatBitsToInt(";
                        out += reinterpret_ctx.source_expr;
                        out += u8"[";
                        out += reinterpret_ctx.index_expr;
                        out += u8" + ";
                        out += anton::to_string(reinterpret_ctx.offset);
                        out += u8"]";
                        out += u8")";
                        reinterpret_ctx.offset += 1;
                    } break;

                    case Builtin_GLSL_Type::glsl_uint: {
                        out += u8"floatBitsToUint(";
                        out += reinterpret_ctx.source_expr;
                        out += u8"[";
                        out += reinterpret_ctx.index_expr;
                        out += u8" + ";
                        out += anton::to_string(reinterpret_ctx.offset);
                        out += u8"]";
                        out += u8")";
                        reinterpret_ctx.offset += 1;
                    } break;

                    case Builtin_GLSL_Type::glsl_float: {
                        out += reinterpret_ctx.source_expr;
                        out += u8"[";
                        out += reinterpret_ctx.index_expr;
                        out += u8" + ";
                        out += anton::to_string(reinterpret_ctx.offset);
                        out += u8"]";
                        reinterpret_ctx.offset += 1;
                    } break;

                    case Builtin_GLSL_Type::glsl_double: {
                        // Output
                        // packDouble2x32(uvec2(floatBitsToUint(data[index + offset]), floatBitsToUint(data[index + offset + 1])))
                        out += u8"packDouble2x32(uvec2(";
                        out += u8"floatBitsToUint(";
                        out += reinterpret_ctx.source_expr;
                        out += u8"[";
                        out += reinterpret_ctx.index_expr;
                        out += u8" + ";
                        out += anton::to_string(reinterpret_ctx.offset);
                        out += u8"]";
                        out += u8"), ";
                        out += u8"floatBitsToUint(";
                        out += reinterpret_ctx.source_expr;
                        out += u8"[";
                        out += reinterpret_ctx.index_expr;
                        out += u8" + ";
                        out += anton::to_string(reinterpret_ctx.offset + 1);
                        out += u8"]";
                        out += u8")";
                        out += u8"))";
                        reinterpret_ctx.offset += 2;
                    } break;

                    case Builtin_GLSL_Type::glsl_vec2:
                    case Builtin_GLSL_Type::glsl_vec3:
                    case Builtin_GLSL_Type::glsl_vec4:
                    case Builtin_GLSL_Type::glsl_mat2:
                    case Builtin_GLSL_Type::glsl_mat2x3:
                    case Builtin_GLSL_Type::glsl_mat2x4:
                    case Builtin_GLSL_Type::glsl_mat3:
                    case Builtin_GLSL_Type::glsl_mat3x2:
                    case Builtin_GLSL_Type::glsl_mat3x4:
                    case Builtin_GLSL_Type::glsl_mat4:
                    case Builtin_GLSL_Type::glsl_mat4x2:
                    case Builtin_GLSL_Type::glsl_mat4x3: {
                        i64 component_count = 0;
                        switch(t.type) {
                            case Builtin_GLSL_Type::glsl_vec2:
                                component_count = 2;
                                break;

                            case Builtin_GLSL_Type::glsl_vec3:
                                component_count = 3;
                                break;

                            case Builtin_GLSL_Type::glsl_vec4:
                                component_count = 4;
                                break;

                            case Builtin_GLSL_Type::glsl_mat2:
                                component_count = 4;
                                break;

                            case Builtin_GLSL_Type::glsl_mat2x3:
                            case Builtin_GLSL_Type::glsl_mat3x2:
                                component_count = 6;
                                break;

                            case Builtin_GLSL_Type::glsl_mat2x4:
                            case Builtin_GLSL_Type::glsl_mat4x2:
                                component_count = 8;
                                break;

                            case Builtin_GLSL_Type::glsl_mat3:
                                component_count = 9;
                                break;

                            case Builtin_GLSL_Type::glsl_mat3x4:
                                component_count = 12;
                                break;

                            case Builtin_GLSL_Type::glsl_mat4:
                                component_count = 16;
                                break;

                            default:
                                ANTON_UNREACHABLE();
                        }

                        for(i64 i = 0; i < component_count; ++i) {
                            if(i != 0) {
                                out += u8", ";
                            }

                            out += reinterpret_ctx.source_expr;
                            out += u8"[";
                            out += reinterpret_ctx.index_expr;
                            out += u8" + ";
                            out += anton::to_string(reinterpret_ctx.offset + i);
                            out += u8"]";
                        }
                        reinterpret_ctx.offset += component_count;
                    } break;

                    default:
                        ANTON_UNREACHABLE();
                }
                out += u8")";
            } break;

            case AST_Node_Type::array_type: {
                Array_Type& t = (Array_Type&)type;
                // We made sure that the array is sized during validation stage
                out += stringify_type(t);
                out += u8"(";
                stringify_type_reinterpret(out, *t.base, codegen_ctx, reinterpret_ctx);
                out += u8")";
            } break;

            default:
                ANTON_UNREACHABLE();
        }
    }

    static void stringify(anton::String& out, AST_Node const& ast_node, Codegen_Context& ctx) {
        switch(ast_node.node_type) {
            case AST_Node_Type::identifier: {
                Identifier& node = (Identifier&)ast_node;
                out += node.value;
                return;
            }

            case AST_Node_Type::builtin_type: {
                Builtin_Type& node = (Builtin_Type&)ast_node;
                out += stringify(node.type);
                return;
            }

            case AST_Node_Type::user_defined_type: {
                User_Defined_Type& node = (User_Defined_Type&)ast_node;
                out += node.name;
                return;
            }

            case AST_Node_Type::array_type: {
                Array_Type& node = (Array_Type&)ast_node;
                stringify(out, *node.base, ctx);
                out += u8"[";
                if(node.size) {
                    stringify(out, *node.size, ctx);
                }
                out += u8"]";
                return;
            }

            case AST_Node_Type::constant_declaration: {
                Constant_Declaration& node = (Constant_Declaration&)ast_node;
                out += u8"const ";
                stringify(out, *node.type, ctx);
                out += u8" ";
                stringify(out, *node.identifier, ctx);
                out += u8" = ";
                stringify(out, *node.initializer, ctx);
                out += u8";\n";
                return;
            }

            case AST_Node_Type::variable_declaration: {
                Variable_Declaration& node = (Variable_Declaration&)ast_node;
                stringify(out, *node.type, ctx);
                out += u8" ";
                stringify(out, *node.identifier, ctx);
                if(node.initializer) {
                    out += u8" = ";
                    stringify(out, *node.initializer, ctx);
                }
                out += u8";\n";
                return;
            }

            case AST_Node_Type::struct_decl: {
                Struct_Decl& node = (Struct_Decl&)ast_node;
                out += u8"struct ";
                stringify(out, *node.identifier, ctx);
                out += u8" {\n";
                ctx.indent += 1;
                for(auto& member: node.members) {
                    write_indent(out, ctx.indent);
                    stringify(out, *member->type, ctx);
                    out += u8" ";
                    stringify(out, *member->identifier, ctx);
                    out += u8";\n";
                }
                ctx.indent -= 1;
                out += u8"};\n";
                return;
            }

            case AST_Node_Type::function_declaration: {
                Function_Declaration& node = (Function_Declaration&)ast_node;
                stringify(out, *node.return_type, ctx);
                out += u8" ";
                stringify(out, *node.identifier, ctx);
                // param list
                out += u8"(";
                if(node.params.size() > 0) {
                    stringify(out, *node.params[0], ctx);
                    for(i64 i = 1; i != node.params.size(); ++i) {
                        out += u8", ";
                        stringify(out, *node.params[i], ctx);
                    }
                }
                out += u8") {\n";
                ctx.indent += 1;
                for(auto& statement: node.body) {
                    stringify(out, *statement, ctx);
                }
                ctx.indent -= 1;
                out += u8"}\n";
                return;
            }

            case AST_Node_Type::ordinary_function_param: {
                Ordinary_Function_Param& node = (Ordinary_Function_Param&)ast_node;
                stringify(out, *node.type, ctx);
                out += u8" ";
                stringify(out, *node.identifier, ctx);
                return;
            }

            case AST_Node_Type::sourced_function_param: {
                Sourced_Function_Param& node = (Sourced_Function_Param&)ast_node;
                stringify(out, *node.type, ctx);
                out += u8" ";
                stringify(out, *node.identifier, ctx);
                return;
            }

            case AST_Node_Type::vertex_input_param: {
                Vertex_Input_Param& node = (Vertex_Input_Param&)ast_node;
                stringify(out, *node.type, ctx);
                out += u8" ";
                stringify(out, *node.identifier, ctx);
                return;
            }

            case AST_Node_Type::block_statement: {
                Block_Statement& node = (Block_Statement&)ast_node;
                write_indent(out, ctx.indent);
                out += u8"{\n";
                ctx.indent += 1;
                for(auto& statement: node.statements) {
                    stringify(out, *statement, ctx);
                }
                ctx.indent -= 1;
                write_indent(out, ctx.indent);
                out += u8"}\n";
                return;
            }

            case AST_Node_Type::if_statement: {
                If_Statement& node = (If_Statement&)ast_node;
                write_indent(out, ctx.indent);
                out += u8"if(";
                stringify(out, *node.condition, ctx);
                out += u8") {\n";
                ctx.indent += 1;
                for(auto& statement: node.true_statements) {
                    stringify(out, *statement, ctx);
                }
                ctx.indent -= 1;
                if(node.false_statements.size() == 0) {
                    write_indent(out, ctx.indent);
                    out += u8"}\n";
                } else {
                    write_indent(out, ctx.indent);
                    out += u8"} else {\n";
                    ctx.indent += 1;
                    for(auto& statement: node.false_statements) {
                        stringify(out, *statement, ctx);
                    }
                    ctx.indent -= 1;
                    write_indent(out, ctx.indent);
                    out += u8"}\n";
                }
                return;
            }

            case AST_Node_Type::switch_statement: {
                Switch_Statement& node = (Switch_Statement&)ast_node;
                write_indent(out, ctx.indent);
                out += u8"switch(";
                stringify(out, *node.match_expr, ctx);
                out += u8") {\n";
                for(auto& switch_node: node.cases) {
                    write_indent(out, ctx.indent + 1);
                    if(switch_node->node_type == AST_Node_Type::case_statement) {
                        Case_Statement& switch_case = (Case_Statement&)*switch_node;
                        out += u8"case ";
                        stringify(out, *switch_case.condition, ctx);
                        out += ":\n";
                        ctx.indent += 2;
                        for(auto& statement: switch_case.statements) {
                            stringify(out, *statement, ctx);
                        }
                        ctx.indent -= 2;
                    } else {
                        Default_Case_Statement& switch_case = (Default_Case_Statement&)*switch_node;
                        out += u8"default:\n";
                        ctx.indent += 2;
                        for(auto& statement: switch_case.statements) {
                            stringify(out, *statement, ctx);
                        }
                        ctx.indent -= 2;
                    }
                }
                write_indent(out, ctx.indent);
                out += u8"}\n";
                return;
            }

            case AST_Node_Type::for_statement: {
                For_Statement& node = (For_Statement&)ast_node;
                write_indent(out, ctx.indent);
                out += u8"for(";
                if(node.declaration) {
                    // We stringify the variable_decl manually because we need inline declaration
                    Variable_Declaration& decl = *node.declaration;
                    stringify(out, *decl.type, ctx);
                    out += u8" ";
                    stringify(out, *decl.identifier, ctx);
                    out += u8" = ";
                    stringify(out, *decl.initializer, ctx);
                }
                out += u8";";
                if(node.condition) {
                    out += u8" ";
                    stringify(out, *node.condition, ctx);
                }
                out += u8";";
                if(node.post_expression) {
                    out += u8" ";
                    stringify(out, *node.post_expression, ctx);
                }
                // We stringify the block ourselves to allow custom formatting of the braces
                out += u8") {\n";
                ctx.indent += 1;
                for(auto& statement: node.statements) {
                    stringify(out, *statement, ctx);
                }
                ctx.indent -= 1;
                write_indent(out, ctx.indent);
                out += u8"}\n";
                return;
            }

            case AST_Node_Type::while_statement: {
                While_Statement& node = (While_Statement&)ast_node;
                write_indent(out, ctx.indent);
                out += u8"while(";
                stringify(out, *node.condition, ctx);
                // We need no-braces block. We add them inline ourselves.
                out += u8") {\n";
                ctx.indent += 1;
                for(auto& statement: node.statements) {
                    stringify(out, *statement, ctx);
                }
                ctx.indent -= 1;
                write_indent(out, ctx.indent);
                out += u8"}\n";
                return;
            }

            case AST_Node_Type::do_while_statement: {
                Do_While_Statement& node = (Do_While_Statement&)ast_node;
                write_indent(out, ctx.indent);
                // We need no-braces block. We add them inline ourselves.
                out += u8"do {\n";
                ctx.indent += 1;
                for(auto& statement: node.statements) {
                    stringify(out, *statement, ctx);
                }
                ctx.indent -= 1;
                write_indent(out, ctx.indent);
                out += u8"} while(";
                stringify(out, *node.condition, ctx);
                out += u8");\n";
                return;
            }

            case AST_Node_Type::return_statement: {
                Return_Statement& node = (Return_Statement&)ast_node;
                write_indent(out, ctx.indent);
                out += u8"return";
                if(node.return_expr) {
                    out += u8" ";
                    stringify(out, *node.return_expr, ctx);
                }
                out += u8";\n";
                return;
            }

            case AST_Node_Type::break_statement: {
                write_indent(out, ctx.indent);
                out += u8"break;\n";
                return;
            }

            case AST_Node_Type::continue_statement: {
                write_indent(out, ctx.indent);
                out += u8"continue;\n";
                return;
            }

            case AST_Node_Type::discard_statement: {
                write_indent(out, ctx.indent);
                out += u8"discard;\n";
                return;
            }

            case AST_Node_Type::declaration_statement: {
                Declaration_Statement& node = (Declaration_Statement&)ast_node;
                write_indent(out, ctx.indent);
                stringify(out, *node.declaration, ctx);
                return;
            }

            case AST_Node_Type::expression_statement: {
                Expression_Statement& node = (Expression_Statement&)ast_node;
                write_indent(out, ctx.indent);
                stringify(out, *node.expression, ctx);
                out += u8";\n";
                return;
            }

            case AST_Node_Type::assignment_expression: {
                Assignment_Expression& node = (Assignment_Expression&)ast_node;
                stringify(out, *node.lhs, ctx);
                out += u8" = ";
                stringify(out, *node.rhs, ctx);
                return;
            }

            case AST_Node_Type::arithmetic_assignment_expression: {
                Arithmetic_Assignment_Expression& node = (Arithmetic_Assignment_Expression&)ast_node;
                stringify(out, *node.lhs, ctx);
                switch(node.type) {
                    case Arithmetic_Assignment_Type::plus: {
                        out += u8" += ";
                    } break;

                    case Arithmetic_Assignment_Type::minus: {
                        out += u8" -= ";
                    } break;

                    case Arithmetic_Assignment_Type::multiply: {
                        out += u8" *= ";
                    } break;

                    case Arithmetic_Assignment_Type::divide: {
                        out += u8" /= ";
                    } break;

                    case Arithmetic_Assignment_Type::remainder: {
                        out += u8" %= ";
                    } break;

                    case Arithmetic_Assignment_Type::lshift: {
                        out += u8" <<= ";
                    } break;

                    case Arithmetic_Assignment_Type::rshift: {
                        out += u8" >>= ";
                    } break;

                    case Arithmetic_Assignment_Type::bit_and: {
                        out += u8" &= ";
                    } break;

                    case Arithmetic_Assignment_Type::bit_xor: {
                        out += u8" ^= ";
                    } break;

                    case Arithmetic_Assignment_Type::bit_or: {
                        out += u8" |= ";
                    } break;
                }
                stringify(out, *node.rhs, ctx);
                return;
            }

            case AST_Node_Type::elvis_expr: {
                Elvis_Expr& node = (Elvis_Expr&)ast_node;
                stringify(out, *node.condition, ctx);
                out += u8" ? ";
                stringify(out, *node.true_expr, ctx);
                out += u8" : ";
                stringify(out, *node.false_expr, ctx);
                return;
            }

            case AST_Node_Type::binary_expr: {
                Binary_Expr& node = (Binary_Expr&)ast_node;
                stringify(out, *node.lhs, ctx);
                switch(node.type) {
                    case Binary_Expr_Type::logic_or:
                        out += u8" || ";
                        break;
                    case Binary_Expr_Type::logic_xor:
                        out += u8" ^^ ";
                        break;
                    case Binary_Expr_Type::logic_and:
                        out += u8" && ";
                        break;
                    case Binary_Expr_Type::equal:
                        out += u8" == ";
                        break;
                    case Binary_Expr_Type::unequal:
                        out += u8" != ";
                        break;
                    case Binary_Expr_Type::greater_than:
                        out += u8" > ";
                        break;
                    case Binary_Expr_Type::less_than:
                        out += u8" < ";
                        break;
                    case Binary_Expr_Type::greater_equal:
                        out += u8" >= ";
                        break;
                    case Binary_Expr_Type::less_equal:
                        out += u8" <= ";
                        break;
                    case Binary_Expr_Type::bit_or:
                        out += u8" | ";
                        break;
                    case Binary_Expr_Type::bit_xor:
                        out += u8" ^ ";
                        break;
                    case Binary_Expr_Type::bit_and:
                        out += u8" & ";
                        break;
                    case Binary_Expr_Type::lshift:
                        out += u8" << ";
                        break;
                    case Binary_Expr_Type::rshift:
                        out += u8" >> ";
                        break;
                    case Binary_Expr_Type::add:
                        out += u8" + ";
                        break;
                    case Binary_Expr_Type::sub:
                        out += u8" - ";
                        break;
                    case Binary_Expr_Type::mul:
                        out += u8" * ";
                        break;
                    case Binary_Expr_Type::div:
                        out += u8" / ";
                        break;
                    case Binary_Expr_Type::mod:
                        out += u8" % ";
                        break;
                }
                stringify(out, *node.rhs, ctx);
                return;
            }

            case AST_Node_Type::unary_expression: {
                Unary_Expression& node = (Unary_Expression&)ast_node;
                switch(node.type) {
                    case Unary_Type::plus:
                        break;

                    case Unary_Type::minus: {
                        out += u8"-";
                    } break;

                    case Unary_Type::logic_not: {
                        out += u8"!";
                    } break;

                    case Unary_Type::bit_not: {
                        out += u8"~";
                    } break;
                }
                stringify(out, *node.expression, ctx);
                return;
            }

            case AST_Node_Type::prefix_inc_expr: {
                Prefix_Inc_Expr& node = (Prefix_Inc_Expr&)ast_node;
                out += u8"++";
                stringify(out, *node.expression, ctx);
                return;
            }

            case AST_Node_Type::prefix_dec_expr: {
                Prefix_Dec_Expr& node = (Prefix_Dec_Expr&)ast_node;
                out += u8"--";
                stringify(out, *node.expression, ctx);
                return;
            }

            case AST_Node_Type::function_call_expression: {
                Function_Call_Expression& node = (Function_Call_Expression&)ast_node;
                stringify(out, *node.identifier, ctx);
                out += u8"(";
                if(node.arguments.size() > 0) {
                    stringify(out, *node.arguments[0], ctx);

                    for(i64 i = 1; i < node.arguments.size(); ++i) {
                        out += u8", ";
                        stringify(out, *node.arguments[i], ctx);
                    }
                }
                out += u8")";
                return;
            }

            case AST_Node_Type::member_access_expression: {
                Member_Access_Expression& node = (Member_Access_Expression&)ast_node;
                stringify(out, *node.base, ctx);
                out += u8".";
                stringify(out, *node.member, ctx);
                return;
            }

            case AST_Node_Type::array_access_expression: {
                Array_Access_Expression& node = (Array_Access_Expression&)ast_node;
                stringify(out, *node.base, ctx);
                out += u8"[";
                stringify(out, *node.index, ctx);
                out += u8"]";
                return;
            }

            case AST_Node_Type::postfix_inc_expr: {
                Postfix_Inc_Expr& node = (Postfix_Inc_Expr&)ast_node;
                stringify(out, *node.expression, ctx);
                out += u8"++";
                return;
            }

            case AST_Node_Type::postfix_dec_expr: {
                Postfix_Dec_Expr& node = (Postfix_Dec_Expr&)ast_node;
                stringify(out, *node.expression, ctx);
                out += u8"--";
                return;
            }

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& node = (Identifier_Expression&)ast_node;
                stringify(out, *node.identifier, ctx);
                return;
            }

            case AST_Node_Type::paren_expr: {
                Paren_Expr& node = (Paren_Expr&)ast_node;
                out += u8"(";
                stringify(out, *node.expression, ctx);
                out += u8")";
                return;
            }

            case AST_Node_Type::reinterpret_expr: {
                Reinterpret_Expr& node = (Reinterpret_Expr&)ast_node;
                // TODO: Validate source is a float array
                // TODO: Validate target type
                Reinterpret_Context reinterpret_ctx;
                stringify(reinterpret_ctx.source_expr, *node.source, ctx);
                stringify(reinterpret_ctx.index_expr, *node.index, ctx);
                stringify_type_reinterpret(out, *node.target_type, ctx, reinterpret_ctx);
                return;
            }

            case AST_Node_Type::bool_literal: {
                Bool_Literal& node = (Bool_Literal&)ast_node;
                out += node.value ? u8"true" : u8"false";
                return;
            }

            case AST_Node_Type::integer_literal: {
                Integer_Literal& node = (Integer_Literal&)ast_node;
                if(node.base == Integer_Literal_Base::hex) {
                    out += u8"0x";
                }
                out += node.value;
                if(node.type == Integer_Literal_Type::u32) {
                    out += u8"u";
                }
                return;
            }

            case AST_Node_Type::float_literal: {
                Float_Literal& node = (Float_Literal&)ast_node;
                out += node.value;
                if(node.type == Float_Literal_Type::f64) {
                    out += u8"lf";
                }
                return;
            }

            default:
                break;
        }
    }

    static void stringify_function_forward_decl(anton::String& out, Function_Declaration const& node, Codegen_Context& ctx) {
        stringify(out, *node.return_type, ctx);
        out += u8" ";
        stringify(out, *node.identifier, ctx);
        // param list
        out += u8"(";
        if(node.params.size() > 0) {
            stringify(out, *node.params[0], ctx);
            for(i64 i = 1; i != node.params.size(); ++i) {
                out += u8", ";
                stringify(out, *node.params[i], ctx);
            }
        }
        out += u8");\n";
    }

    struct Member_Info {
        anton::String name;
        anton::String accessor;
        Builtin_GLSL_Type type;
        i64 location_slots;
        Interpolation interpolation;
        bool invariant;
    };

    // explode_type
    // name is prefixed with '_'
    // accessor is prefixed with '.'
    //
    static void explode_type(Context const& ctx, Type const& type, anton::Array<Member_Info>& member_info) {
        auto _explode_type = [](auto& _explode_type, Context const& ctx, Type const& type, anton::Array<anton::String>& name_components,
                                Interpolation parent_interpolation, bool parent_invariant, anton::Array<Member_Info>& member_info) -> void {
            ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type, "unknown ast node type");
            if(type.node_type == AST_Node_Type::user_defined_type) {
                User_Defined_Type const& node = (User_Defined_Type const&)type;
                Symbol const* symbol = find_symbol(ctx, node.name);
                ANTON_ASSERT(symbol, "undefined symbol");
                Struct_Decl const* struct_decl = (Struct_Decl const*)symbol;
                for(auto& member: struct_decl->members) {
                    name_components.emplace_back(member->identifier->value);
                    Interpolation const interpolation = (member->interpolation != Interpolation::none ? member->interpolation : parent_interpolation);
                    bool const invariant = (parent_invariant ? parent_invariant : member->invariant);
                    _explode_type(_explode_type, ctx, *member->type, name_components, interpolation, invariant, member_info);
                    name_components.pop_back();
                }
            } else {
                Builtin_Type const& t = (Builtin_Type const&)type;
                anton::String name;
                anton::String accessor;
                for(anton::String const& component: name_components) {
                    name += u8"_" + component;
                    accessor += u8"." + component;
                }
                // TODO: fix location increment for types that require more than 1 slot
                member_info.emplace_back(ANTON_MOV(name), ANTON_MOV(accessor), t.type, 1, parent_interpolation, parent_invariant);
            }
        };

        anton::Array<anton::String> name_components;
        _explode_type(_explode_type, ctx, type, name_components, Interpolation::none, false, member_info);
    }

    static anton::Expected<anton::String, anton::String> format_bind_string(anton::String_View string, Identifier const& identifier, Type const& type) {
        anton::String out;
        auto iter1 = string.bytes_begin();
        auto iter2 = string.bytes_begin();
        auto const end = string.bytes_end();
        while(true) {
            while(iter2 != end && *iter2 != U'{') {
                ++iter2;
            }

            if(iter2 == end) {
                out.append({iter1, iter2});
                break;
            }

            ++iter2;
            if(iter2 == end || *iter2 != U'{') {
                // Single brace or end
                continue;
            }

            out.append({iter1, iter2 - 1});

            ++iter2;
            iter1 = iter2;
            while(iter2 != end && *iter2 != '}') {
                ++iter2;
            }

            // TODO: Better error messages

            // Check for unterminated placeholder
            if(iter2 == end || iter2 + 1 == end || *(iter2 + 1) != U'}') {
                return {anton::expected_error, anton::String(u8"error: unterminated placeholder in bind string")};
            }

            anton::String_View const symbol_name = {iter1, iter2};
            i64 const dot_pos = anton::find_substring(symbol_name, u8".");
            if(dot_pos == anton::npos) {
                // If it's not a builtin and doesn't have a dot, what is it?
                return {anton::expected_error, anton::String(u8"error: invalid placeholder in bind string")};
            }

            anton::String_View const iterator_name = {symbol_name.data(), dot_pos};
            anton::String_View const property_name = {symbol_name.data() + dot_pos + 1, symbol_name.bytes_end()};
            if(iterator_name != u8"$variable") {
                return {anton::expected_error, anton::String(u8"error: unknown placeholder in bind string")};
            }

            if(property_name == u8"name") {
                out += identifier.value;
            } else if(property_name == u8"type") {
                out += stringify_type(type);
            } else {
                return {anton::expected_error, anton::String(u8"error: unknown property name in bind string")};
            }

            // Skip the terminating }}
            iter2 = iter2 + 2;
            iter1 = iter2;
        }

        return {anton::expected_value, ANTON_MOV(out)};
    }

    static anton::Expected<anton::String, anton::String>
    generate_vertex_stage(Context const& ctx, Codegen_Context& codegen_ctx, Pass_Stage_Declaration const& stage,
                          anton::Flat_Hash_Map<anton::String, Source_Definition> const& source_definitions) {
        anton::String const& pass_name = stage.pass->value;
        anton::String out;
        // Stringify the stage function
        stringify(out, *stage.return_type, codegen_ctx);
        out += u8" ";
        anton::String const stage_function_name = u8"_pass_" + pass_name + u8"_stage_vertex";
        out += stage_function_name;
        // param list
        out += u8"(";
        if(stage.params.size() > 0) {
            stringify(out, *stage.params[0], codegen_ctx);
            for(i64 i = 1; i != stage.params.size(); ++i) {
                out += u8", ";
                stringify(out, *stage.params[i], codegen_ctx);
            }
        }
        out += u8") {\n";
        codegen_ctx.indent += 1;
        for(auto& statement: stage.body) {
            stringify(out, *statement, codegen_ctx);
        }
        codegen_ctx.indent -= 1;
        out += u8"}\n\n";

        // Write vertex inputs
        // Vertex input variable names are of the form '_pass_<pass name>_<parameter name><exploded member name>'.
        {
            anton::Array<Member_Info> members_info;
            for(auto& param: stage.params) {
                if(param->node_type == AST_Node_Type::vertex_input_param) {
                    Owning_Ptr<Vertex_Input_Param>& node = (Owning_Ptr<Vertex_Input_Param>&)param;
                    explode_type(ctx, *node->type, members_info);
                    i64 location = 0;
                    for(Member_Info const& m: members_info) {
                        anton::String location_str = anton::to_string(location);
                        anton::String_View type_str = stringify(m.type);
                        // We attach parameter names to the vertex inputs
                        anton::String const& param_name = node->identifier->value;
                        out += anton::format(u8"layout(location = {}) in {} _pass_{}_{}{};\n", location_str, type_str, pass_name, param_name, m.name);
                        location += m.location_slots;
                    }
                    members_info.clear();
                }
            }
            out += u8"\n";
        }

        bool const return_type_is_void =
            stage.return_type->node_type == AST_Node_Type::builtin_type && ((Builtin_Type&)*stage.return_type).type == Builtin_GLSL_Type::glsl_void;
        if(!return_type_is_void) {
            // Write vertex outputs
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *stage.return_type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                out += anton::format(u8"layout(location = {}) out {} _pass_{}_out{};\n", location_str, type_str, pass_name, m.name);
                location += m.location_slots;
            }
            out += u8"\n";
        }

        // Output main
        out += u8"void main() {\n";
        codegen_ctx.indent += 1;

        // Generate sourced parameters
        anton::Array<anton::String> arguments;
        {
            i64 param_index = 0;
            anton::Array<Member_Info> members_info;
            for(auto& param: stage.params) {
                ANTON_ASSERT(param->node_type == AST_Node_Type::vertex_input_param || param->node_type == AST_Node_Type::sourced_function_param,
                             u8"invalid parameter type");
                switch(param->node_type) {
                    case AST_Node_Type::vertex_input_param: {
                        Vertex_Input_Param* const node = (Vertex_Input_Param*)param.get();
                        write_indent(out, codegen_ctx.indent);
                        stringify(out, *node->type, codegen_ctx);
                        anton::String arg_name = "arg" + anton::to_string(param_index);
                        out += u8" ";
                        out += arg_name;
                        out += u8";\n";

                        explode_type(ctx, *node->type, members_info);
                        // write vertex input assignments
                        for(Member_Info const& m: members_info) {
                            write_indent(out, codegen_ctx.indent);
                            anton::String const& param_name = node->identifier->value;
                            out += anton::format(u8"{}{} = _pass_{}_{}{};\n", arg_name, m.accessor, pass_name, param_name, m.name);
                        }
                        members_info.clear();
                        arguments.emplace_back(ANTON_MOV(arg_name));
                        param_index += 1;
                    } break;

                    case AST_Node_Type::sourced_function_param: {
                        Sourced_Function_Param const& node = (Sourced_Function_Param const&)*param;
                        auto iter = source_definitions.find(node.source->value);
                        ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
                        anton::String_View bind_string = iter->value.bind;
                        anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, *node.identifier, *node.type);
                        if(res) {
                            arguments.emplace_back(ANTON_MOV(res.value()));
                        } else {
                            return {anton::expected_error, ANTON_MOV(res.error())};
                        }
                    } break;

                    default:
                        ANTON_UNREACHABLE();
                }
            }
        }

        write_indent(out, codegen_ctx.indent);
        if(!return_type_is_void) {
            out += stringify_type(*stage.return_type);
            out += u8" _res = ";
        }

        out += stage_function_name;
        out += u8"(";
        if(arguments.size() > 0) {
            out += arguments[0];
            for(i64 i = 1; i < arguments.size(); ++i) {
                out += u8", ";
                out += arguments[i];
            }
        }
        out += u8");\n";

        if(!return_type_is_void) {
            // Write vertex output assignments
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *stage.return_type, members_info);
            for(Member_Info const& m: members_info) {
                write_indent(out, codegen_ctx.indent);
                out += anton::format(u8"_pass_{}_out{} = _res{};\n", pass_name, m.name, m.accessor);
            }
        }

        codegen_ctx.indent -= 1;
        out += u8"}\n";
        return {anton::expected_value, ANTON_MOV(out)};
    }

    static anton::Expected<anton::String, anton::String>
    generate_fragment_stage(Context const& ctx, Codegen_Context& codegen_ctx, Pass_Stage_Declaration const& stage,
                            anton::Flat_Hash_Map<anton::String, Source_Definition> const& source_definitions) {
        anton::String const& pass_name = stage.pass->value;
        anton::String out;
        // Stringify the stage function
        stringify(out, *stage.return_type, codegen_ctx);
        out += u8" ";
        anton::String const stage_function_name = u8"_pass_" + pass_name + u8"_stage_fragment";
        out += stage_function_name;
        // param list
        out += u8"(";
        if(stage.params.size() > 0) {
            stringify(out, *stage.params[0], codegen_ctx);
            for(i64 i = 1; i != stage.params.size(); ++i) {
                out += u8", ";
                stringify(out, *stage.params[i], codegen_ctx);
            }
        }
        out += u8") {\n";
        codegen_ctx.indent += 1;
        for(auto& statement: stage.body) {
            stringify(out, *statement, codegen_ctx);
        }
        codegen_ctx.indent -= 1;
        out += u8"}\n\n";

        anton::Array<anton::String> arguments;
        bool const has_prev_stage_input = stage.params.size() > 0 && stage.params[0]->node_type == AST_Node_Type::ordinary_function_param;
        // Write input from the previous stage if the first parameter is an ordinary parameter
        if(has_prev_stage_input) {
            Ordinary_Function_Param const& param = (Ordinary_Function_Param const&)*stage.params[0];
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *param.type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                anton::String_View interpolation_str = stringify(m.interpolation);
                out += anton::format(u8"layout(location = {}) {} in {} _pass_{}_in_{}{};\n", location_str, interpolation_str, type_str, pass_name,
                                     param.identifier->value, m.name);
                location += m.location_slots;
            }
            out += u8"\n";
            arguments.emplace_back(u8"_arg0");
        }

        // Generate parameters
        for(i64 i = has_prev_stage_input; i < stage.params.size(); ++i) {
            Sourced_Function_Param const& node = (Sourced_Function_Param const&)*stage.params[i];
            auto iter = source_definitions.find(node.source->value);
            ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
            anton::String_View bind_string = iter->value.bind;
            anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, *node.identifier, *node.type);
            if(res) {
                arguments.emplace_back(ANTON_MOV(res.value()));
            } else {
                return {anton::expected_error, ANTON_MOV(res.error())};
            }
        }

        bool const return_type_is_void =
            stage.return_type->node_type == AST_Node_Type::builtin_type && ((Builtin_Type&)*stage.return_type).type == Builtin_GLSL_Type::glsl_void;
        // Decompose return type to individual outputs
        if(!return_type_is_void) {
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *stage.return_type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                out += anton::format(u8"layout(location = {}) out {} _pass_{}_out{};\n", location_str, type_str, pass_name, m.name);
                location += m.location_slots;
            }
            out += u8"\n";
        }

        // Output main
        out += u8"void main() {\n";
        codegen_ctx.indent += 1;

        // Build _arg0 (input from the previous stage aggregated into a struct) from fragment inputs
        if(has_prev_stage_input) {
            Ordinary_Function_Param const& param = (Ordinary_Function_Param const&)*stage.params[0];
            write_indent(out, codegen_ctx.indent);
            out += stringify_type(*param.type);
            out += u8" _arg0;\n";
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *param.type, members_info);
            for(Member_Info const& m: members_info) {
                write_indent(out, codegen_ctx.indent);
                out += anton::format(u8"_arg0{} = _pass_{}_in_{}{};\n", m.accessor, pass_name, param.identifier->value, m.name);
            }
        }

        // Write stage function call
        anton::String const shader_return_name{u8"_res"};
        write_indent(out, codegen_ctx.indent);
        if(!return_type_is_void) {
            stringify(out, *stage.return_type, codegen_ctx);
            out += u8" ";
            // Write result name
            out += shader_return_name;
            out += u8" = ";
        }

        out += stage_function_name;
        out += u8"(";
        if(arguments.size() > 0) {
            out += arguments[0];
            for(i64 i = 1; i < arguments.size(); ++i) {
                out += u8", ";
                out += arguments[i];
            }
        }
        out += u8");\n";

        if(!return_type_is_void) {
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *stage.return_type, members_info);
            for(Member_Info const& m: members_info) {
                write_indent(out, codegen_ctx.indent);
                out += anton::format(u8"_pass_{}_out{} = {}{};\n", pass_name, m.name, shader_return_name, m.accessor);
            }
        }

        codegen_ctx.indent -= 1;
        out += u8"}\n";
        return {anton::expected_value, ANTON_MOV(out)};
    }

    static anton::Expected<anton::String, anton::String>
    generate_compute_stage(Codegen_Context& codegen_ctx, Pass_Stage_Declaration const& stage,
                           anton::Flat_Hash_Map<anton::String, Source_Definition> const& source_definitions) {
        anton::String const& pass_name = stage.pass->value;
        anton::String out;
        // Stringify the stage function
        stringify(out, *stage.return_type, codegen_ctx);
        out += u8" ";
        anton::String const stage_function_name = u8"_pass_" + pass_name + u8"_stage_compute";
        out += stage_function_name;
        // param list
        out += u8"(";
        if(stage.params.size() > 0) {
            stringify(out, *stage.params[0], codegen_ctx);
            for(i64 i = 1; i != stage.params.size(); ++i) {
                out += u8", ";
                stringify(out, *stage.params[i], codegen_ctx);
            }
        }
        out += u8") {\n";
        codegen_ctx.indent += 1;
        for(auto& statement: stage.body) {
            stringify(out, *statement, codegen_ctx);
        }
        codegen_ctx.indent -= 1;
        out += u8"}\n\n";

        for(auto& attribute: stage.attributes) {
            switch(attribute->node_type) {
                case AST_Node_Type::workgroup_attribute: {
                    Workgroup_Attribute& attrib = (Workgroup_Attribute&)*attribute;
                    out += u8"layout(local_size_x = ";
                    stringify(out, *attrib.x, codegen_ctx);
                    if(attrib.y) {
                        out += u8", local_size_y = ";
                        stringify(out, *attrib.y, codegen_ctx);
                        if(attrib.z) {
                            out += u8", local_size_z = ";
                            stringify(out, *attrib.z, codegen_ctx);
                        }
                    }
                    out += u8") in;\n\n";
                } break;

                default:
                    break;
            }
        }

        // Output main
        out += u8"void main() {\n";
        codegen_ctx.indent += 1;

        // Write stage function call
        write_indent(out, codegen_ctx.indent);
        out += stage_function_name;
        out += u8"(";

        // Write arguments
        for(i64 i = 0; i < stage.params.size(); ++i) {
            Sourced_Function_Param const& node = (Sourced_Function_Param const&)*stage.params[i];
            auto iter = source_definitions.find(node.source->value);
            ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
            anton::String_View bind_string = iter->value.bind;
            anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, *node.identifier, *node.type);
            if(!res) {
                return {anton::expected_error, ANTON_MOV(res.error())};
            }

            if(i > 0) {
                out += u8", ";
            }

            out += res.value();
        }

        out += u8");\n";
        codegen_ctx.indent -= 1;
        out += u8"}\n";
        return {anton::expected_value, ANTON_MOV(out)};
    }

    anton::Expected<anton::Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Codegen_Data const& data) {
        Codegen_Context codegen_ctx{ctx, 0};
        anton::String stringified_extensions;
        if(data.extensions.size() > 0) {
            for(Extension const& extension: data.extensions) {
                stringified_extensions += u8"#extension ";
                stringified_extensions += extension.name;
                stringified_extensions += u8": ";
                switch(extension.behaviour) {
                    case Extension_Behaviour::require:
                        stringified_extensions += u8"require";
                        break;
                    case Extension_Behaviour::enable:
                        stringified_extensions += u8"enable";
                        break;
                    case Extension_Behaviour::warn:
                        stringified_extensions += u8"warn";
                        break;
                    case Extension_Behaviour::disable:
                        stringified_extensions += u8"disable";
                        break;
                }
                stringified_extensions += U'\n';
            }
            stringified_extensions += U'\n';
        }

        anton::String stringified_structs_and_consts;
        if(data.structs_and_constants.size() > 0) {
            for(Declaration const* const decl: data.structs_and_constants) {
                stringify(stringified_structs_and_consts, *decl, codegen_ctx);
            }

            stringified_structs_and_consts += u8"\n";
        }

        anton::String stringified_functions;
        if(data.functions.size() > 0) {
            for(Function_Declaration const* const decl: data.functions) {
                stringify_function_forward_decl(stringified_functions, *decl, codegen_ctx);
            }

            stringified_functions += u8"\n";

            for(Function_Declaration const* const decl: data.functions) {
                stringify(stringified_functions, *decl, codegen_ctx);
                stringified_functions += u8"\n";
            }
        }

        anton::Array<Pass_Data> pass_datas;
        for(Pass_Context const& pass: data.passes) {
            Pass_Settings const* const this_pass_settings =
                anton::find_if(data.settings.begin(), data.settings.end(), [&pass](Pass_Settings const& settings) { return settings.pass_name == pass.name; });
            anton::Flat_Hash_Map<anton::String, Source_Definition> source_definitions;
            for(auto [key, value]: pass.sourced_data) {
                // We have to make a lot of string copies so that we can expose the variables in the public api

                anton::Array<Sourced_Variable> variables{anton::reserve, value.variables.size()};
                for(Sourced_Function_Param const* const data: value.variables) {
                    anton::String type = stringify_type(*data->type);
                    bool const unsized = is_unsized_array(*data->type);
                    Sourced_Variable variable{data->identifier->value, ANTON_MOV(type), unsized};
                    variables.emplace_back(ANTON_MOV(variable));
                }

                anton::Array<Sourced_Opaque_Variable> opaque_variables{anton::reserve, value.opaque_variables.size()};
                for(Sourced_Function_Param const* const data: value.opaque_variables) {
                    anton::String type = stringify_type(*data->type);
                    bool const unsized = is_unsized_array(*data->type);
                    anton::String image_layout;
                    if(data->image_layout) {
                        image_layout = anton::String{stringify(data->image_layout->type)};
                    }
                    Sourced_Opaque_Variable variable{data->identifier->value, ANTON_MOV(type), ANTON_MOV(image_layout), unsized};
                    opaque_variables.emplace_back(ANTON_MOV(variable));
                }

                anton::Slice<Setting_Key_Value const> skv;
                if(this_pass_settings) {
                    skv = this_pass_settings->settings;
                }

                Source_Definition_Context src_def_ctx{pass.name, key, skv, variables, opaque_variables, ctx.source_definition_user_data};
                anton::Expected<Source_Definition, anton::String> result = ctx.source_definition_cb(src_def_ctx);
                if(result) {
                    source_definitions.emplace(key, ANTON_MOV(result.value()));
                } else {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }
            }

            anton::String stringified_sources;
            for(auto [source, def]: source_definitions) {
                stringified_sources += def.declaration;
                stringified_sources += U'\n';
            }

            Pass_Data& pass_data = pass_datas.emplace_back(Pass_Data{pass.name, {}});
            if(pass.vertex_stage) {
                anton::String out{"#version 460 core\n#pragma shader_stage(vertex)\n\n"};
                // write the common part
                out += stringified_extensions;
                out += stringified_structs_and_consts;
                out += stringified_sources;
                out += stringified_functions;
                anton::Expected<anton::String, anton::String> res = generate_vertex_stage(ctx, codegen_ctx, *pass.vertex_stage, source_definitions);
                if(!res) {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }
                out += res.value();
                pass_data.files.emplace_back(GLSL_File{ANTON_MOV(out), Stage_Type::vertex});
            }

            if(pass.fragment_stage) {
                anton::String out{"#version 460 core\n#pragma shader_stage(fragment)\n\n"};
                // write the common part
                out += stringified_extensions;
                out += stringified_structs_and_consts;
                out += stringified_sources;
                out += stringified_functions;
                anton::Expected<anton::String, anton::String> res = generate_fragment_stage(ctx, codegen_ctx, *pass.fragment_stage, source_definitions);
                if(!res) {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }
                out += res.value();
                pass_data.files.emplace_back(GLSL_File{ANTON_MOV(out), Stage_Type::fragment});
            }

            if(pass.compute_stage) {
                anton::String out{"#version 460 core\n#pragma shader_stage(compute)\n\n"};
                // write the common part
                out += stringified_extensions;
                out += stringified_structs_and_consts;
                out += stringified_sources;
                out += stringified_functions;
                anton::Expected<anton::String, anton::String> res = generate_compute_stage(codegen_ctx, *pass.compute_stage, source_definitions);
                if(!res) {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }
                out += res.value();
                pass_data.files.emplace_back(GLSL_File{ANTON_MOV(out), Stage_Type::compute});
            }
        }

        return {anton::expected_value, ANTON_MOV(pass_datas)};
    }
} // namespace vush
