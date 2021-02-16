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
        Format_Options format;
        anton::String_View current_pass;
        Stage_Type current_stage;
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
                Struct_Decl& struct_decl = (Struct_Decl&)*symbol->declaration;
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

    static void stringify(anton::String& out, AST_Node& ast_node, Codegen_Context& ctx) {
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
                stringify(out, *node.name, ctx);
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
                stringify(out, *node.name, ctx);
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
                write_indent(out, ctx.indent);
                If_Statement* node = (If_Statement*)&ast_node;
                while(true) {
                    out += u8"if(";
                    stringify(out, *node->condition, ctx);
                    out += u8") {\n";
                    ctx.indent += 1;
                    Block_Statement& true_statement = (Block_Statement&)*node->true_statement;
                    for(auto& statement: true_statement.statements) {
                        stringify(out, *statement, ctx);
                    }
                    ctx.indent -= 1;
                    // We keep iterating as long as the else branch exists and it's an if statement
                    if(node->false_statement && node->false_statement->node_type == AST_Node_Type::if_statement) {
                        write_indent(out, ctx.indent);
                        out += u8"} else ";
                        node = (If_Statement*)node->false_statement.get();
                    } else {
                        break;
                    }
                }

                if(node->false_statement) {
                    write_indent(out, ctx.indent);
                    out += u8"} else {\n";
                    ctx.indent += 1;
                    Block_Statement& false_statement = (Block_Statement&)*node->false_statement;
                    for(auto& statement: false_statement.statements) {
                        stringify(out, *statement, ctx);
                    }
                    ctx.indent -= 1;
                }
                write_indent(out, ctx.indent);
                out += u8"}\n";
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

            case AST_Node_Type::declaration_statement: {
                Declaration_Statement& node = (Declaration_Statement&)ast_node;
                write_indent(out, ctx.indent);
                stringify(out, *node.declaration, ctx);
                return;
            }

            case AST_Node_Type::expression_statement: {
                Expression_Statement& node = (Expression_Statement&)ast_node;
                write_indent(out, ctx.indent);
                stringify(out, *node.expr, ctx);
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
                if(ctx.format.space_around_operators) {
                    out += u8" ";
                }

                switch(node.type) {
                    case Binary_Expr_Type::logic_or:
                        out += u8"||";
                        break;
                    case Binary_Expr_Type::logic_xor:
                        out += u8"^^";
                        break;
                    case Binary_Expr_Type::logic_and:
                        out += u8"&&";
                        break;
                    case Binary_Expr_Type::equal:
                        out += u8"==";
                        break;
                    case Binary_Expr_Type::unequal:
                        out += u8"!=";
                        break;
                    case Binary_Expr_Type::greater_than:
                        out += u8">";
                        break;
                    case Binary_Expr_Type::less_than:
                        out += u8"<";
                        break;
                    case Binary_Expr_Type::greater_equal:
                        out += u8">=";
                        break;
                    case Binary_Expr_Type::less_equal:
                        out += u8"<=";
                        break;
                    case Binary_Expr_Type::bit_or:
                        out += u8"|";
                        break;
                    case Binary_Expr_Type::bit_xor:
                        out += u8"^";
                        break;
                    case Binary_Expr_Type::bit_and:
                        out += u8"&";
                        break;
                    case Binary_Expr_Type::lshift:
                        out += u8"<<";
                        break;
                    case Binary_Expr_Type::rshift:
                        out += u8">>";
                        break;
                    case Binary_Expr_Type::add:
                        out += u8"+";
                        break;
                    case Binary_Expr_Type::sub:
                        out += u8"-";
                        break;
                    case Binary_Expr_Type::mul:
                        out += u8"*";
                        break;
                    case Binary_Expr_Type::div:
                        out += u8"/";
                        break;
                    case Binary_Expr_Type::mod:
                        out += u8"%";
                        break;
                }

                if(ctx.format.space_around_operators) {
                    out += u8" ";
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

            case AST_Node_Type::argument_list: {
                Argument_List& node = (Argument_List&)ast_node;
                if(node.arguments.size() > 0) {
                    stringify(out, *node.arguments[0], ctx);

                    for(i64 i = 1; i != node.arguments.size(); ++i) {
                        out += u8", ";
                        stringify(out, *node.arguments[i], ctx);
                    }
                }
                return;
            }

            case AST_Node_Type::function_call_expression: {
                Function_Call_Expression& node = (Function_Call_Expression&)ast_node;
                stringify(out, *node.identifier, ctx);
                out += u8"(";
                stringify(out, *node.arg_list, ctx);
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

            case AST_Node_Type::declaration_if:
            case AST_Node_Type::import_decl:
            case AST_Node_Type::function_param_if:
            case AST_Node_Type::pass_stage_declaration:
            case AST_Node_Type::expression_if:
            case AST_Node_Type::string_literal:
            case AST_Node_Type::case_statement:
            case AST_Node_Type::default_case_statement:
                break;
        }
    }

    static void stringify_function_forward_decl(anton::String& out, Function_Declaration& node, Codegen_Context& ctx) {
        stringify(out, *node.return_type, ctx);
        out += u8" ";
        stringify(out, *node.name, ctx);
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

    struct Sourced_Data {
        Type* type;
        Identifier* name;
        Identifier* source;
    };

    struct Sourced_Data_Buffers {
        anton::Array<Sourced_Data> all;
        anton::Array<Sourced_Data> variables;
        anton::Array<Sourced_Data> opaque_variables;
        anton::Array<Sourced_Data> unsized_variables;
    };

    struct Pass_Context {
        anton::String name;
        // Maps source name to sourced params and globals
        anton::Flat_Hash_Map<anton::String, Sourced_Data_Buffers> sourced_data;
        Pass_Stage_Declaration* vertex_stage = nullptr;
        Pass_Stage_Declaration* fragment_stage = nullptr;
        Pass_Stage_Declaration* compute_stage = nullptr;
    };

    // TODO: Arrays in vertex inputs/fragment outputs are illegal (for now). Add error handling.

    struct Member_Info {
        anton::String name;
        anton::String accessor;
        Builtin_GLSL_Type type;
        i64 location_slots;
        Interpolation interpolation;
        bool invariant;
    };

    // explode_type
    //
    static void explode_type(Context const& ctx, Type const& type, anton::Array<Member_Info>& member_info) {
        auto _explode_type = [](auto& _explode_type, Context const& ctx, Type const& type, anton::Array<anton::String>& name_components,
                                Interpolation parent_interpolation, bool parent_invariant, anton::Array<Member_Info>& member_info) -> void {
            ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type, "unknown ast node type");
            if(type.node_type == AST_Node_Type::user_defined_type) {
                User_Defined_Type const& node = (User_Defined_Type const&)type;
                Symbol const* symbol = find_symbol(ctx, node.name);
                ANTON_ASSERT(symbol, "undefined symbol");
                Struct_Decl const* struct_decl = (Struct_Decl const*)symbol->declaration;
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

    static anton::Expected<anton::String, anton::String> format_bind_string(anton::String_View string, Sourced_Data const& symbol) {
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
                out += symbol.name->value;
            } else if(property_name == u8"type") {
                out += stringify_type(*symbol.type);
            } else {
                return {anton::expected_error, anton::String(u8"error: unknown property name in bind string")};
            }

            // Skip the terminating }}
            iter2 = iter2 + 2;
            iter1 = iter2;
        }

        return {anton::expected_value, ANTON_MOV(out)};
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

                case Builtin_GLSL_Type::glsl_sampler1D:
                case Builtin_GLSL_Type::glsl_texture1D:
                case Builtin_GLSL_Type::glsl_image1D:
                case Builtin_GLSL_Type::glsl_sampler1DShadow:
                case Builtin_GLSL_Type::glsl_sampler1DArray:
                case Builtin_GLSL_Type::glsl_texture1DArray:
                case Builtin_GLSL_Type::glsl_image1DArray:
                case Builtin_GLSL_Type::glsl_sampler1DArrayShadow:
                case Builtin_GLSL_Type::glsl_sampler2D:
                case Builtin_GLSL_Type::glsl_texture2D:
                case Builtin_GLSL_Type::glsl_image2D:
                case Builtin_GLSL_Type::glsl_sampler2DShadow:
                case Builtin_GLSL_Type::glsl_sampler2DArray:
                case Builtin_GLSL_Type::glsl_texture2DArray:
                case Builtin_GLSL_Type::glsl_image2DArray:
                case Builtin_GLSL_Type::glsl_sampler2DArrayShadow:
                case Builtin_GLSL_Type::glsl_sampler2DMS:
                case Builtin_GLSL_Type::glsl_texture2DMS:
                case Builtin_GLSL_Type::glsl_image2DMS:
                case Builtin_GLSL_Type::glsl_sampler2DMSArray:
                case Builtin_GLSL_Type::glsl_texture2DMSArray:
                case Builtin_GLSL_Type::glsl_image2DMSArray:
                case Builtin_GLSL_Type::glsl_sampler2DRect:
                case Builtin_GLSL_Type::glsl_texture2DRect:
                case Builtin_GLSL_Type::glsl_image2DRect:
                case Builtin_GLSL_Type::glsl_sampler2DRectShadow:
                case Builtin_GLSL_Type::glsl_sampler3D:
                case Builtin_GLSL_Type::glsl_texture3D:
                case Builtin_GLSL_Type::glsl_image3D:
                case Builtin_GLSL_Type::glsl_samplerCube:
                case Builtin_GLSL_Type::glsl_textureCube:
                case Builtin_GLSL_Type::glsl_imageCube:
                case Builtin_GLSL_Type::glsl_samplerCubeShadow:
                case Builtin_GLSL_Type::glsl_samplerCubeArray:
                case Builtin_GLSL_Type::glsl_textureCubeArray:
                case Builtin_GLSL_Type::glsl_imageCubeArray:
                case Builtin_GLSL_Type::glsl_samplerCubeArrayShadow:
                case Builtin_GLSL_Type::glsl_samplerBuffer:
                case Builtin_GLSL_Type::glsl_textureBuffer:
                case Builtin_GLSL_Type::glsl_imageBuffer:
                case Builtin_GLSL_Type::glsl_subpassInput:
                case Builtin_GLSL_Type::glsl_subpassInputMS:
                case Builtin_GLSL_Type::glsl_isampler1D:
                case Builtin_GLSL_Type::glsl_itexture1D:
                case Builtin_GLSL_Type::glsl_iimage1D:
                case Builtin_GLSL_Type::glsl_isampler1DArray:
                case Builtin_GLSL_Type::glsl_itexture1DArray:
                case Builtin_GLSL_Type::glsl_iimage1DArray:
                case Builtin_GLSL_Type::glsl_isampler2D:
                case Builtin_GLSL_Type::glsl_itexture2D:
                case Builtin_GLSL_Type::glsl_iimage2D:
                case Builtin_GLSL_Type::glsl_isampler2DArray:
                case Builtin_GLSL_Type::glsl_itexture2DArray:
                case Builtin_GLSL_Type::glsl_iimage2DArray:
                case Builtin_GLSL_Type::glsl_isampler2DMS:
                case Builtin_GLSL_Type::glsl_itexture2DMS:
                case Builtin_GLSL_Type::glsl_iimage2DMS:
                case Builtin_GLSL_Type::glsl_isampler2DMSArray:
                case Builtin_GLSL_Type::glsl_itexture2DMSArray:
                case Builtin_GLSL_Type::glsl_iimage2DMSArray:
                case Builtin_GLSL_Type::glsl_isampler2DRect:
                case Builtin_GLSL_Type::glsl_itexture2DRect:
                case Builtin_GLSL_Type::glsl_iimage2DRect:
                case Builtin_GLSL_Type::glsl_isampler3D:
                case Builtin_GLSL_Type::glsl_itexture3D:
                case Builtin_GLSL_Type::glsl_iimage3D:
                case Builtin_GLSL_Type::glsl_isamplerCube:
                case Builtin_GLSL_Type::glsl_itextureCube:
                case Builtin_GLSL_Type::glsl_iimageCube:
                case Builtin_GLSL_Type::glsl_isamplerCubeArray:
                case Builtin_GLSL_Type::glsl_itextureCubeArray:
                case Builtin_GLSL_Type::glsl_iimageCubeArray:
                case Builtin_GLSL_Type::glsl_isamplerBuffer:
                case Builtin_GLSL_Type::glsl_itextureBuffer:
                case Builtin_GLSL_Type::glsl_iimageBuffer:
                case Builtin_GLSL_Type::glsl_isubpassInput:
                case Builtin_GLSL_Type::glsl_isubpassInputMS:
                case Builtin_GLSL_Type::glsl_usampler1D:
                case Builtin_GLSL_Type::glsl_utexture1D:
                case Builtin_GLSL_Type::glsl_uimage1D:
                case Builtin_GLSL_Type::glsl_usampler1DArray:
                case Builtin_GLSL_Type::glsl_utexture1DArray:
                case Builtin_GLSL_Type::glsl_uimage1DArray:
                case Builtin_GLSL_Type::glsl_usampler2D:
                case Builtin_GLSL_Type::glsl_utexture2D:
                case Builtin_GLSL_Type::glsl_uimage2D:
                case Builtin_GLSL_Type::glsl_usampler2DArray:
                case Builtin_GLSL_Type::glsl_utexture2DArray:
                case Builtin_GLSL_Type::glsl_uimage2DArray:
                case Builtin_GLSL_Type::glsl_usampler2DMS:
                case Builtin_GLSL_Type::glsl_utexture2DMS:
                case Builtin_GLSL_Type::glsl_uimage2DMS:
                case Builtin_GLSL_Type::glsl_usampler2DMSArray:
                case Builtin_GLSL_Type::glsl_utexture2DMSArray:
                case Builtin_GLSL_Type::glsl_uimage2DMSArray:
                case Builtin_GLSL_Type::glsl_usampler2DRect:
                case Builtin_GLSL_Type::glsl_utexture2DRect:
                case Builtin_GLSL_Type::glsl_uimage2DRect:
                case Builtin_GLSL_Type::glsl_usampler3D:
                case Builtin_GLSL_Type::glsl_utexture3D:
                case Builtin_GLSL_Type::glsl_uimage3D:
                case Builtin_GLSL_Type::glsl_usamplerCube:
                case Builtin_GLSL_Type::glsl_utextureCube:
                case Builtin_GLSL_Type::glsl_uimageCube:
                case Builtin_GLSL_Type::glsl_usamplerCubeArray:
                case Builtin_GLSL_Type::glsl_utextureCubeArray:
                case Builtin_GLSL_Type::glsl_uimageCubeArray:
                case Builtin_GLSL_Type::glsl_usamplerBuffer:
                case Builtin_GLSL_Type::glsl_utextureBuffer:
                case Builtin_GLSL_Type::glsl_uimageBuffer:
                case Builtin_GLSL_Type::glsl_usubpassInput:
                case Builtin_GLSL_Type::glsl_usubpassInputMS:
                case Builtin_GLSL_Type::glsl_sampler:
                case Builtin_GLSL_Type::glsl_samplerShadow:
                    return {0, 0};
            }
        } else if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& t = (User_Defined_Type const&)type;
            Symbol const* symbol = find_symbol(ctx, t.name);
            Struct_Decl const* struct_decl = (Struct_Decl const*)symbol->declaration;
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

    anton::Expected<anton::Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Declaration_List& declarations, Format_Options const& format,
                                                                          anton::Slice<Extension const> const extensions,
                                                                          anton::Slice<Pass_Settings const> const settings) {
        // TODO: Move all validation, transformations and aggregation into vush.cpp

        anton::Array<Declaration*> structs_and_consts;
        anton::Array<Declaration*> functions;
        anton::Array<Pass_Context> passes;

        {
            anton::Flat_Hash_Map<anton::String, anton::Array<Sourced_Global_Decl*>> globals;
            for(auto& decl: declarations) {
                switch(decl->node_type) {
                    case AST_Node_Type::struct_decl:
                    case AST_Node_Type::constant_declaration: {
                        structs_and_consts.emplace_back(decl.get());
                    } break;

                    case AST_Node_Type::function_declaration: {
                        functions.emplace_back(decl.get());
                    } break;

                    case AST_Node_Type::pass_stage_declaration: {
                        Pass_Stage_Declaration* pass_decl = (Pass_Stage_Declaration*)decl.get();
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
                                    Source_Info const& src1 = pass_decl->source_info;
                                    Source_Info const& src2 = pass->vertex_stage->source_info;
                                    return {anton::expected_error, format_duplicate_pass_stage_error(src1, src2, pass->name, Stage_Type::vertex)};
                                }

                                pass->vertex_stage = pass_decl;
                            } break;

                            case Stage_Type::fragment: {
                                if(pass->fragment_stage) {
                                    Source_Info const& src1 = pass_decl->source_info;
                                    Source_Info const& src2 = pass->vertex_stage->source_info;
                                    return {anton::expected_error, format_duplicate_pass_stage_error(src1, src2, pass->name, Stage_Type::fragment)};
                                }

                                pass->fragment_stage = pass_decl;
                            } break;

                            case Stage_Type::compute: {
                                if(pass->compute_stage) {
                                    Source_Info const& src1 = pass_decl->source_info;
                                    Source_Info const& src2 = pass->vertex_stage->source_info;
                                    return {anton::expected_error, format_duplicate_pass_stage_error(src1, src2, pass->name, Stage_Type::compute)};
                                }

                                pass->compute_stage = pass_decl;
                            } break;
                        }

                        for(auto& param: pass_decl->params) {
                            if(param->node_type == AST_Node_Type::sourced_function_param) {
                                Sourced_Function_Param* sourced_param = (Sourced_Function_Param*)param.get();
                                auto iter = pass->sourced_data.find_or_emplace(sourced_param->source->value);
                                Sourced_Data data{sourced_param->type.get(), sourced_param->identifier.get(), sourced_param->source.get()};
                                iter->value.all.emplace_back(data);
                            }
                        }
                    } break;

                    case AST_Node_Type::sourced_global_decl: {
                        Sourced_Global_Decl* global = (Sourced_Global_Decl*)decl.get();
                        anton::String const& pass_name = global->pass_name->value;
                        auto iter = globals.find_or_emplace(pass_name);
                        iter->value.emplace_back(global);
                    } break;

                    default:
                        break;
                }
            }

            for(auto [pass_name, data]: globals) {
                anton::String const& pn = pass_name;
                Pass_Context* const pass = anton::find_if(passes.begin(), passes.end(), [&pn](Pass_Context& pass) { return pass.name == pn; });
                if(pass != passes.end()) {
                    for(Sourced_Global_Decl* global: data) {
                        auto iter = pass->sourced_data.find_or_emplace(global->source->value);
                        Sourced_Data data{global->type.get(), global->name.get(), global->source.get()};
                        iter->value.all.emplace_back(data);
                    }
                } else {
                    // TODO: Improve this diagnostic
                    return {anton::expected_error, format_sourced_global_pass_does_not_exist(*data[0])};
                }
            }
        }

        for(Pass_Context& pass: passes) {
            // Validate that a pass has a vertex stage and optionally fragment stage or a compute stage
            if(!pass.compute_stage) {
                if(!pass.vertex_stage) {
                    return {anton::expected_error, format_missing_vertex_stage_error(pass.name)};
                }
            } else {
                if(pass.vertex_stage || pass.fragment_stage) {
                    return {anton::expected_error, format_vertex_and_compute_stages_error(pass.name)};
                }
            }

            // Remove duplicates, validate there is no different-type-same-name sourced data, optimize layout
            for(auto& [source_name, data]: pass.sourced_data) {
                // TODO: Use stable sort to preserve the order and report duplicates in the correct order.
                anton::quick_sort(data.all.begin(), data.all.end(), [](Sourced_Data const& lhs, Sourced_Data const& rhs) {
                    anton::String const& lhs_str = lhs.name->value;
                    anton::String const& rhs_str = rhs.name->value;
                    return anton::compare(lhs_str, rhs_str) == -1;
                });

                // Ensure there are no name duplicates with different types
                for(auto i = data.all.begin(), j = data.all.begin() + 1, end = data.all.end(); j != end; ++i, ++j) {
                    anton::String const i_type = stringify_type(*i->type);
                    anton::String const j_type = stringify_type(*j->type);
                    if(i->name->value == j->name->value && i_type != j_type) {
                        Source_Info const& src = j->name->source_info;
                        return {anton::expected_error,
                                build_error_message(src.file_path, src.line, src.column, u8"duplicate sourced parameter name with a different type")};
                    }
                }

                // Remove type duplicates
                auto end = anton::unique(data.all.begin(), data.all.end(), [](Sourced_Data const& lhs, Sourced_Data const& rhs) {
                    anton::String const i_type = stringify_type(*lhs.type);
                    anton::String const j_type = stringify_type(*rhs.type);
                    return i_type == j_type && lhs.name->value == rhs.name->value;
                });

                data.all.erase(end, data.all.end());

                // Copy Sourced_Data to specialized buffers
                for(auto i = data.all.begin(), end = data.all.end(); i != end; ++i) {
                    bool const opaque = is_opaque_type(*i->type);
                    bool const unsized = is_unsized_array(*i->type);
                    if(!opaque && !unsized) {
                        data.variables.emplace_back(*i);
                    } else if(opaque && !unsized) {
                        data.opaque_variables.emplace_back(*i);
                    } else {
                        data.unsized_variables.emplace_back(*i);
                    }
                }

                // Optimize layout of variables
                i64 const variables_count = data.variables.size();
                anton::Array<Layout_Info> layout_info{anton::reserve, variables_count};
                for(Sourced_Data const& d: data.variables) {
                    Layout_Info info = calculate_type_layout_info(ctx, *d.type);
                    layout_info.emplace_back(info);
                }
                // Create a permutation that will sort by alignment
                anton::Array<i64> indices{variables_count, 0};
                anton::fill_with_consecutive(indices.begin(), indices.end(), 0);
                anton::quick_sort(indices.begin(), indices.end(),
                                  [&layout_info](i64 const lhs, i64 const rhs) { return layout_info[lhs].alignment > layout_info[rhs].alignment; });
                // Apply the permutation
                {
                    anton::Array<Sourced_Data> perm_data{variables_count};
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

        Codegen_Context codegen_ctx{ctx};
        codegen_ctx.format = format;
        codegen_ctx.indent = 0;

        anton::String stringified_extensions;
        if(extensions.size() > 0) {
            for(Extension const& extension: extensions) {
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
        if(structs_and_consts.size() > 0) {
            for(Declaration* decl: structs_and_consts) {
                stringify(stringified_structs_and_consts, *decl, codegen_ctx);
            }

            stringified_structs_and_consts += u8"\n";
        }

        anton::String stringified_functions;
        if(functions.size() > 0) {
            for(Declaration* decl: functions) {
                stringify_function_forward_decl(stringified_functions, (Function_Declaration&)*decl, codegen_ctx);
            }

            stringified_functions += u8"\n";
        }

        for(Declaration* decl: functions) {
            stringify(stringified_functions, (Function_Declaration&)*decl, codegen_ctx);
            stringified_functions += u8"\n";
        }

        anton::Array<Pass_Data> pass_data;
        for(Pass_Context& pass: passes) {
            Pass_Settings const* const this_pass_settings =
                anton::find_if(settings.begin(), settings.end(), [&pass](Pass_Settings const& settings) { return settings.pass_name == pass.name; });
            anton::Flat_Hash_Map<anton::String, Source_Definition> source_definitions;
            for(auto [key, value]: pass.sourced_data) {
                // We have to make a lot of string copies so that we can expose the variables in the public api

                anton::Array<Sourced_Variable> variables{anton::reserve, value.variables.size()};
                for(Sourced_Data const& data: value.variables) {
                    anton::String type = stringify_type(*data.type);
                    variables.emplace_back(data.name->value, ANTON_MOV(type));
                }

                anton::Array<Sourced_Variable> opaque_variables{anton::reserve, value.opaque_variables.size()};
                for(Sourced_Data const& data: value.opaque_variables) {
                    anton::String type = stringify_type(*data.type);
                    opaque_variables.emplace_back(data.name->value, ANTON_MOV(type));
                }

                anton::Array<Sourced_Variable> unsized_variables{anton::reserve, value.unsized_variables.size()};
                for(Sourced_Data const& data: value.unsized_variables) {
                    anton::String type = stringify_type(*data.type);
                    unsized_variables.emplace_back(data.name->value, ANTON_MOV(type));
                }

                anton::Slice<Setting_Key_Value const> skv;
                if(this_pass_settings) {
                    skv = this_pass_settings->settings;
                }

                Source_Definition_Context src_def_ctx{pass.name, key, skv, variables, opaque_variables, unsized_variables, ctx.source_definition_user_data};
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

            // I'm too lazy to rewrite this code properly, so I just stuff all the stages into an array and reuse the old code
            anton::Array<Pass_Stage_Declaration*> stages;
            if(pass.vertex_stage) {
                stages.emplace_back(pass.vertex_stage);
            }
            if(pass.fragment_stage) {
                stages.emplace_back(pass.fragment_stage);
            }
            if(pass.compute_stage) {
                stages.emplace_back(pass.compute_stage);
            }

            for(Pass_Stage_Declaration* stage: stages) {
                codegen_ctx.current_pass = stage->pass->value;
                codegen_ctx.current_stage = stage->stage;

                anton::String out = anton::String("#version 460 core\n#pragma shader_stage(") + stringify(stage->stage) + ")\n\n";
                // write the common part
                out += stringified_extensions;
                out += stringified_structs_and_consts;
                out += stringified_sources;
                out += stringified_functions;

                // Stringify the stage function
                anton::String const stage_function_name = u8"_pass_" + stage->pass->value + u8"_stage_" + stringify(stage->stage);
                stringify(out, *stage->return_type, codegen_ctx);
                out += u8" ";
                out += stage_function_name;
                // param list
                out += u8"(";
                if(stage->params.size() > 0) {
                    stringify(out, *stage->params[0], codegen_ctx);
                    for(i64 i = 1; i != stage->params.size(); ++i) {
                        out += u8", ";
                        stringify(out, *stage->params[i], codegen_ctx);
                    }
                }
                out += u8") {\n";
                codegen_ctx.indent += 1;
                for(auto& statement: stage->body) {
                    stringify(out, *statement, codegen_ctx);
                }
                codegen_ctx.indent -= 1;
                out += u8"}\n\n";

                bool const return_type_is_void =
                    stage->return_type->node_type == AST_Node_Type::builtin_type && ((Builtin_Type&)*stage->return_type).type == Builtin_GLSL_Type::glsl_void;

                switch(stage->stage) {
                    case Stage_Type::vertex: {
                        // write vertex inputs
                        {
                            anton::Array<Member_Info> members_info;
                            for(auto& param: stage->params) {
                                if(param->node_type == AST_Node_Type::vertex_input_param) {
                                    Owning_Ptr<Vertex_Input_Param>& node = (Owning_Ptr<Vertex_Input_Param>&)param;
                                    explode_type(ctx, *node->type, members_info);
                                    // prepend variable name to all names and accessors
                                    i64 location = 0;
                                    for(Member_Info const& m: members_info) {
                                        out += u8"layout(location = ";
                                        out += anton::to_string(location);
                                        out += u8") in ";
                                        out += stringify(m.type);
                                        out += u8" _pass_";
                                        out += stage->pass->value;
                                        out += u8"_";
                                        out += node->identifier->value;
                                        out += m.name;
                                        out += u8";\n";
                                        location += m.location_slots;
                                    }
                                    members_info.clear();
                                }
                            }
                            out += u8"\n";
                        }

                        if(!return_type_is_void) {
                            // Write vertex outputs
                            anton::Array<Member_Info> members_info;
                            explode_type(ctx, *stage->return_type, members_info);
                            i64 location = 0;
                            for(Member_Info const& m: members_info) {
                                anton::String location_str = anton::to_string(location);
                                anton::String_View type_str = stringify(m.type);
                                out += anton::format(u8"layout(location = {}) out {} _pass_{}_out{};\n", location_str, type_str, stage->pass->value, m.name);
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
                            for(auto& param: stage->params) {
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
                                            out += arg_name;
                                            out += m.accessor;
                                            out += u8" = _pass_";
                                            out += stage->pass->value;
                                            out += u8"_";
                                            out += node->identifier->value;
                                            out += m.name;
                                            out += u8";\n";
                                        }
                                        members_info.clear();
                                        arguments.emplace_back(ANTON_MOV(arg_name));
                                        param_index += 1;
                                    } break;

                                    case AST_Node_Type::sourced_function_param: {
                                        Sourced_Function_Param* const node = (Sourced_Function_Param*)param.get();
                                        auto iter = source_definitions.find(node->source->value);
                                        ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
                                        Sourced_Data data{node->type.get(), node->identifier.get(), node->source.get()};
                                        anton::String_View bind_string = iter->value.bind;
                                        anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, data);
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
                            out += stringify_type(*stage->return_type);
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
                            explode_type(ctx, *stage->return_type, members_info);
                            for(Member_Info const& m: members_info) {
                                write_indent(out, codegen_ctx.indent);
                                out += anton::format(u8"_pass_{}_out{} = _res{};\n", stage->pass->value, m.name, m.accessor);
                            }
                        }

                        codegen_ctx.indent -= 1;
                        out += u8"}\n";
                    } break;

                    case Stage_Type::fragment: {
                        anton::Array<anton::String> arguments;
                        bool const has_prev_stage_input = stage->params.size() > 0 && stage->params[0]->node_type == AST_Node_Type::ordinary_function_param;
                        // Write input from the previous stage if the first parameter is an ordinary parameter
                        if(has_prev_stage_input) {
                            Ordinary_Function_Param const& param = (Ordinary_Function_Param const&)*stage->params[0];
                            anton::Array<Member_Info> members_info;
                            explode_type(ctx, *param.type, members_info);
                            i64 location = 0;
                            for(Member_Info const& m: members_info) {
                                anton::String location_str = anton::to_string(location);
                                anton::String_View type_str = stringify(m.type);
                                anton::String_View interpolation_str = stringify(m.interpolation);
                                out += anton::format(u8"layout(location = {}) {} in {} _pass_{}_in_{}{};\n", location_str, interpolation_str, type_str,
                                                     stage->pass->value, param.identifier->value, m.name);
                                location += m.location_slots;
                            }
                            out += u8"\n";
                            arguments.emplace_back(u8"_arg0");
                        }

                        // Generate parameters
                        for(i64 i = has_prev_stage_input; i < stage->params.size(); ++i) {
                            ANTON_ASSERT(stage->params[i]->node_type == AST_Node_Type::sourced_function_param, u8"invalid parameter type");
                            Sourced_Function_Param const* param = (Sourced_Function_Param const*)stage->params[i].get();
                            auto iter = source_definitions.find(param->source->value);
                            ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
                            Sourced_Data data{param->type.get(), param->identifier.get(), param->source.get()};
                            anton::String_View bind_string = iter->value.bind;
                            anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, data);
                            if(res) {
                                arguments.emplace_back(ANTON_MOV(res.value()));
                            } else {
                                return {anton::expected_error, ANTON_MOV(res.error())};
                            }
                        }

                        // Decompose return type to individual outputs
                        if(!return_type_is_void) {
                            anton::Array<Member_Info> members_info;
                            explode_type(ctx, *stage->return_type, members_info);
                            i64 location = 0;
                            for(Member_Info const& m: members_info) {
                                out += u8"layout(location = ";
                                out += anton::to_string(location);
                                out += u8") out ";
                                out += stringify(m.type);
                                out += u8" _pass_";
                                out += stage->pass->value;
                                out += u8"_out";
                                out += m.name;
                                out += u8";\n";
                                location += m.location_slots;
                            }
                            out += u8"\n";
                        }

                        // Output main
                        out += u8"void main() {\n";
                        codegen_ctx.indent += 1;

                        // Build _arg0 from fragment inputs
                        if(has_prev_stage_input) {
                            Ordinary_Function_Param const& param = (Ordinary_Function_Param const&)*stage->params[0];
                            write_indent(out, codegen_ctx.indent);
                            out += stringify_type(*param.type);
                            out += u8" _arg0;\n";
                            anton::Array<Member_Info> members_info;
                            explode_type(ctx, *param.type, members_info);
                            for(Member_Info const& m: members_info) {
                                write_indent(out, codegen_ctx.indent);
                                out += anton::format(u8"_arg0{} = _pass_{}_in_{}{};\n", m.accessor, stage->pass->value, param.identifier->value, m.name);
                            }
                        }

                        // Write stage function call
                        anton::String const shader_return_name{u8"_res"};
                        write_indent(out, codegen_ctx.indent);
                        if(!return_type_is_void) {
                            out += stringify_type(*stage->return_type);
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
                            explode_type(ctx, *stage->return_type, members_info);
                            for(Member_Info const& m: members_info) {
                                write_indent(out, codegen_ctx.indent);
                                out += u8"_pass_";
                                out += stage->pass->value;
                                out += u8"_out";
                                out += m.name;
                                out += u8" = ";
                                out += shader_return_name;
                                out += m.accessor;
                                out += u8";\n";
                            }
                        }

                        codegen_ctx.indent -= 1;
                        out += u8"}\n";
                    } break;

                    case Stage_Type::compute: {
                        for(auto& attribute: stage->attributes) {
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
                        i64 i = 0;
                        for(auto& param: stage->params) {
                            ANTON_ASSERT(param->node_type == AST_Node_Type::sourced_function_param, u8"invalid parameter type");
                            Sourced_Function_Param* const node = (Sourced_Function_Param*)param.get();
                            auto iter = source_definitions.find(node->source->value);
                            ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
                            Sourced_Data data{node->type.get(), node->identifier.get(), node->source.get()};
                            anton::String_View bind_string = iter->value.bind;
                            anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, data);
                            if(res) {
                                if (i > 0) {
                                    out += u8", ";
                                }
                                out += res.value();
                                i++;
                            } else {
                                return {anton::expected_error, ANTON_MOV(res.error())};
                            }
                        }

                        out += u8");\n";
                        codegen_ctx.indent -= 1;
                        out += u8"}\n";
                    } break;
                }

                // TODO: We don't have to search because we're processing a single pass
                auto i = anton::find_if(pass_data.begin(), pass_data.end(),
                                        [cur_pass = codegen_ctx.current_pass](Pass_Data const& v) { return v.name == cur_pass; });
                if(i == pass_data.end()) {
                    Pass_Data& v = pass_data.emplace_back(Pass_Data{anton::String{codegen_ctx.current_pass}, {}});
                    i = &v;
                }

                i->files.emplace_back(GLSL_File{ANTON_MOV(out), codegen_ctx.current_stage});
            }
        }

        return {anton::expected_value, ANTON_MOV(pass_data)};
    }
} // namespace vush
