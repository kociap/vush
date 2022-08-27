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
    using namespace anton::literals;

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
                out += t.identifier;
                out += u8"(";
                // We made sure that the symbol exists during validation stage
                Symbol const* symbol = find_symbol(codegen_ctx.ctx, t.identifier);
                Struct_Declaration const& struct_declaration = (Struct_Declaration const&)*symbol;
                for(i64 i = 0; i < struct_declaration.members.size(); ++i) {
                    if(i != 0) {
                        out += u8", ";
                    }

                    auto& member = struct_declaration.members[i];
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
                out += stringify_type(codegen_ctx.ctx.allocator, t);
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
                out += node.identifier;
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

            case AST_Node_Type::struct_declaration: {
                Struct_Declaration& node = (Struct_Declaration&)ast_node;
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
                if(node.parameters.size() > 0) {
                    stringify(out, *node.parameters[0], ctx);
                    for(i64 i = 1; i != node.parameters.size(); ++i) {
                        out += u8", ";
                        stringify(out, *node.parameters[i], ctx);
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

            case AST_Node_Type::function_parameter: {
                Function_Parameter& node = (Function_Parameter&)ast_node;
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
                stringify(out, *node.match_expression, ctx);
                out += u8") {\n";
                ctx.indent += 1;
                for(auto& s: node.cases) {
                    for(auto& label: s->labels) {
                        write_indent(out, ctx.indent);
                        if(label->node_type == AST_Node_Type::default_expression) {
                            out += u8"default:\n"_sv;
                        } else {
                            out += u8"case "_sv;
                            stringify(out, *label, ctx);
                            out += u8":\n";
                        }
                    }

                    write_indent(out, ctx.indent);
                    out += u8"{\n"_sv;
                    ctx.indent += 1;
                    for(auto& statement: s->statements) {
                        stringify(out, *statement, ctx);
                    }
                    write_indent(out, ctx.indent);
                    out += u8"break;\n"_sv;
                    ctx.indent -= 1;
                    write_indent(out, ctx.indent);
                    out += u8"}\n"_sv;
                }
                ctx.indent -= 1;
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
                if(node.return_expression) {
                    out += u8" ";
                    stringify(out, *node.return_expression, ctx);
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

            case AST_Node_Type::binary_expression: {
                Binary_Expression& node = (Binary_Expression&)ast_node;
                stringify(out, *node.lhs, ctx);
                switch(node.type) {
                    case Binary_Expression_Type::assign:
                        out += "="_sv;
                        break;
                    case Binary_Expression_Type::add_assign:
                        out += "+="_sv;
                        break;
                    case Binary_Expression_Type::sub_assign:
                        out += "-="_sv;
                        break;
                    case Binary_Expression_Type::mul_assign:
                        out += "*="_sv;
                        break;
                    case Binary_Expression_Type::div_assign:
                        out += "/="_sv;
                        break;
                    case Binary_Expression_Type::mod_assign:
                        out += "%="_sv;
                        break;
                    case Binary_Expression_Type::shl_assign:
                        out += "<<="_sv;
                        break;
                    case Binary_Expression_Type::shr_assign:
                        out += ">>="_sv;
                        break;
                    case Binary_Expression_Type::band_assign:
                        out += "&="_sv;
                        break;
                    case Binary_Expression_Type::bor_assign:
                        out += "|="_sv;
                        break;
                    case Binary_Expression_Type::bxor_assign:
                        out += "^="_sv;
                        break;
                    case Binary_Expression_Type::lor:
                        out += "||"_sv;
                        break;
                    case Binary_Expression_Type::lxor:
                        out += "^^"_sv;
                        break;
                    case Binary_Expression_Type::land:
                        out += "&&"_sv;
                        break;
                    case Binary_Expression_Type::eq:
                        out += "=="_sv;
                        break;
                    case Binary_Expression_Type::neq:
                        out += "!="_sv;
                        break;
                    case Binary_Expression_Type::gt:
                        out += ">"_sv;
                        break;
                    case Binary_Expression_Type::lt:
                        out += "<"_sv;
                        break;
                    case Binary_Expression_Type::gteq:
                        out += ">="_sv;
                        break;
                    case Binary_Expression_Type::lteq:
                        out += "<="_sv;
                        break;
                    case Binary_Expression_Type::bor:
                        out += "|"_sv;
                        break;
                    case Binary_Expression_Type::bxor:
                        out += "^"_sv;
                        break;
                    case Binary_Expression_Type::band:
                        out += "&"_sv;
                        break;
                    case Binary_Expression_Type::shl:
                        out += "<<"_sv;
                        break;
                    case Binary_Expression_Type::shr:
                        out += ">>"_sv;
                        break;
                    case Binary_Expression_Type::add:
                        out += "+"_sv;
                        break;
                    case Binary_Expression_Type::sub:
                        out += "-"_sv;
                        break;
                    case Binary_Expression_Type::mul:
                        out += "*"_sv;
                        break;
                    case Binary_Expression_Type::div:
                        out += "/"_sv;
                        break;
                    case Binary_Expression_Type::mod:
                        out += "%"_sv;
                        break;
                }
                stringify(out, *node.rhs, ctx);
                return;
            }

            case AST_Node_Type::prefix_expression: {
                Prefix_Expression& node = (Prefix_Expression&)ast_node;
                switch(node.type) {
                    case Prefix_Expression_Type::plus:
                        // Do not emit anything.
                        break;
                    case Prefix_Expression_Type::minus:
                        out += u8"-";
                        break;
                    case Prefix_Expression_Type::lnot:
                        out += u8"!";
                        break;
                    case Prefix_Expression_Type::bnot:
                        out += u8"~";
                        break;
                    case Prefix_Expression_Type::inc:
                        out += "++"_sv;
                        break;
                    case Prefix_Expression_Type::dec:
                        out += "--"_sv;
                        break;
                }
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

            case AST_Node_Type::postfix_expression: {
                Postfix_Expression& node = (Postfix_Expression&)ast_node;
                stringify(out, *node.expression, ctx);
                switch(node.type) {
                    case Postfix_Expression_Type::inc:
                        out += "++"_sv;
                        break;
                    case Postfix_Expression_Type::dec:
                        out += "--"_sv;
                        break;
                }
                return;
            }

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& node = (Identifier_Expression&)ast_node;
                out += node.value;
                return;
            }

            case AST_Node_Type::parenthesised_expression: {
                Parenthesised_Expression& node = (Parenthesised_Expression&)ast_node;
                out += u8"(";
                stringify(out, *node.expression, ctx);
                out += u8")";
                return;
            }

            case AST_Node_Type::reinterpret_expression: {
                Reinterpret_Expression& node = (Reinterpret_Expression&)ast_node;
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
                switch(node.base) {
                    case Integer_Literal_Base::dec: {
                        out += node.value;
                    } break;

                    case Integer_Literal_Base::bin: {
                        // Convert the binary literal to a hexadecimal literal because GLSL does not support binary literals
                        auto to_hex_digit = [](i64 value) -> char32 {
                            if(value < 10) {
                                return U'0' + value;
                            } else {
                                return U'A' + value - 10;
                            }
                        };

                        out += u8"0x";

                        i64 const length = node.value.size_bytes();
                        char8 const* string = node.value.data();
                        // Since we want to append digits as we go and the literal may have an arbitrary length,
                        // we have to process the first digits that are superfluous separately
                        i64 const superfluous = length % 4;
                        if(superfluous != 0) {
                            i64 value = 0;
                            for(i64 i = 0; i < superfluous; ++i) {
                                value *= 2;
                                value += string[i] == '1';
                            }
                            char32 const digit = to_hex_digit(value);
                            out += digit;
                        }

                        for(i64 i = superfluous; i < length; i += 4) {
                            i64 value = 0;
                            value |= (string[i] == '1') << 3;
                            value |= (string[i + 1] == '1') << 2;
                            value |= (string[i + 2] == '1') << 1;
                            value |= (string[i + 3] == '1');
                            char32 const digit = to_hex_digit(value);
                            out += digit;
                        }
                    } break;

                    case Integer_Literal_Base::oct: {
                        out += u8"0"_sv;
                        out += node.value;
                    } break;

                    case Integer_Literal_Base::hex: {
                        out += u8"0x"_sv;
                        out += node.value;
                    } break;
                }

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
        if(node.parameters.size() > 0) {
            stringify(out, *node.parameters[0], ctx);
            for(i64 i = 1; i != node.parameters.size(); ++i) {
                out += u8", ";
                stringify(out, *node.parameters[i], ctx);
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
                Symbol const* symbol = find_symbol(ctx, node.identifier);
                ANTON_ASSERT(symbol, "undefined symbol");
                Struct_Declaration const* struct_declaration = (Struct_Declaration const*)symbol;
                for(auto& member: struct_declaration->members) {
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

    static anton::Expected<anton::String, anton::String> format_bind_string(Allocator* const allocator, anton::String_View string, Identifier const& identifier,
                                                                            Type const& type) {
        anton::String out(allocator);
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
                return {anton::expected_error, anton::String(u8"error: unterminated placeholder in bind string", allocator)};
            }

            anton::String_View const symbol_name = {iter1, iter2};
            i64 const dot_pos = anton::find_substring(symbol_name, u8".");
            if(dot_pos == anton::npos) {
                // If it's not a builtin and doesn't have a dot, what is it?
                return {anton::expected_error, anton::String(u8"error: invalid placeholder in bind string", allocator)};
            }

            anton::String_View const iterator_name = {symbol_name.data(), dot_pos};
            anton::String_View const property_name = {symbol_name.data() + dot_pos + 1, symbol_name.bytes_end()};
            if(iterator_name != u8"$variable") {
                return {anton::expected_error, anton::String(u8"error: unknown placeholder in bind string", allocator)};
            }

            if(property_name == u8"name") {
                out += identifier.value;
            } else if(property_name == u8"type") {
                out += stringify_type(allocator, type);
            } else {
                return {anton::expected_error, anton::String(u8"error: unknown property name in bind string", allocator)};
            }

            // Skip the terminating }}
            iter2 = iter2 + 2;
            iter1 = iter2;
        }

        return {anton::expected_value, ANTON_MOV(out)};
    }

    // TODO: functions and structs_and_constants are passed as parameters.
    //       Remove them when we move functions and structs_and_constants to Stage_Context.
    [[nodiscard]] static anton::Expected<anton::String, anton::String>
    generate_vertex_stage(Context const& ctx, Codegen_Context& codegen_ctx, anton::String_View const pass_name, Stage_Context const& stage_ctx,
                          anton::String_View const stringified_extensions, anton::Slice<Function_Declaration const* const> const functions,
                          anton::Slice<Declaration const* const> const structs_and_constants) {
        anton::String out{anton::reserve, 4096, codegen_ctx.ctx.allocator};
        out += "#version 460 core\n#pragma shader_stage(vertex)\n\n"_sv;
        out += stringified_extensions;

        // Stringify and append structs and consts.
        if(structs_and_constants.size() > 0) {
            for(Declaration const* const decl: structs_and_constants) {
                stringify(out, *decl, codegen_ctx);
            }

            out += "\n"_sv;
        }

        // Append sources.
        if(stage_ctx.source_definitions.size() > 0) {
            for(auto [source, def]: stage_ctx.source_definitions) {
                out += def.declaration;
                out += "\n"_sv;
            }

            out += "\n"_sv;
        }

        // Stringify and append functions.
        if(functions.size() > 0) {
            // We stringify declarations first to make the functions visible anywhere.
            for(Function_Declaration const* const decl: functions) {
                stringify_function_forward_decl(out, *decl, codegen_ctx);
            }

            out += "\n"_sv;

            for(Function_Declaration const* const decl: functions) {
                stringify(out, *decl, codegen_ctx);
                out += "\n"_sv;
            }
        }

        // Stringify the stage function.

        // Stringify the return type.
        stringify(out, *stage_ctx.declaration->return_type, codegen_ctx);
        out += u8" "_sv;
        // Emit the function name in the form '_pass_<pass name>_stage_vertex'.
        anton::String const stage_function_name = anton::concat("_pass_"_sv, pass_name, "_stage_vertex"_sv);
        out += stage_function_name;
        // Emit the parameter list.
        {
            out += u8"("_sv;
            bool comma = false;
            for(auto& parameter_node: stage_ctx.declaration->parameters) {
                Function_Parameter const& parameter = (Function_Parameter const&)*parameter_node;
                // We skip unsized and opaque parameters.
                if(is_unsized_array(*parameter.type) || is_opaque_type(*parameter.type)) {
                    continue;
                }

                if(comma) {
                    out += u8", "_sv;
                }

                stringify(out, parameter, codegen_ctx);
                comma = true;
            }
            out += u8")"_sv;
        }
        // Emit the function body.
        out += " {\n"_sv;
        codegen_ctx.indent += 1;
        for(auto& statement: stage_ctx.declaration->body) {
            stringify(out, *statement, codegen_ctx);
        }
        codegen_ctx.indent -= 1;
        out += u8"}\n\n"_sv;

        // Write vertex inputs
        // Vertex input variable names are of the form '_pass_<pass name>_<parameter name><exploded member name>'.
        {
            anton::Array<Member_Info> members_info;
            for(auto& parameter_node: stage_ctx.declaration->parameters) {
                Function_Parameter const& p = (Function_Parameter const&)*parameter_node;
                if(is_vertex_input_parameter(p)) {
                    explode_type(ctx, *p.type, members_info);
                    i64 location = 0;
                    for(Member_Info const& m: members_info) {
                        anton::String location_str = anton::to_string(ctx.allocator, location);
                        anton::String_View const type_str = stringify(m.type);
                        // We attach parameter names to the vertex inputs
                        anton::String_View const param_name = p.identifier->value;
                        out += anton::format(ctx.allocator, "layout(location = {}) in {} _pass_{}_{}{};\n"_sv, location_str, type_str, pass_name, param_name,
                                             m.name);
                        location += m.location_slots;
                    }
                    members_info.clear();
                }
            }
            out += u8"\n"_sv;
        }

        bool const return_type_is_void = is_void(*stage_ctx.declaration->return_type);
        if(!return_type_is_void) {
            // Write vertex outputs
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *stage_ctx.declaration->return_type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                out += anton::format(u8"layout(location = {}) out {} _pass_{}_out{};\n"_sv, location_str, type_str, pass_name, m.name);
                location += m.location_slots;
            }
            out += u8"\n"_sv;
        }

        // Output main
        out += u8"void main() {\n"_sv;
        codegen_ctx.indent += 1;

        // Generate sourced parameters
        anton::Array<anton::String> arguments;
        {
            i64 param_index = 0;
            anton::Array<Member_Info> members_info;
            for(auto& parameter_node: stage_ctx.declaration->parameters) {
                Function_Parameter const& p = (Function_Parameter const&)*parameter_node;
                ANTON_ASSERT(is_sourced_parameter(p), u8"all vertex stage parameters must be sourced");
                if(is_vertex_input_parameter(p)) {
                    write_indent(out, codegen_ctx.indent);
                    stringify(out, *p.type, codegen_ctx);
                    anton::String arg_name = "arg"_sv + anton::to_string(param_index);
                    out += u8" "_sv;
                    out += arg_name;
                    out += u8";\n"_sv;

                    explode_type(ctx, *p.type, members_info);
                    // write vertex input assignments
                    for(Member_Info const& m: members_info) {
                        write_indent(out, codegen_ctx.indent);
                        anton::String_View const param_name = p.identifier->value;
                        out += anton::format(ctx.allocator, u8"{}{} = _pass_{}_{}{};\n"_sv, arg_name, m.accessor, pass_name, param_name, m.name);
                    }
                    members_info.clear();
                    arguments.emplace_back(ANTON_MOV(arg_name));
                    param_index += 1;
                } else if(!is_unsized_array(*p.type) && !is_opaque_type(*p.type)) {
                    auto iter = stage_ctx.source_definitions.find(p.source->value);
                    ANTON_ASSERT(iter != stage_ctx.source_definitions.end(), u8"sourced parameter doesn't have an existing source");
                    anton::String_View bind_string = iter->value.bind;
                    anton::Expected<anton::String, anton::String> res = format_bind_string(ctx.allocator, bind_string, *p.identifier, *p.type);
                    if(res) {
                        arguments.emplace_back(ANTON_MOV(res.value()));
                    } else {
                        return {anton::expected_error, ANTON_MOV(res.error())};
                    }
                }
                // else the parameter is sourced unsized or sourced opaque and we do not generate anything for those
            }
        }

        write_indent(out, codegen_ctx.indent);
        if(!return_type_is_void) {
            out += stringify_type(ctx.allocator, *stage_ctx.declaration->return_type);
            out += u8" _res = "_sv;
        }

        out += stage_function_name;
        out += u8"("_sv;
        if(arguments.size() > 0) {
            out += arguments[0];
            for(i64 i = 1; i < arguments.size(); ++i) {
                out += u8", "_sv;
                out += arguments[i];
            }
        }
        out += u8");\n"_sv;

        if(!return_type_is_void) {
            // Write vertex output assignments
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *stage_ctx.declaration->return_type, members_info);
            for(Member_Info const& m: members_info) {
                write_indent(out, codegen_ctx.indent);
                out += anton::format(u8"_pass_{}_out{} = _res{};\n"_sv, pass_name, m.name, m.accessor);
            }
        }

        codegen_ctx.indent -= 1;
        out += u8"}\n"_sv;
        return {anton::expected_value, ANTON_MOV(out)};
    }

    // TODO: functions and structs_and_constants are passed as parameters.
    //       Remove them when we move functions and structs_and_constants to Stage_Context.
    [[nodiscard]] static anton::Expected<anton::String, anton::String>
    generate_fragment_stage(Context const& ctx, Codegen_Context& codegen_ctx, anton::String_View const pass_name, Stage_Context const& stage_ctx,
                            anton::String_View const stringified_extensions, anton::Slice<Function_Declaration const* const> const functions,
                            anton::Slice<Declaration const* const> const structs_and_constants) {
        anton::String out{anton::reserve, 4096, codegen_ctx.ctx.allocator};
        out += "#version 460 core\n#pragma shader_stage(fragment)\n\n"_sv;
        out += stringified_extensions;

        // Stringify and append structs and consts.
        if(structs_and_constants.size() > 0) {
            for(Declaration const* const decl: structs_and_constants) {
                stringify(out, *decl, codegen_ctx);
            }

            out += "\n"_sv;
        }

        // Append sources.
        if(stage_ctx.source_definitions.size() > 0) {
            for(auto [source, def]: stage_ctx.source_definitions) {
                out += def.declaration;
                out += "\n"_sv;
            }

            out += "\n"_sv;
        }

        // Stringify and append functions.
        if(functions.size() > 0) {
            // We stringify declarations first to make the functions visible anywhere.
            for(Function_Declaration const* const decl: functions) {
                stringify_function_forward_decl(out, *decl, codegen_ctx);
            }

            out += "\n"_sv;

            for(Function_Declaration const* const decl: functions) {
                stringify(out, *decl, codegen_ctx);
                out += "\n"_sv;
            }
        }

        // Stringify the stage function

        // Stringify the return type.
        stringify(out, *stage_ctx.declaration->return_type, codegen_ctx);
        out += u8" "_sv;
        // Emit the function name in the form '_pass_<pass name>_stage_fragment'.
        anton::String const stage_function_name = anton::concat("_pass_"_sv, pass_name, "_stage_fragment"_sv);
        out += stage_function_name;
        // Emit the parameter list.
        {
            out += u8"("_sv;
            bool comma = false;
            for(auto& parameter_node: stage_ctx.declaration->parameters) {
                Function_Parameter const& parameter = (Function_Parameter const&)*parameter_node;
                // We skip unsized and opaque parameters.
                if(is_unsized_array(*parameter.type) || is_opaque_type(*parameter.type)) {
                    continue;
                }

                if(comma) {
                    out += u8", "_sv;
                }

                stringify(out, parameter, codegen_ctx);
                comma = true;
            }
            out += u8")"_sv;
        }
        out += " {\n"_sv;
        codegen_ctx.indent += 1;
        for(auto& statement: stage_ctx.declaration->body) {
            stringify(out, *statement, codegen_ctx);
        }
        codegen_ctx.indent -= 1;
        out += u8"}\n\n"_sv;

        anton::Array<anton::String> arguments;
        bool const has_prev_stage_input =
            stage_ctx.declaration->parameters.size() > 0 && !is_sourced_parameter((Function_Parameter const&)*stage_ctx.declaration->parameters[0]);
        // Write input from the previous stage if the first parameter is an ordinary parameter
        if(has_prev_stage_input) {
            Function_Parameter const& p = (Function_Parameter const&)*stage_ctx.declaration->parameters[0];
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *p.type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                anton::String_View interpolation_str = stringify_interpolation(m.interpolation);
                out += anton::format(u8"layout(location = {}) {} in {} _pass_{}_in_{}{};\n"_sv, location_str, interpolation_str, type_str, pass_name,
                                     p.identifier->value, m.name);
                location += m.location_slots;
            }
            out += u8"\n"_sv;
            arguments.emplace_back(u8"_arg0"_sv);
        }

        // Generate parameters
        for(i64 i = has_prev_stage_input; i < stage_ctx.declaration->parameters.size(); ++i) {
            Function_Parameter const& p = (Function_Parameter const&)*stage_ctx.declaration->parameters[i];
            // We skip unsized and opaque parameters since we can't pass those.
            if(is_unsized_array(*p.type) || is_opaque_type(*p.type)) {
                continue;
            }

            auto iter = stage_ctx.source_definitions.find(p.source->value);
            ANTON_ASSERT(iter != stage_ctx.source_definitions.end(), u8"sourced parameter doesn't have an existing source");
            anton::String_View bind_string = iter->value.bind;
            anton::Expected<anton::String, anton::String> res = format_bind_string(ctx.allocator, bind_string, *p.identifier, *p.type);
            if(res) {
                arguments.emplace_back(ANTON_MOV(res.value()));
            } else {
                return {anton::expected_error, ANTON_MOV(res.error())};
            }
        }

        bool const return_type_is_void = is_void(*stage_ctx.declaration->return_type);
        // Decompose return type to individual outputs
        if(!return_type_is_void) {
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *stage_ctx.declaration->return_type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                out += anton::format(u8"layout(location = {}) out {} _pass_{}_out{};\n"_sv, location_str, type_str, pass_name, m.name);
                location += m.location_slots;
            }
            out += u8"\n"_sv;
        }

        // Output main
        out += u8"void main() {\n"_sv;
        codegen_ctx.indent += 1;

        // Build _arg0 (input from the previous stage aggregated into a struct) from fragment inputs
        if(has_prev_stage_input) {
            Function_Parameter const& p = (Function_Parameter const&)*stage_ctx.declaration->parameters[0];
            write_indent(out, codegen_ctx.indent);
            out += stringify_type(ctx.allocator, *p.type);
            out += u8" _arg0;\n"_sv;
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *p.type, members_info);
            for(Member_Info const& m: members_info) {
                write_indent(out, codegen_ctx.indent);
                out += anton::format(u8"_arg0{} = _pass_{}_in_{}{};\n"_sv, m.accessor, pass_name, p.identifier->value, m.name);
            }
        }

        // Write stage function call
        anton::String const shader_return_name = u8"_res"_s;
        write_indent(out, codegen_ctx.indent);
        if(!return_type_is_void) {
            stringify(out, *stage_ctx.declaration->return_type, codegen_ctx);
            // Write result name
            out += u8" "_sv;
            out += shader_return_name;
            out += u8" = "_sv;
        }

        out += stage_function_name;
        out += u8"("_sv;
        if(arguments.size() > 0) {
            out += arguments[0];
            for(i64 i = 1; i < arguments.size(); ++i) {
                out += u8", "_sv;
                out += arguments[i];
            }
        }
        out += u8");\n";

        if(!return_type_is_void) {
            anton::Array<Member_Info> members_info;
            explode_type(ctx, *stage_ctx.declaration->return_type, members_info);
            for(Member_Info const& m: members_info) {
                write_indent(out, codegen_ctx.indent);
                out += anton::format(u8"_pass_{}_out{} = {}{};\n"_sv, pass_name, m.name, shader_return_name, m.accessor);
            }
        }

        codegen_ctx.indent -= 1;
        out += u8"}\n";
        return {anton::expected_value, ANTON_MOV(out)};
    }

    // TODO: functions and structs_and_constants are passed as parameters.
    //       Remove them when we move functions and structs_and_constants to Stage_Context.
    [[nodiscard]] static anton::Expected<anton::String, anton::String>
    generate_compute_stage(Codegen_Context& codegen_ctx, anton::String_View const pass_name, Stage_Context const& stage_ctx,
                           anton::String_View const stringified_extensions, anton::Slice<Function_Declaration const* const> const functions,
                           anton::Slice<Declaration const* const> const structs_and_constants) {
        anton::String out{anton::reserve, 4096, codegen_ctx.ctx.allocator};
        out += "#version 460 core\n#pragma shader_stage(compute)\n\n"_sv;
        out += stringified_extensions;

        // Stringify and append structs and consts.
        if(structs_and_constants.size() > 0) {
            for(Declaration const* const decl: structs_and_constants) {
                stringify(out, *decl, codegen_ctx);
            }

            out += "\n"_sv;
        }

        // Append sources.
        if(stage_ctx.source_definitions.size() > 0) {
            for(auto [source, def]: stage_ctx.source_definitions) {
                out += def.declaration;
                out += "\n"_sv;
            }

            out += "\n"_sv;
        }

        // Stringify and append functions.
        if(functions.size() > 0) {
            // We stringify declarations first to make the functions visible anywhere.
            for(Function_Declaration const* const decl: functions) {
                stringify_function_forward_decl(out, *decl, codegen_ctx);
            }

            out += "\n"_sv;

            for(Function_Declaration const* const decl: functions) {
                stringify(out, *decl, codegen_ctx);
                out += "\n"_sv;
            }
        }

        // Stringify the stage function.

        // Stringify the return type.
        stringify(out, *stage_ctx.declaration->return_type, codegen_ctx);
        out += u8" "_sv;
        // Emit the function name in the form '_pass_<pass name>_stage_fragment'.
        anton::String const stage_function_name = anton::concat("_pass_"_sv, pass_name, "_stage_compute"_sv);
        out += stage_function_name;
        // Emit the parameter list.
        {
            out += u8"("_sv;
            bool comma = false;
            for(auto& parameter_node: stage_ctx.declaration->parameters) {
                Function_Parameter const& parameter = (Function_Parameter const&)*parameter_node;
                // We skip unsized and opaque parameters.
                if(is_unsized_array(*parameter.type) || is_opaque_type(*parameter.type)) {
                    continue;
                }

                if(comma) {
                    out += u8", "_sv;
                }

                stringify(out, parameter, codegen_ctx);
                comma = true;
            }
            out += u8")"_sv;
        }
        out += " {\n"_sv;
        codegen_ctx.indent += 1;
        for(auto& statement: stage_ctx.declaration->body) {
            stringify(out, *statement, codegen_ctx);
        }
        codegen_ctx.indent -= 1;
        out += u8"}\n\n"_sv;

        for(auto& attribute: stage_ctx.declaration->attributes) {
            switch(attribute->node_type) {
                case AST_Node_Type::workgroup_attribute: {
                    Workgroup_Attribute& attrib = (Workgroup_Attribute&)*attribute;
                    out += u8"layout(local_size_x = "_sv;
                    stringify(out, *attrib.x, codegen_ctx);
                    if(attrib.y) {
                        out += u8", local_size_y = "_sv;
                        stringify(out, *attrib.y, codegen_ctx);
                        if(attrib.z) {
                            out += u8", local_size_z = "_sv;
                            stringify(out, *attrib.z, codegen_ctx);
                        }
                    }
                    out += u8") in;\n\n"_sv;
                } break;

                default:
                    break;
            }
        }

        // Output main
        out += u8"void main() {\n"_sv;
        codegen_ctx.indent += 1;

        // Write stage function call
        write_indent(out, codegen_ctx.indent);
        out += stage_function_name;
        out += u8"("_sv;

        // Write arguments.
        bool comma = false;
        for(auto& parameter_node: stage_ctx.declaration->parameters) {
            Function_Parameter const& parameter = (Function_Parameter const&)*parameter_node;
            // We skip unsized and opaque parameters.
            if(is_unsized_array(*parameter.type) || is_opaque_type(*parameter.type)) {
                continue;
            }

            auto iter = stage_ctx.source_definitions.find(parameter.source->value);
            ANTON_ASSERT(iter != stage_ctx.source_definitions.end(), u8"sourced parameter doesn't have an existing source");
            anton::String_View bind_string = iter->value.bind;
            anton::Expected<anton::String, anton::String> res =
                format_bind_string(codegen_ctx.ctx.allocator, bind_string, *parameter.identifier, *parameter.type);
            if(!res) {
                return {anton::expected_error, ANTON_MOV(res.error())};
            }

            if(comma) {
                out += u8", "_sv;
            }

            out += res.value();
            comma = true;
        }

        out += u8");\n"_sv;
        codegen_ctx.indent -= 1;
        out += u8"}\n"_sv;
        return {anton::expected_value, ANTON_MOV(out)};
    }

    anton::Expected<Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Codegen_Data const& data) {
        Codegen_Context codegen_ctx{ctx, 0};
        anton::String stringified_extensions;
        if(data.extensions.size() > 0) {
            for(Extension const& extension: data.extensions) {
                stringified_extensions += u8"#extension "_sv;
                stringified_extensions += extension.name;
                stringified_extensions += u8": "_sv;
                switch(extension.behaviour) {
                    case Extension_Behaviour::require:
                        stringified_extensions += u8"require"_sv;
                        break;
                    case Extension_Behaviour::enable:
                        stringified_extensions += u8"enable"_sv;
                        break;
                    case Extension_Behaviour::warn:
                        stringified_extensions += u8"warn"_sv;
                        break;
                    case Extension_Behaviour::disable:
                        stringified_extensions += u8"disable"_sv;
                        break;
                }
                stringified_extensions += U'\n';
            }
            stringified_extensions += U'\n';
        }

        Array<Pass_Data> pass_datas{ctx.allocator};
        for(Pass_Context const& pass: data.passes) {
            Pass_Data& pass_data = pass_datas.emplace_back(Pass_Data{pass.name, {}});
            if(pass.vertex_context) {
                anton::Expected<anton::String, anton::String> res =
                    generate_vertex_stage(ctx, codegen_ctx, pass.name, pass.vertex_context, stringified_extensions, pass.functions, pass.structs_and_constants);
                if(res) {
                    pass_data.files.emplace_back(GLSL_File{ANTON_MOV(res.value()), Stage_Type::vertex});
                } else {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }
            }

            if(pass.fragment_context) {
                anton::Expected<anton::String, anton::String> res = generate_fragment_stage(ctx, codegen_ctx, pass.name, pass.fragment_context,
                                                                                            stringified_extensions, pass.functions, pass.structs_and_constants);
                if(res) {
                    pass_data.files.emplace_back(GLSL_File{ANTON_MOV(res.value()), Stage_Type::fragment});
                } else {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }
            }

            if(pass.compute_context) {
                anton::Expected<anton::String, anton::String> res =
                    generate_compute_stage(codegen_ctx, pass.name, pass.compute_context, stringified_extensions, pass.functions, pass.structs_and_constants);
                if(res) {
                    pass_data.files.emplace_back(GLSL_File{ANTON_MOV(res.value()), Stage_Type::compute});
                } else {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }
            }
        }

        return {anton::expected_value, ANTON_MOV(pass_datas)};
    }
} // namespace vush
