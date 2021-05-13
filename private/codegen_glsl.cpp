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

    struct Code_Mapper {
    private:
        anton::Array<Generated_To_Source_Mapping> mappings;
        anton::Flat_Hash_Map<anton::String, i64> sources;

    public:
        i64 begin_mapping(Source_Info const& source_info, i64 const line, i64 const column) {
            i64 source_index = -1;
            auto iter = sources.find(source_info.file_path);
            if(iter != sources.end()) {
                source_index = iter->value;
            } else {
                source_index = sources.size();
                sources.emplace(source_info.file_path, source_index);
            }

            Generated_To_Source_Mapping mapping{
                .source_index = source_index,
                .generated_line_begin = line,
                .generated_column_begin = column,
                .source_line_begin = source_info.line,
                .source_column_begin = source_info.column,
                .source_line_end = source_info.end_line,
                .source_column_end = source_info.end_column,
            };
            mappings.emplace_back(mapping);
            i64 const index = mappings.size() - 1;
            return index;
        }

        void end_mapping(i64 const index, i64 const line, i64 const column) {
            Generated_To_Source_Mapping& mapping = mappings[index];
            mapping.generated_line_end = line;
            mapping.generated_column_end = column;
        }

        Code_Mappings build_code_mappings() {
            i64 const source_count = sources.size();
            Code_Mappings result{.mappings = ANTON_MOV(mappings)};
            result.sources.resize(source_count);
            for(auto kv: sources) {
                result.sources[kv.value] = ANTON_MOV(kv.key);
            }

            sources.clear();
            return result;
        }
    };

    struct Codegen_Context {
    public:
        Context const& ctx;

    private:
        Code_Mapper mapper;
        anton::String out;
        i64 indent_level = 0;
        i64 line = 0;
        i64 column = 0;

    public:
        Codegen_Context(Context const& ctx): ctx(ctx) {}
        Codegen_Context(Context const& ctx, i64 line): ctx(ctx), line(line) {}
        Codegen_Context(Codegen_Context const& other)
            : ctx(other.ctx), mapper(other.mapper), out(other.out), indent_level(other.indent_level), line(other.line), column(other.column) {}
        ~Codegen_Context() = default;

        void operator+=(char8 const c) {
            if(c == '\n') {
                line += 1;
                column = 1;
            }

            out += c;
        }

        void operator+=(char32 const c) {
            if(c == U'\n') {
                line += 1;
                column = 1;
            }

            out += c;
        }

        void operator+=(anton::String_View const sv) {
            for(auto c: sv.bytes()) {
                if(c != '\n') {
                    column += 1;
                } else {
                    line += 1;
                    column = 1;
                }
            }

            out += sv;
        }

        void indent(i64 const indent_count = 1) {
            indent_level += indent_count;
        }

        void unindent(i64 const indent_count = 1) {
            indent_level -= indent_count;
        }

        void write_indent() {
            for(i64 i = 0; i < indent_level; ++i) {
                out += u8"    ";
            }

            column += 4 * indent_level;
        }

        anton::String& get_output() {
            return out;
        }

        i64 begin_mapping(Source_Info const& source_info) {
            return mapper.begin_mapping(source_info, line, column);
        }

        void end_mapping(i64 const index) {
            mapper.end_mapping(index, line, column);
        }

        Code_Mappings build_code_mappings() {
            return mapper.build_code_mappings();
        }
    };

    static void stringify(Codegen_Context& ctx, AST_Node const& ast_node);

    struct Reinterpret_Context {
        Expression const* source_expr;
        Expression const* index_expr;
        i64 offset = 0;
    };

    static void stringify_type_reinterpret(Codegen_Context& ctx, Reinterpret_Context& reinterpret_ctx, Type& type) {
        switch(type.node_type) {
            case AST_Node_Type::user_defined_type: {
                User_Defined_Type& t = (User_Defined_Type&)type;
                ctx += t.identifier;
                ctx += u8"(";
                // We made sure that the symbol exists during validation stage
                Symbol const* symbol = find_symbol(ctx.ctx, t.identifier);
                Struct_Declaration const& struct_declaration = (Struct_Declaration const&)*symbol;
                for(i64 i = 0; i < struct_declaration.members.size(); ++i) {
                    if(i != 0) {
                        ctx += u8", ";
                    }

                    auto& member = struct_declaration.members[i];
                    stringify_type_reinterpret(ctx, reinterpret_ctx, *member->type);
                }
                ctx += u8")";
            } break;

            case AST_Node_Type::builtin_type: {
                Builtin_Type& t = (Builtin_Type&)type;
                ctx += stringify(t.type);
                ctx += u8"(";
                switch(t.type) {
                    case Builtin_GLSL_Type::glsl_bool:
                    case Builtin_GLSL_Type::glsl_int: {
                        ctx += u8"floatBitsToInt(";
                        stringify(ctx, *reinterpret_ctx.source_expr);
                        ctx += u8"[";
                        stringify(ctx, *reinterpret_ctx.index_expr);
                        ctx += u8" + ";
                        ctx += anton::to_string(reinterpret_ctx.offset);
                        ctx += u8"]";
                        ctx += u8")";
                        reinterpret_ctx.offset += 1;
                    } break;

                    case Builtin_GLSL_Type::glsl_uint: {
                        ctx += u8"floatBitsToUint(";
                        stringify(ctx, *reinterpret_ctx.source_expr);
                        ctx += u8"[";
                        stringify(ctx, *reinterpret_ctx.index_expr);
                        ctx += u8" + ";
                        ctx += anton::to_string(reinterpret_ctx.offset);
                        ctx += u8"]";
                        ctx += u8")";
                        reinterpret_ctx.offset += 1;
                    } break;

                    case Builtin_GLSL_Type::glsl_float: {
                        stringify(ctx, *reinterpret_ctx.source_expr);
                        ctx += u8"[";
                        stringify(ctx, *reinterpret_ctx.index_expr);
                        ctx += u8" + ";
                        ctx += anton::to_string(reinterpret_ctx.offset);
                        ctx += u8"]";
                        reinterpret_ctx.offset += 1;
                    } break;

                    case Builtin_GLSL_Type::glsl_double: {
                        // output
                        // packDouble2x32(uvec2(floatBitsToUint(data[index + offset]), floatBitsToUint(data[index + offset + 1])))
                        ctx += u8"packDouble2x32(uvec2(";
                        ctx += u8"floatBitsToUint(";
                        stringify(ctx, *reinterpret_ctx.source_expr);
                        ctx += u8"[";
                        stringify(ctx, *reinterpret_ctx.index_expr);
                        ctx += u8" + ";
                        ctx += anton::to_string(reinterpret_ctx.offset);
                        ctx += u8"]";
                        ctx += u8"), ";
                        ctx += u8"floatBitsToUint(";
                        stringify(ctx, *reinterpret_ctx.source_expr);
                        ctx += u8"[";
                        stringify(ctx, *reinterpret_ctx.index_expr);
                        ctx += u8" + ";
                        ctx += anton::to_string(reinterpret_ctx.offset + 1);
                        ctx += u8"]";
                        ctx += u8")";
                        ctx += u8"))";
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
                                ctx += u8", ";
                            }

                            stringify(ctx, *reinterpret_ctx.source_expr);
                            ctx += u8"[";
                            stringify(ctx, *reinterpret_ctx.index_expr);
                            ctx += u8" + ";
                            ctx += anton::to_string(reinterpret_ctx.offset + i);
                            ctx += u8"]";
                        }
                        reinterpret_ctx.offset += component_count;
                    } break;

                    default:
                        ANTON_UNREACHABLE();
                }
                ctx += u8")";
            } break;

            case AST_Node_Type::array_type: {
                Array_Type& t = (Array_Type&)type;
                // We made sure that the array is sized during validation stage
                ctx += stringify_type(t);
                ctx += u8"(";
                stringify_type_reinterpret(ctx, reinterpret_ctx, *t.base);
                ctx += u8")";
            } break;

            default:
                ANTON_UNREACHABLE();
        }
    }

    static void stringify(Codegen_Context& ctx, AST_Node const& ast_node) {
        switch(ast_node.node_type) {
            case AST_Node_Type::identifier: {
                Identifier& node = (Identifier&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += node.value;
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::builtin_type: {
                Builtin_Type& node = (Builtin_Type&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += stringify(node.type);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::user_defined_type: {
                User_Defined_Type& node = (User_Defined_Type&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += node.identifier;
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::array_type: {
                Array_Type& node = (Array_Type&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                stringify(ctx, *node.base);
                ctx += u8"[";
                if(node.size) {
                    stringify(ctx, *node.size);
                }
                ctx += u8"]";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::constant_declaration: {
                Constant_Declaration& node = (Constant_Declaration&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += u8"const ";
                stringify(ctx, *node.type);
                ctx += u8" ";
                stringify(ctx, *node.identifier);
                ctx += u8" = ";
                stringify(ctx, *node.initializer);
                ctx += u8";\n";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::variable_declaration: {
                Variable_Declaration& node = (Variable_Declaration&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                stringify(ctx, *node.type);
                ctx += u8" ";
                stringify(ctx, *node.identifier);
                if(node.initializer) {
                    ctx += u8" = ";
                    stringify(ctx, *node.initializer);
                }
                ctx += u8";\n";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::struct_declaration: {
                Struct_Declaration& node = (Struct_Declaration&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += u8"struct ";
                stringify(ctx, *node.identifier);
                ctx += u8" {\n";
                ctx.indent();
                for(auto& member: node.members) {
                    ctx.write_indent();
                    stringify(ctx, *member->type);
                    ctx += u8" ";
                    stringify(ctx, *member->identifier);
                    ctx += u8";\n";
                }
                ctx.unindent();
                ctx += u8"};\n";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::function_declaration: {
                Function_Declaration& node = (Function_Declaration&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                stringify(ctx, *node.return_type);
                ctx += u8" ";
                stringify(ctx, *node.identifier);
                // param list
                ctx += u8"(";
                if(node.params.size() > 0) {
                    stringify(ctx, *node.params[0]);
                    for(i64 i = 1; i != node.params.size(); ++i) {
                        ctx += u8", ";
                        stringify(ctx, *node.params[i]);
                    }
                }
                ctx += u8") {\n";
                ctx.indent();
                for(auto& statement: node.body) {
                    stringify(ctx, *statement);
                }
                ctx.unindent();
                ctx += u8"}\n";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::function_parameter: {
                Function_Parameter& node = (Function_Parameter&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                stringify(ctx, *node.type);
                ctx += u8" ";
                stringify(ctx, *node.identifier);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::block_statement: {
                Block_Statement& node = (Block_Statement&)ast_node;
                ctx.write_indent();
                ctx += u8"{\n";
                ctx.indent();
                for(auto& statement: node.statements) {
                    stringify(ctx, *statement);
                }
                ctx.unindent();
                ctx.write_indent();
                ctx += u8"}\n";
                return;
            }

            case AST_Node_Type::if_statement: {
                If_Statement& node = (If_Statement&)ast_node;
                ctx.write_indent();
                ctx += u8"if(";
                stringify(ctx, *node.condition);
                ctx += u8") {\n";
                ctx.indent();
                for(auto& statement: node.true_statements) {
                    stringify(ctx, *statement);
                }
                ctx.unindent();
                if(node.false_statements.size() == 0) {
                    ctx.write_indent();
                    ctx += u8"}\n";
                } else {
                    ctx.write_indent();
                    ctx += u8"} else {\n";
                    ctx.indent();
                    for(auto& statement: node.false_statements) {
                        stringify(ctx, *statement);
                    }
                    ctx.unindent();
                    ctx.write_indent();
                    ctx += u8"}\n";
                }
                return;
            }

            case AST_Node_Type::switch_statement: {
                Switch_Statement& node = (Switch_Statement&)ast_node;
                ctx.write_indent();
                ctx += u8"switch(";
                stringify(ctx, *node.match_expression);
                ctx += u8") {\n";
                ctx.indent();
                for(auto& s: node.cases) {
                    for(auto& label: s->labels) {
                        ctx.write_indent();
                        if(label->node_type == AST_Node_Type::default_expression) {
                            ctx += u8"default:\n"_sv;
                        } else {
                            ctx += u8"case "_sv;
                            stringify(ctx, *label);
                            ctx += u8":\n";
                        }
                    }

                    ctx.write_indent();
                    ctx += u8"{\n"_sv;
                    ctx.indent();
                    for(auto& statement: s->statements) {
                        stringify(ctx, *statement);
                    }
                    ctx.write_indent();
                    ctx += u8"break;\n"_sv;
                    ctx.unindent();
                    ctx.write_indent();
                    ctx += u8"}\n"_sv;
                }
                ctx.unindent();
                ctx.write_indent();
                ctx += u8"}\n";
                return;
            }

            case AST_Node_Type::for_statement: {
                For_Statement& node = (For_Statement&)ast_node;
                ctx.write_indent();
                ctx += u8"for(";
                if(node.declaration) {
                    // We stringify the variable_decl manually because we need inline declaration
                    Variable_Declaration& decl = *node.declaration;
                    stringify(ctx, *decl.type);
                    ctx += u8" ";
                    stringify(ctx, *decl.identifier);
                    ctx += u8" = ";
                    stringify(ctx, *decl.initializer);
                }
                ctx += u8";";
                if(node.condition) {
                    ctx += u8" ";
                    stringify(ctx, *node.condition);
                }
                ctx += u8";";
                if(node.post_expression) {
                    ctx += u8" ";
                    stringify(ctx, *node.post_expression);
                }
                // We stringify the block ourselves to allow custom formatting of the braces
                ctx += u8") {\n";
                ctx.indent();
                for(auto& statement: node.statements) {
                    stringify(ctx, *statement);
                }
                ctx.unindent();
                ctx.write_indent();
                ctx += u8"}\n";
                return;
            }

            case AST_Node_Type::while_statement: {
                While_Statement& node = (While_Statement&)ast_node;
                ctx.write_indent();
                ctx += u8"while(";
                stringify(ctx, *node.condition);
                // We need no-braces block. We add them inline ourselves.
                ctx += u8") {\n";
                ctx.indent();
                for(auto& statement: node.statements) {
                    stringify(ctx, *statement);
                }
                ctx.unindent();
                ctx.write_indent();
                ctx += u8"}\n";
                return;
            }

            case AST_Node_Type::do_while_statement: {
                Do_While_Statement& node = (Do_While_Statement&)ast_node;
                ctx.write_indent();
                // We need no-braces block. We add them inline ourselves.
                ctx += u8"do {\n";
                ctx.indent();
                for(auto& statement: node.statements) {
                    stringify(ctx, *statement);
                }
                ctx.unindent();
                ctx.write_indent();
                ctx += u8"} while(";
                stringify(ctx, *node.condition);
                ctx += u8");\n";
                return;
            }

            case AST_Node_Type::return_statement: {
                Return_Statement& node = (Return_Statement&)ast_node;
                ctx.write_indent();
                ctx += u8"return";
                if(node.return_expression) {
                    ctx += u8" ";
                    stringify(ctx, *node.return_expression);
                }
                ctx += u8";\n";
                return;
            }

            case AST_Node_Type::break_statement: {
                Break_Statement& node = (Break_Statement&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx.write_indent();
                ctx += u8"break;\n";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::continue_statement: {
                Continue_Statement& node = (Continue_Statement&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx.write_indent();
                ctx += u8"continue;\n";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::discard_statement: {
                Discard_Statement& node = (Discard_Statement&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx.write_indent();
                ctx += u8"discard;\n";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::declaration_statement: {
                Declaration_Statement& node = (Declaration_Statement&)ast_node;
                ctx.write_indent();
                stringify(ctx, *node.declaration);
                return;
            }

            case AST_Node_Type::expression_statement: {
                Expression_Statement& node = (Expression_Statement&)ast_node;
                ctx.write_indent();
                stringify(ctx, *node.expression);
                ctx += u8";\n";
                return;
            }

            case AST_Node_Type::assignment_expression: {
                Assignment_Expression& node = (Assignment_Expression&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                stringify(ctx, *node.lhs);
                ctx += u8" = ";
                stringify(ctx, *node.rhs);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::arithmetic_assignment_expression: {
                Arithmetic_Assignment_Expression& node = (Arithmetic_Assignment_Expression&)ast_node;
                stringify(ctx, *node.lhs);
                switch(node.type) {
                    case Arithmetic_Assignment_Type::plus: {
                        ctx += u8" += ";
                    } break;

                    case Arithmetic_Assignment_Type::minus: {
                        ctx += u8" -= ";
                    } break;

                    case Arithmetic_Assignment_Type::multiply: {
                        ctx += u8" *= ";
                    } break;

                    case Arithmetic_Assignment_Type::divide: {
                        ctx += u8" /= ";
                    } break;

                    case Arithmetic_Assignment_Type::remainder: {
                        ctx += u8" %= ";
                    } break;

                    case Arithmetic_Assignment_Type::lshift: {
                        ctx += u8" <<= ";
                    } break;

                    case Arithmetic_Assignment_Type::rshift: {
                        ctx += u8" >>= ";
                    } break;

                    case Arithmetic_Assignment_Type::bit_and: {
                        ctx += u8" &= ";
                    } break;

                    case Arithmetic_Assignment_Type::bit_xor: {
                        ctx += u8" ^= ";
                    } break;

                    case Arithmetic_Assignment_Type::bit_or: {
                        ctx += u8" |= ";
                    } break;
                }
                stringify(ctx, *node.rhs);
                return;
            }

            case AST_Node_Type::elvis_expression: {
                Elvis_Expression& node = (Elvis_Expression&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                stringify(ctx, *node.condition);
                ctx += u8" ? ";
                stringify(ctx, *node.true_expression);
                ctx += u8" : ";
                stringify(ctx, *node.false_expression);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::binary_expression: {
                Binary_Expression& node = (Binary_Expression&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                stringify(ctx, *node.lhs);
                switch(node.type) {
                    case Binary_Expression_Type::logic_or:
                        ctx += u8" || ";
                        break;
                    case Binary_Expression_Type::logic_xor:
                        ctx += u8" ^^ ";
                        break;
                    case Binary_Expression_Type::logic_and:
                        ctx += u8" && ";
                        break;
                    case Binary_Expression_Type::equal:
                        ctx += u8" == ";
                        break;
                    case Binary_Expression_Type::unequal:
                        ctx += u8" != ";
                        break;
                    case Binary_Expression_Type::greater_than:
                        ctx += u8" > ";
                        break;
                    case Binary_Expression_Type::less_than:
                        ctx += u8" < ";
                        break;
                    case Binary_Expression_Type::greater_equal:
                        ctx += u8" >= ";
                        break;
                    case Binary_Expression_Type::less_equal:
                        ctx += u8" <= ";
                        break;
                    case Binary_Expression_Type::bit_or:
                        ctx += u8" | ";
                        break;
                    case Binary_Expression_Type::bit_xor:
                        ctx += u8" ^ ";
                        break;
                    case Binary_Expression_Type::bit_and:
                        ctx += u8" & ";
                        break;
                    case Binary_Expression_Type::lshift:
                        ctx += u8" << ";
                        break;
                    case Binary_Expression_Type::rshift:
                        ctx += u8" >> ";
                        break;
                    case Binary_Expression_Type::add:
                        ctx += u8" + ";
                        break;
                    case Binary_Expression_Type::sub:
                        ctx += u8" - ";
                        break;
                    case Binary_Expression_Type::mul:
                        ctx += u8" * ";
                        break;
                    case Binary_Expression_Type::div:
                        ctx += u8" / ";
                        break;
                    case Binary_Expression_Type::mod:
                        ctx += u8" % ";
                        break;
                }
                stringify(ctx, *node.rhs);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::unary_expression: {
                Unary_Expression& node = (Unary_Expression&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                switch(node.type) {
                    case Unary_Type::plus:
                        break;

                    case Unary_Type::minus: {
                        ctx += u8"-";
                    } break;

                    case Unary_Type::logic_not: {
                        ctx += u8"!";
                    } break;

                    case Unary_Type::bit_not: {
                        ctx += u8"~";
                    } break;
                }
                stringify(ctx, *node.expression);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::prefix_increment_expression: {
                Prefix_Increment_Expression& node = (Prefix_Increment_Expression&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += u8"++";
                stringify(ctx, *node.expression);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::prefix_decrement_expression: {
                Prefix_Decrement_Expression& node = (Prefix_Decrement_Expression&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += u8"--";
                stringify(ctx, *node.expression);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::function_call_expression: {
                Function_Call_Expression& node = (Function_Call_Expression&)ast_node;
                stringify(ctx, *node.identifier);
                ctx += u8"(";
                if(node.arguments.size() > 0) {
                    stringify(ctx, *node.arguments[0]);
                    for(i64 i = 1; i < node.arguments.size(); ++i) {
                        ctx += u8", ";
                        stringify(ctx, *node.arguments[i]);
                    }
                }
                ctx += u8")";
                return;
            }

            case AST_Node_Type::member_access_expression: {
                Member_Access_Expression& node = (Member_Access_Expression&)ast_node;
                stringify(ctx, *node.base);
                ctx += u8".";
                stringify(ctx, *node.member);
                return;
            }

            case AST_Node_Type::array_access_expression: {
                Array_Access_Expression& node = (Array_Access_Expression&)ast_node;
                stringify(ctx, *node.base);
                ctx += u8"[";
                stringify(ctx, *node.index);
                ctx += u8"]";
                return;
            }

            case AST_Node_Type::postfix_increment_expression: {
                Postfix_Increment_Expression& node = (Postfix_Increment_Expression&)ast_node;
                stringify(ctx, *node.expression);
                ctx += u8"++";
                return;
            }

            case AST_Node_Type::postfix_decrement_expression: {
                Postfix_Decrement_Expression& node = (Postfix_Decrement_Expression&)ast_node;
                stringify(ctx, *node.expression);
                ctx += u8"--";
                return;
            }

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& node = (Identifier_Expression&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                stringify(ctx, *node.identifier);
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::parenthesised_expression: {
                Parenthesised_Expression& node = (Parenthesised_Expression&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += u8"(";
                stringify(ctx, *node.expression);
                ctx += u8")";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::reinterpret_expression: {
                Reinterpret_Expression& node = (Reinterpret_Expression&)ast_node;
                // TODO: Validate source is a float array
                // TODO: Validate target type
                Reinterpret_Context reinterpret_ctx{node.source.get(), node.index.get()};
                stringify_type_reinterpret(ctx, reinterpret_ctx, *node.target_type);
                return;
            }

            case AST_Node_Type::bool_literal: {
                Bool_Literal& node = (Bool_Literal&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += node.value ? u8"true" : u8"false";
                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::integer_literal: {
                Integer_Literal& node = (Integer_Literal&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                switch(node.base) {
                    case Integer_Literal_Base::dec: {
                        ctx += node.value;
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

                        ctx += u8"0x";

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
                            ctx += digit;
                        }

                        for(i64 i = superfluous; i < length; i += 4) {
                            i64 value = 0;
                            value |= (string[i] == '1') << 3;
                            value |= (string[i + 1] == '1') << 2;
                            value |= (string[i + 2] == '1') << 1;
                            value |= (string[i + 3] == '1');
                            char32 const digit = to_hex_digit(value);
                            ctx += digit;
                        }
                    } break;

                    case Integer_Literal_Base::oct: {
                        ctx += u8"0"_sv;
                        ctx += node.value;
                    } break;

                    case Integer_Literal_Base::hex: {
                        ctx += u8"0x"_sv;
                        ctx += node.value;
                    } break;
                }

                if(node.type == Integer_Literal_Type::u32) {
                    ctx += u8"u";
                }

                ctx.end_mapping(mapping);
                return;
            }

            case AST_Node_Type::float_literal: {
                Float_Literal& node = (Float_Literal&)ast_node;
                i64 const mapping = ctx.begin_mapping(node.source_info);
                ctx += node.value;
                if(node.type == Float_Literal_Type::f64) {
                    ctx += u8"lf";
                }

                ctx.end_mapping(mapping);
                return;
            }

            default:
                break;
        }
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

    static anton::Expected<void, anton::String> generate_vertex_stage(Codegen_Context& ctx, Pass_Stage_Declaration const& stage,
                                                                      anton::Flat_Hash_Map<anton::String, Source_Definition> const& source_definitions) {
        anton::String const& pass_name = stage.pass_name->value;
        // Stringify the stage function
        stringify(ctx, *stage.return_type);
        ctx += u8" ";
        anton::String const stage_function_name = u8"_pass_" + pass_name + u8"_stage_vertex";
        ctx += stage_function_name;
        // param list
        ctx += u8"(";
        if(stage.params.size() > 0) {
            stringify(ctx, *stage.params[0]);
            for(i64 i = 1; i < stage.params.size(); ++i) {
                ctx += u8", ";
                stringify(ctx, *stage.params[i]);
            }
        }
        ctx += u8") {\n";
        ctx.indent();
        for(auto& statement: stage.body) {
            stringify(ctx, *statement);
        }
        ctx.unindent();
        ctx += u8"}\n\n";

        // Write vertex inputs
        // Vertex input variable names are of the form '_pass_<pass name>_<parameter name><exploded member name>'.
        {
            anton::Array<Member_Info> members_info;
            for(auto& param: stage.params) {
                Function_Parameter& p = (Function_Parameter&)*param;
                if(is_vertex_input_parameter(p)) {
                    explode_type(ctx.ctx, *p.type, members_info);
                    i64 location = 0;
                    for(Member_Info const& m: members_info) {
                        anton::String location_str = anton::to_string(location);
                        anton::String_View type_str = stringify(m.type);
                        // We attach parameter names to the vertex inputs
                        anton::String const& param_name = p.identifier->value;
                        ctx += anton::format(u8"layout(location = {}) in {} _pass_{}_{}{};\n", location_str, type_str, pass_name, param_name, m.name);
                        location += m.location_slots;
                    }
                    members_info.clear();
                }
            }
            ctx += u8"\n";
        }

        bool const return_type_is_void =
            stage.return_type->node_type == AST_Node_Type::builtin_type && ((Builtin_Type&)*stage.return_type).type == Builtin_GLSL_Type::glsl_void;
        if(!return_type_is_void) {
            // Write vertex outputs
            anton::Array<Member_Info> members_info;
            explode_type(ctx.ctx, *stage.return_type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                ctx += anton::format(u8"layout(location = {}) out {} _pass_{}_out{};\n", location_str, type_str, pass_name, m.name);
                location += m.location_slots;
            }
            ctx += u8"\n";
        }

        // Output main
        ctx += u8"void main() {\n";
        ctx.indent();

        // Generate sourced parameters
        anton::Array<anton::String> arguments;
        {
            i64 param_index = 0;
            anton::Array<Member_Info> members_info;
            for(auto& param: stage.params) {
                Function_Parameter& p = (Function_Parameter&)*param;
                ANTON_ASSERT(is_sourced_parameter(p), u8"all parameters must be sourced");
                if(is_vertex_input_parameter(p)) {
                    ctx.write_indent();
                    stringify(ctx, *p.type);
                    anton::String arg_name = "arg" + anton::to_string(param_index);
                    ctx += u8" ";
                    ctx += arg_name;
                    ctx += u8";\n";

                    explode_type(ctx.ctx, *p.type, members_info);
                    // Write vertex input assignments
                    for(Member_Info const& m: members_info) {
                        ctx.write_indent();
                        anton::String const& param_name = p.identifier->value;
                        ctx += anton::format(u8"{}{} = _pass_{}_{}{};\n", arg_name, m.accessor, pass_name, param_name, m.name);
                    }
                    members_info.clear();
                    arguments.emplace_back(ANTON_MOV(arg_name));
                    param_index += 1;
                } else {
                    auto iter = source_definitions.find(p.source->value);
                    ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
                    anton::String_View bind_string = iter->value.bind;
                    anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, *p.identifier, *p.type);
                    if(res) {
                        arguments.emplace_back(ANTON_MOV(res.value()));
                    } else {
                        return {anton::expected_error, ANTON_MOV(res.error())};
                    }
                }
            }
        }

        ctx.write_indent();
        if(!return_type_is_void) {
            ctx += stringify_type(*stage.return_type);
            ctx += u8" _res = ";
        }

        ctx += stage_function_name;
        ctx += u8"(";
        if(arguments.size() > 0) {
            ctx += arguments[0];
            for(i64 i = 1; i < arguments.size(); ++i) {
                ctx += u8", ";
                ctx += arguments[i];
            }
        }
        ctx += u8");\n";

        if(!return_type_is_void) {
            // Write vertex output assignments
            anton::Array<Member_Info> members_info;
            explode_type(ctx.ctx, *stage.return_type, members_info);
            for(Member_Info const& m: members_info) {
                ctx.write_indent();
                ctx += anton::format(u8"_pass_{}_out{} = _res{};\n", pass_name, m.name, m.accessor);
            }
        }

        ctx.unindent();
        ctx += u8"}\n";
        return {anton::expected_value};
    }

    static anton::Expected<void, anton::String> generate_fragment_stage(Codegen_Context& ctx, Pass_Stage_Declaration const& stage,
                                                                        anton::Flat_Hash_Map<anton::String, Source_Definition> const& source_definitions) {
        anton::String const& pass_name = stage.pass_name->value;
        // Stringify the stage function
        stringify(ctx, *stage.return_type);
        ctx += u8" ";
        anton::String const stage_function_name = u8"_pass_" + pass_name + u8"_stage_fragment";
        ctx += stage_function_name;
        // param list
        ctx += u8"(";
        if(stage.params.size() > 0) {
            stringify(ctx, *stage.params[0]);
            for(i64 i = 1; i != stage.params.size(); ++i) {
                ctx += u8", ";
                stringify(ctx, *stage.params[i]);
            }
        }
        ctx += u8") {\n";
        ctx.indent();
        for(auto& statement: stage.body) {
            stringify(ctx, *statement);
        }
        ctx.unindent();
        ctx += u8"}\n\n";

        anton::Array<anton::String> arguments;
        bool const has_prev_stage_input = stage.params.size() > 0 && !is_sourced_parameter((Function_Parameter const&)*stage.params[0]);
        // Write input from the previous stage if the first parameter is an ordinary parameter
        if(has_prev_stage_input) {
            Function_Parameter const& p = (Function_Parameter const&)*stage.params[0];
            anton::Array<Member_Info> members_info;
            explode_type(ctx.ctx, *p.type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                anton::String_View interpolation_str = stringify(m.interpolation);
                ctx += anton::format(u8"layout(location = {}) {} in {} _pass_{}_in_{}{};\n", location_str, interpolation_str, type_str, pass_name,
                                     p.identifier->value, m.name);
                location += m.location_slots;
            }
            ctx += u8"\n";
            arguments.emplace_back(u8"_arg0");
        }

        // Generate parameters
        for(i64 i = has_prev_stage_input; i < stage.params.size(); ++i) {
            Function_Parameter const& p = (Function_Parameter const&)*stage.params[i];
            auto iter = source_definitions.find(p.source->value);
            ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
            anton::String_View bind_string = iter->value.bind;
            anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, *p.identifier, *p.type);
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
            explode_type(ctx.ctx, *stage.return_type, members_info);
            i64 location = 0;
            for(Member_Info const& m: members_info) {
                anton::String location_str = anton::to_string(location);
                anton::String_View type_str = stringify(m.type);
                ctx += anton::format(u8"layout(location = {}) out {} _pass_{}_out{};\n", location_str, type_str, pass_name, m.name);
                location += m.location_slots;
            }
            ctx += u8"\n";
        }

        // Output main
        ctx += u8"void main() {\n";
        ctx.indent();

        // Build _arg0 (input from the previous stage aggregated into a struct) from fragment inputs
        if(has_prev_stage_input) {
            Function_Parameter const& p = (Function_Parameter const&)*stage.params[0];
            ctx.write_indent();
            ctx += stringify_type(*p.type);
            ctx += u8" _arg0;\n";
            anton::Array<Member_Info> members_info;
            explode_type(ctx.ctx, *p.type, members_info);
            for(Member_Info const& m: members_info) {
                ctx.write_indent();
                ctx += anton::format(u8"_arg0{} = _pass_{}_in_{}{};\n", m.accessor, pass_name, p.identifier->value, m.name);
            }
        }

        // Write stage function call
        anton::String const shader_return_name{u8"_res"};
        ctx.write_indent();
        if(!return_type_is_void) {
            stringify(ctx, *stage.return_type);
            ctx += u8" ";
            // Write result name
            ctx += shader_return_name;
            ctx += u8" = ";
        }

        ctx += stage_function_name;
        ctx += u8"(";
        if(arguments.size() > 0) {
            ctx += arguments[0];
            for(i64 i = 1; i < arguments.size(); ++i) {
                ctx += u8", ";
                ctx += arguments[i];
            }
        }
        ctx += u8");\n";

        if(!return_type_is_void) {
            anton::Array<Member_Info> members_info;
            explode_type(ctx.ctx, *stage.return_type, members_info);
            for(Member_Info const& m: members_info) {
                ctx.write_indent();
                ctx += anton::format(u8"_pass_{}_out{} = {}{};\n", pass_name, m.name, shader_return_name, m.accessor);
            }
        }

        ctx.unindent();
        ctx += u8"}\n";
        return {anton::expected_value};
    }

    static anton::Expected<void, anton::String> generate_compute_stage(Codegen_Context& ctx, Pass_Stage_Declaration const& stage,
                                                                       anton::Flat_Hash_Map<anton::String, Source_Definition> const& source_definitions) {
        anton::String const& pass_name = stage.pass_name->value;
        // Stringify the stage function
        stringify(ctx, *stage.return_type);
        ctx += u8" ";
        anton::String const stage_function_name = u8"_pass_" + pass_name + u8"_stage_compute";
        ctx += stage_function_name;
        // param list
        ctx += u8"(";
        if(stage.params.size() > 0) {
            stringify(ctx, *stage.params[0]);
            for(i64 i = 1; i != stage.params.size(); ++i) {
                ctx += u8", ";
                stringify(ctx, *stage.params[i]);
            }
        }
        ctx += u8") {\n";
        ctx.indent();
        for(auto& statement: stage.body) {
            stringify(ctx, *statement);
        }
        ctx.unindent();
        ctx += u8"}\n\n";

        for(auto& attribute: stage.attributes) {
            switch(attribute->node_type) {
                case AST_Node_Type::workgroup_attribute: {
                    Workgroup_Attribute& attrib = (Workgroup_Attribute&)*attribute;
                    ctx += u8"layout(local_size_x = ";
                    stringify(ctx, *attrib.x);
                    if(attrib.y) {
                        ctx += u8", local_size_y = ";
                        stringify(ctx, *attrib.y);
                        if(attrib.z) {
                            ctx += u8", local_size_z = ";
                            stringify(ctx, *attrib.z);
                        }
                    }
                    ctx += u8") in;\n\n";
                } break;

                default:
                    break;
            }
        }

        // Output main
        ctx += u8"void main() {\n";
        ctx.indent();

        // Write stage function call
        ctx.write_indent();
        ctx += stage_function_name;
        ctx += u8"(";

        // Write arguments
        for(i64 i = 0; i < stage.params.size(); ++i) {
            Function_Parameter const& p = (Function_Parameter const&)*stage.params[i];
            auto iter = source_definitions.find(p.source->value);
            ANTON_ASSERT(iter != source_definitions.end(), u8"sourced parameter doesn't have an existing source");
            anton::String_View bind_string = iter->value.bind;
            anton::Expected<anton::String, anton::String> res = format_bind_string(bind_string, *p.identifier, *p.type);
            if(!res) {
                return {anton::expected_error, ANTON_MOV(res.error())};
            }

            if(i > 0) {
                ctx += u8", ";
            }

            ctx += res.value();
        }

        ctx += u8");\n";
        ctx.unindent();
        ctx += u8"}\n";
        return {anton::expected_value};
    }

    static void stringify_function_forward_declaration(Codegen_Context& ctx, Function_Declaration const& node) {
        stringify(ctx, *node.return_type);
        ctx += u8" ";
        stringify(ctx, *node.identifier);
        ctx += u8"(";
        if(node.params.size() > 0) {
            stringify(ctx, *node.params[0]);
            for(i64 i = 1; i != node.params.size(); ++i) {
                ctx += u8", ";
                stringify(ctx, *node.params[i]);
            }
        }
        ctx += u8");\n";
    }

    anton::Expected<anton::Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Codegen_Data const& data) {
        // We force the start line to be 4 because we are going to prepend 3 lines to the final string
        // containing the version and shader_type pragmas:
        //
        // 1 | #version <version number> core\n
        // 2 | #pragma shader_type(<type>)\n
        // 3 | \n
        // 4 | <here we will start appending code>
        //
        Codegen_Context codegen_ctx{ctx, 4};
        if(data.extensions.size() > 0) {
            for(Extension const& extension: data.extensions) {
                codegen_ctx += u8"#extension ";
                codegen_ctx += extension.name;
                codegen_ctx += u8": ";
                switch(extension.behaviour) {
                    case Extension_Behaviour::require:
                        codegen_ctx += u8"require";
                        break;
                    case Extension_Behaviour::enable:
                        codegen_ctx += u8"enable";
                        break;
                    case Extension_Behaviour::warn:
                        codegen_ctx += u8"warn";
                        break;
                    case Extension_Behaviour::disable:
                        codegen_ctx += u8"disable";
                        break;
                }
                codegen_ctx += '\n';
            }
            codegen_ctx += '\n';
        }

        anton::Array<Pass_Data> pass_datas;
        for(Pass_Context const& pass: data.passes) {
            Pass_Settings const* const this_pass_settings =
                anton::find_if(data.settings.begin(), data.settings.end(), [&pass](Pass_Settings const& settings) { return settings.pass_name == pass.name; });
            anton::Flat_Hash_Map<anton::String, Source_Definition> source_definitions;
            for(auto [key, value]: pass.sourced_data) {
                // We have to make a lot of string copies so that we can expose the variables in the public api

                anton::Array<Sourced_Variable> variables{anton::reserve, value.variables.size()};
                for(Function_Parameter const* const data: value.variables) {
                    anton::String type = stringify_type(*data->type);
                    bool const unsized = is_unsized_array(*data->type);
                    Sourced_Variable variable{data->identifier->value, ANTON_MOV(type), unsized};
                    variables.emplace_back(ANTON_MOV(variable));
                }

                anton::Array<Sourced_Opaque_Variable> opaque_variables{anton::reserve, value.opaque_variables.size()};
                for(Function_Parameter const* const data: value.opaque_variables) {
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
                if(this_pass_settings != data.settings.end()) {
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

            if(pass.structs_and_constants.size() > 0) {
                for(Declaration const* const declaration: pass.structs_and_constants) {
                    stringify(codegen_ctx, *declaration);
                }
                codegen_ctx += u8"\n";
            }

            if(source_definitions.size() > 0) {
                for(auto [source, def]: source_definitions) {
                    codegen_ctx += def.declaration;
                    codegen_ctx += U'\n';
                }
                codegen_ctx += u8"\n";
            }

            if(pass.functions.size() > 0) {
                for(Function_Declaration const* const fn: pass.functions) {
                    stringify_function_forward_declaration(codegen_ctx, *fn);
                }

                codegen_ctx += u8"\n";

                for(Function_Declaration const* const fn: pass.functions) {
                    stringify(codegen_ctx, *fn);
                    codegen_ctx += u8"\n";
                }
            }

            Pass_Data& pass_data = pass_datas.emplace_back(Pass_Data{pass.name, {}});
            if(pass.vertex_stage) {
                // We make a copy of the common part
                Codegen_Context vertex_ctx{codegen_ctx};
                anton::Expected<void, anton::String> res = generate_vertex_stage(vertex_ctx, *pass.vertex_stage, source_definitions);
                if(!res) {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }

                anton::String_View const header = u8"#version 460 core\n#pragma shader_stage(vertex)\n\n"_sv;
                anton::String const& out = vertex_ctx.get_output();
                anton::String result{anton::reserve, header.size_bytes() + out.size_bytes()};
                result += header;
                result += out;
                Code_Mappings mappings = vertex_ctx.build_code_mappings();
                pass_data.files.emplace_back(GLSL_File{ANTON_MOV(result), ANTON_MOV(mappings), Stage_Type::vertex});
            }

            if(pass.fragment_stage) {
                // We make a copy of the common part
                Codegen_Context fragment_ctx{codegen_ctx};
                anton::Expected<void, anton::String> res = generate_fragment_stage(fragment_ctx, *pass.fragment_stage, source_definitions);
                if(!res) {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }

                anton::String_View const header = u8"#version 460 core\n#pragma shader_stage(fragment)\n\n"_sv;
                anton::String const& out = fragment_ctx.get_output();
                anton::String result{anton::reserve, header.size_bytes() + out.size_bytes()};
                result += header;
                result += out;
                Code_Mappings mappings = fragment_ctx.build_code_mappings();
                pass_data.files.emplace_back(GLSL_File{ANTON_MOV(result), ANTON_MOV(mappings), Stage_Type::fragment});
            }

            if(pass.compute_stage) {
                // We make a copy of the common part
                Codegen_Context compute_ctx{codegen_ctx};
                anton::Expected<void, anton::String> res = generate_compute_stage(compute_ctx, *pass.compute_stage, source_definitions);
                if(!res) {
                    return {anton::expected_error, ANTON_MOV(res.error())};
                }

                anton::String_View const header = u8"#version 460 core\n#pragma shader_stage(compute)\n\n"_sv;
                anton::String const& out = compute_ctx.get_output();
                anton::String result{anton::reserve, header.size_bytes() + out.size_bytes()};
                result += header;
                result += out;
                Code_Mappings mappings = compute_ctx.build_code_mappings();
                pass_data.files.emplace_back(GLSL_File{ANTON_MOV(result), ANTON_MOV(mappings), Stage_Type::compute});
            }
        }

        return {anton::expected_value, ANTON_MOV(pass_datas)};
    }
} // namespace vush
