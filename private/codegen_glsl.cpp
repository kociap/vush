#include <codegen.hpp>

#include <anton/algorithm.hpp>
#include <anton/assert.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/intrinsics.hpp>
#include <anton/slice.hpp>
#include <ast.hpp>
#include <utility.hpp>

namespace vush {
    struct Codegen_Context {
        anton::String_View current_pass;
        Pass_Stage_Type current_stage;
        i64 indent;
    };

    static void write_indent(anton::String& out, i64 indent) {
        for(i64 i = 0; i < indent; ++i) {
            out += u8"    ";
        }
    }

    static void stringify(anton::String& out, AST_Node& ast_node, Format_Options const& format, Codegen_Context& ctx) {
        switch(ast_node.node_type) {
            case AST_Node_Type::identifier: {
                Identifier& node = (Identifier&)ast_node;
                out += node.identifier;
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

            case AST_Node_Type::constant_declaration: {
                Constant_Declaration& node = (Constant_Declaration&)ast_node;
                out += u8"const ";
                stringify(out, *node.type, format, ctx);
                out += u8" ";
                stringify(out, *node.identifier, format, ctx);
                out += u8" = ";
                stringify(out, *node.initializer, format, ctx);
                out += u8";\n";
                return;
            }

            case AST_Node_Type::variable_declaration: {
                Variable_Declaration& node = (Variable_Declaration&)ast_node;
                stringify(out, *node.type, format, ctx);
                out += u8" ";
                stringify(out, *node.identifier, format, ctx);
                if(node.initializer) {
                    out += u8" = ";
                    stringify(out, *node.initializer, format, ctx);
                }
                out += u8";\n";
                return;
            }

            case AST_Node_Type::struct_decl: {
                Struct_Decl& node = (Struct_Decl&)ast_node;
                out += u8"struct ";
                stringify(out, *node.name, format, ctx);
                out += u8" {\n";
                ctx.indent += 1;
                for(auto& member: node.members) {
                    write_indent(out, ctx.indent);
                    stringify(out, *member->type, format, ctx);
                    out += u8" ";
                    stringify(out, *member->identifier, format, ctx);
                    out += u8";\n";
                }
                ctx.indent -= 1;
                out += u8"};\n";
                return;
            }

            case AST_Node_Type::function_declaration: {
                Function_Declaration& node = (Function_Declaration&)ast_node;
                stringify(out, *node.return_type, format, ctx);
                out += u8" ";
                stringify(out, *node.name, format, ctx);
                stringify(out, *node.param_list, format, ctx);
                out += u8" {\n";
                ctx.indent += 1;
                Statement_List& body = (Statement_List&)*node.body;
                stringify(out, body, format, ctx);
                ctx.indent -= 1;
                out += u8"}\n";
                return;
            }

            case AST_Node_Type::function_param_list: {
                Function_Param_List& node = (Function_Param_List&)ast_node;
                out += u8"(";
                if(node.params.size() > 0) {
                    stringify(out, *node.params[0], format, ctx);

                    for(i64 i = 1; i != node.params.size(); ++i) {
                        out += u8", ";
                        stringify(out, *node.params[i], format, ctx);
                    }
                }

                out += u8")";
                return;
            }

            case AST_Node_Type::ordinary_function_param: {
                Ordinary_Function_Param& node = (Ordinary_Function_Param&)ast_node;
                stringify(out, *node.type, format, ctx);
                out += u8" ";
                stringify(out, *node.identifier, format, ctx);
                return;
            }

            case AST_Node_Type::sourced_function_param: {
                Sourced_Function_Param& node = (Sourced_Function_Param&)ast_node;
                stringify(out, *node.type, format, ctx);
                out += u8" ";
                stringify(out, *node.identifier, format, ctx);
                out += u8" from ";
                stringify(out, *node.source, format, ctx);
                return;
            }

            case AST_Node_Type::vertex_input_param: {
                Vertex_Input_Param& node = (Vertex_Input_Param&)ast_node;
                stringify(out, *node.type, format, ctx);
                out += u8" ";
                stringify(out, *node.identifier, format, ctx);
                return;
            }

            case AST_Node_Type::statement_list: {
                Statement_List& node = (Statement_List&)ast_node;
                for(auto& statement: node.statements) {
                    stringify(out, *statement, format, ctx);
                }
                return;
            }

            case AST_Node_Type::block_statement: {
                Block_Statement& node = (Block_Statement&)ast_node;
                write_indent(out, ctx.indent);
                out += u8"{\n";
                ctx.indent += 1;
                stringify(out, *node.statements, format, ctx);
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
                    stringify(out, *node->condition, format, ctx);
                    out += u8") {\n";
                    ctx.indent += 1;
                    Block_Statement& true_statement = (Block_Statement&)*node->true_statement;
                    for(auto& statement: true_statement.statements->statements) {
                        stringify(out, *statement, format, ctx);
                    }
                    ctx.indent -= 1;
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
                    for(auto& statement: false_statement.statements->statements) {
                        stringify(out, *statement, format, ctx);
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
                stringify(out, *node.match_expr, format, ctx);
                out += u8") {\n";
                for(auto& switch_node: node.cases) {
                    write_indent(out, ctx.indent + 1);
                    if(switch_node->node_type == AST_Node_Type::case_statement) {
                        Case_Statement& switch_case = (Case_Statement&)*switch_node;
                        out += u8"case ";
                        stringify(out, *switch_case.condition, format, ctx);
                        out += ":\n";
                        ctx.indent += 2;
                        stringify(out, *switch_case.statements, format, ctx);
                        ctx.indent -= 2;
                    } else {
                        Default_Case_Statement& switch_case = (Default_Case_Statement&)*switch_node;
                        out += u8"default:\n";
                        ctx.indent += 2;
                        stringify(out, *switch_case.statements, format, ctx);
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
                // We stringify the variable_decl manually because we need inline declaration;
                Variable_Declaration& decl = *node.declaration;
                stringify(out, *decl.type, format, ctx);
                out += u8" ";
                stringify(out, *decl.identifier, format, ctx);
                out += u8" = ";
                stringify(out, *decl.initializer, format, ctx);
                out += u8"; ";
                stringify(out, *node.condition, format, ctx);
                out += u8"; ";
                stringify(out, *node.post_expression, format, ctx);
                // We need no-braces block. We add them inline ourselves.
                out += u8") {\n";
                ctx.indent += 1;
                for(auto& statement: node.block->statements->statements) {
                    stringify(out, *statement, format, ctx);
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
                stringify(out, *node.condition, format, ctx);
                // We need no-braces block. We add them inline ourselves.
                out += u8") {\n";
                ctx.indent += 1;
                for(auto& statement: node.block->statements->statements) {
                    stringify(out, *statement, format, ctx);
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
                for(auto& statement: node.block->statements->statements) {
                    stringify(out, *statement, format, ctx);
                }
                ctx.indent -= 1;
                write_indent(out, ctx.indent);
                out += u8"} while(";
                stringify(out, *node.condition, format, ctx);
                out += u8");\n";
                return;
            }

            case AST_Node_Type::return_statement: {
                Return_Statement& node = (Return_Statement&)ast_node;
                write_indent(out, ctx.indent);
                out += u8"return ";
                stringify(out, *node.return_expr, format, ctx);
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
                stringify(out, *node.declaration, format, ctx);
                return;
            }

            case AST_Node_Type::expression_statement: {
                Expression_Statement& node = (Expression_Statement&)ast_node;
                write_indent(out, ctx.indent);
                stringify(out, *node.expr, format, ctx);
                out += u8";\n";
                return;
            }

            case AST_Node_Type::assignment_expression: {
                Assignment_Expression& node = (Assignment_Expression&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" = ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::arithmetic_assignment_expression: {
                Arithmetic_Assignment_Expression& node = (Arithmetic_Assignment_Expression&)ast_node;
                stringify(out, *node.lhs, format, ctx);
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
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::elvis_expr: {
                Elvis_Expr& node = (Elvis_Expr&)ast_node;
                stringify(out, *node.condition, format, ctx);
                out += u8" ? ";
                stringify(out, *node.true_expr, format, ctx);
                out += u8" : ";
                stringify(out, *node.false_expr, format, ctx);
                return;
            }

            case AST_Node_Type::logic_or_expr: {
                Logic_Or_Expr& node = (Logic_Or_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" || ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::logic_xor_expr: {
                Logic_Xor_Expr& node = (Logic_Xor_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" ^^ ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::logic_and_expr: {
                Logic_And_Expr& node = (Logic_And_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" && ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::relational_equality_expression: {
                Relational_Equality_Expression& node = (Relational_Equality_Expression&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += (node.is_equality ? u8" == " : u8" != ");
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::relational_expression: {
                Relational_Expression& node = (Relational_Expression&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                switch(node.type) {
                    case Relational_Type::greater_equal: {
                        out += u8" >= ";
                    } break;

                    case Relational_Type::greater_than: {
                        out += u8" > ";
                    } break;

                    case Relational_Type::less_equal: {
                        out += u8" <= ";
                    } break;

                    case Relational_Type::less_than: {
                        out += u8" < ";
                    } break;
                }
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::bit_or_expr: {
                Bit_Or_Expr& node = (Bit_Or_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" | ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::bit_xor_expr: {
                Bit_Xor_Expr& node = (Bit_Xor_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" ^ ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::bit_and_expr: {
                Bit_And_Expr& node = (Bit_And_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" & ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::lshift_expr: {
                LShift_Expr& node = (LShift_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" << ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::rshift_expr: {
                RShift_Expr& node = (RShift_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" >> ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::add_expr: {
                Add_Expr& node = (Add_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" + ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::sub_expr: {
                Sub_Expr& node = (Sub_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" - ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::mul_expr: {
                Mul_Expr& node = (Mul_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" * ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::div_expr: {
                Div_Expr& node = (Div_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" / ";
                stringify(out, *node.rhs, format, ctx);
                return;
            }

            case AST_Node_Type::mod_expr: {
                Mod_Expr& node = (Mod_Expr&)ast_node;
                stringify(out, *node.lhs, format, ctx);
                out += u8" % ";
                stringify(out, *node.rhs, format, ctx);
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
                stringify(out, *node.expression, format, ctx);
                return;
            }

            case AST_Node_Type::prefix_inc_expr: {
                Prefix_Inc_Expr& node = (Prefix_Inc_Expr&)ast_node;
                out += u8"++";
                stringify(out, *node.expression, format, ctx);
                return;
            }

            case AST_Node_Type::prefix_dec_expr: {
                Prefix_Dec_Expr& node = (Prefix_Dec_Expr&)ast_node;
                out += u8"--";
                stringify(out, *node.expression, format, ctx);
                return;
            }

            case AST_Node_Type::argument_list: {
                Argument_List& node = (Argument_List&)ast_node;
                if(node.arguments.size() > 0) {
                    stringify(out, *node.arguments[0], format, ctx);

                    for(i64 i = 1; i != node.arguments.size(); ++i) {
                        out += u8", ";
                        stringify(out, *node.arguments[i], format, ctx);
                    }
                }
                return;
            }

            case AST_Node_Type::function_call_expression: {
                Function_Call_Expression& node = (Function_Call_Expression&)ast_node;
                stringify(out, *node.identifier, format, ctx);
                out += u8"(";
                stringify(out, *node.arg_list, format, ctx);
                out += u8")";
                return;
            }

            case AST_Node_Type::member_access_expression: {
                Member_Access_Expression& node = (Member_Access_Expression&)ast_node;
                stringify(out, *node.base, format, ctx);
                out += u8".";
                stringify(out, *node.member, format, ctx);
                return;
            }

            case AST_Node_Type::array_access_expression: {
                Array_Access_Expression& node = (Array_Access_Expression&)ast_node;
                stringify(out, *node.base, format, ctx);
                out += u8"[";
                stringify(out, *node.index, format, ctx);
                out += u8"]";
                return;
            }

            case AST_Node_Type::postfix_inc_expr: {
                Postfix_Inc_Expr& node = (Postfix_Inc_Expr&)ast_node;
                stringify(out, *node.base, format, ctx);
                out += u8"++";
                return;
            }

            case AST_Node_Type::postfix_dec_expr: {
                Postfix_Dec_Expr& node = (Postfix_Dec_Expr&)ast_node;
                stringify(out, *node.base, format, ctx);
                out += u8"--";
                return;
            }

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& node = (Identifier_Expression&)ast_node;
                stringify(out, *node.identifier, format, ctx);
                return;
            }

            case AST_Node_Type::paren_expr: {
                Paren_Expr& node = (Paren_Expr&)ast_node;
                out += u8"(";
                stringify(out, *node.expr, format, ctx);
                out += u8")";
                return;
            }

            case AST_Node_Type::bool_literal: {
                Bool_Literal& node = (Bool_Literal&)ast_node;
                out += node.value ? u8"true" : u8"false";
                return;
            }

            case AST_Node_Type::integer_literal: {
                Integer_Literal& node = (Integer_Literal&)ast_node;
                out += node.value;
                return;
            }

            case AST_Node_Type::float_literal: {
                Float_Literal& node = (Float_Literal&)ast_node;
                out += node.value;
                return;
            }

            case AST_Node_Type::declaration_list:
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

    static void stringify_function_forward_decl(anton::String& out, Function_Declaration& node, Format_Options const& format, Codegen_Context& ctx) {
        stringify(out, *node.return_type, format, ctx);
        out += u8" ";
        stringify(out, *node.name, format, ctx);
        stringify(out, *node.param_list, format, ctx);
        out += u8";\n";
    }

    static Expected<anton::String, anton::String> format_string(String_Literal const& string, anton::Flat_Hash_Map<anton::String, void*>& symbols) {
        anton::String out;
        auto iter1 = string.value.bytes_begin();
        auto iter2 = string.value.bytes_begin();
        auto const end = string.value.bytes_end();
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

            // Check for unterminated placeholder
            if(iter2 == end || iter2 + 1 == end || *(iter2 + 1) != U'}') {
                Source_Info const& src = string.source_info;
                // We add 1 to account for the opening double quote ( " )
                i64 const string_offset = 1 + (iter2 - string.value.bytes_begin());
                return {expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unterminated placeholder")};
            }

            anton::String_View const symbol_name = {iter1, iter2};
            if(symbol_name == u8"$binding") {
                auto iter = symbols.find(u8"$binding");
                i64* binding = (i64*)iter->value;
                out += anton::to_string(*binding);
                *binding += 1;
            } else {
                i64 const dot_pos = anton::find_substring(symbol_name, u8".");
                if(dot_pos == anton::npos) {
                    // If it's not a builtin and doesn't have a dot, what is it?
                    Source_Info const& src = string.source_info;
                    // We add 1 to account for the opening double quote ( " )
                    i64 const string_offset = 1 + (iter1 - string.value.bytes_begin());
                    return {expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"invalid placeholder")};
                }

                anton::String_View const iterator_name = {symbol_name.data(), dot_pos};
                anton::String_View const property_name = {symbol_name.data() + dot_pos + 1, symbol_name.bytes_end()};
                auto iter = symbols.find(iterator_name);
                if(iter == symbols.end()) {
                    Source_Info const& src = string.source_info;
                    // We add 1 to account for the opening double quote ( " )
                    i64 const string_offset = 1 + (iter1 - string.value.bytes_begin());
                    return {expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unknown placeholder")};
                }

                Sourced_Function_Param* sourced_param = static_cast<Sourced_Function_Param*>(iter->value);
                if(property_name == u8"type") {
                    out += stringify_type(*sourced_param->type);
                } else if(property_name == u8"name") {
                    out += sourced_param->identifier->identifier;
                } else {
                    Source_Info const& src = string.source_info;
                    // We add 1 to account for the opening double quote ( " )
                    i64 const string_offset = 1 + (iter1 - string.value.bytes_begin()) + dot_pos + 1;
                    return {expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unknown property name")};
                }
            }

            // Skip the terminating }}
            iter2 = iter2 + 2;
            iter1 = iter2;
        }

        return {expected_value, anton::move(out)};
    }

    static Expected<bool, anton::String> process_source_definition_statement(anton::String& out, Source_Definition_Statement const& statement,
                                                                             anton::Slice<Sourced_Function_Param*> const sourced_params, Context const& ctx,
                                                                             Format_Options const& format, Codegen_Context& codegen_ctx,
                                                                             anton::Flat_Hash_Map<anton::String, void*>& symbols) {
        ANTON_ASSERT(statement.node_type == AST_Node_Type::source_definition_emit_statement ||
                         statement.node_type == AST_Node_Type::source_definition_for_statement,
                     u8"unknown node type");
        switch(statement.node_type) {
            case AST_Node_Type::source_definition_emit_statement: {
                Source_Definition_Emit_Statement const& node = (Source_Definition_Emit_Statement const&)statement;
                write_indent(out, codegen_ctx.indent);
                Expected<anton::String, anton::String> result = format_string(*node.string, symbols);
                if(!result) {
                    return {expected_error, anton::move(result.error())};
                }

                out += result.value();
                out += U'\n';
            } break;

            case AST_Node_Type::source_definition_for_statement: {
                Source_Definition_For_Statement const& node = (Source_Definition_For_Statement const&)statement;
                if(node.range_expr->identifier == u8"$variables") {
                    for(Sourced_Function_Param* param: sourced_params) {
                        symbols.emplace(node.iterator->identifier, param);
                        ANTON_ASSERT(param->type->node_type == AST_Node_Type::builtin_type || param->type->node_type == AST_Node_Type::user_defined_type,
                                     u8"unknown ast node type");
                        if(Type* type = param->type.get();
                           type->node_type == AST_Node_Type::builtin_type && is_opaque_type(static_cast<Builtin_Type*>(type)->type)) {
                            continue;
                        }

                        for(auto& nested_statement: node.statements) {
                            process_source_definition_statement(out, *nested_statement, sourced_params, ctx, format, codegen_ctx, symbols);
                        }
                    }
                } else {
                    Source_Info const& src = node.range_expr->source_info;
                    return {expected_error, build_error_message(src.file_path, src.line, src.column, u8"invalid range expression")};
                }
            } break;

            default:
                ANTON_UNREACHABLE();
        }

        return {expected_value, true};
    }

    // The size of sourced_params should be > 0, otherwise empty definitions will be generated.
    static Expected<bool, anton::String> instantiate_source_template(anton::String& out, Source_Definition const& source_def,
                                                                     anton::Slice<Sourced_Function_Param*> const sourced_params, Context const& ctx,
                                                                     Format_Options const& format, Codegen_Context& codegen_ctx,
                                                                     anton::Flat_Hash_Map<anton::String, void*>& symbols) {
        for(auto& statement: source_def.decl_prop->statements) {
            Expected<bool, anton::String> result = process_source_definition_statement(out, *statement, sourced_params, ctx, format, codegen_ctx, symbols);
            if(!result) {
                return {expected_error, anton::move(result.error())};
            }
        }

        return {expected_value, true};
    }

    static void write_vertex_inputs(Context const& ctx, Codegen_Context& codegen_ctx, anton::String& out, Type const& type, anton::String const& input_name,
                                    i64& input_location, anton::Array<anton::String>& input_names) {
        // TODO: fix location increment for types that require more than 1 slot
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type, "unknown ast node type");
        if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& node = (User_Defined_Type const&)type;
            Symbol const* symbol = find_symbol(ctx, node.name);
            ANTON_ASSERT(symbol, "undefined symbol");
            Struct_Decl const* struct_decl = (Struct_Decl const*)symbol->declaration;
            for(auto& member: struct_decl->members) {
                anton::String nested_name = input_name + u8"_" + member->identifier->identifier;
                write_vertex_inputs(ctx, codegen_ctx, out, *member->type, nested_name, input_location, input_names);
            }
        } else {
            Builtin_Type const& node = (Builtin_Type const&)type;
            out += u8"layout(location = ";
            out += anton::to_string(input_location);
            out += u8") in ";
            out += stringify(node.type);
            out += u8" ";
            anton::String name = u8"_pass_" + codegen_ctx.current_pass + u8"_" + input_name + "_in";
            out += name;
            out += u8";\n";
            input_names.emplace_back(anton::move(name));
            input_location += 1;
        }
    }

    static void write_vertex_input_assignments(Context const& ctx, Codegen_Context& codegen_ctx, anton::String& out, Type const& type,
                                               anton::String const& name, anton::String const*& input_names) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type, "unknown ast node type");
        if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& node = (User_Defined_Type const&)type;
            Symbol const* symbol = find_symbol(ctx, node.name);
            ANTON_ASSERT(symbol, "undefined symbol");
            Struct_Decl const* struct_decl = (Struct_Decl const*)symbol->declaration;
            for(auto& member: struct_decl->members) {
                anton::String nested_name = name + u8"." + member->identifier->identifier;
                write_vertex_input_assignments(ctx, codegen_ctx, out, *member->type, nested_name, input_names);
            }
        } else {
            // Builtin_Type
            write_indent(out, codegen_ctx.indent);
            out += name;
            out += u8" = ";
            out += *input_names;
            out += u8";\n";
            input_names += 1;
        }
    }

    static void write_fragment_outputs(Context const& ctx, Codegen_Context& codegen_ctx, anton::String& out, Type const& type, anton::String const& output_name,
                                       i64& output_location, anton::Array<anton::String>& output_names) {
        // TODO: fix location increment for types that require more than 1 slot
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type, "unknown ast node type");
        if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& node = (User_Defined_Type const&)type;
            Symbol const* symbol = find_symbol(ctx, node.name);
            ANTON_ASSERT(symbol, "undefined symbol");
            Struct_Decl const* struct_decl = (Struct_Decl const*)symbol->declaration;
            for(auto& member: struct_decl->members) {
                anton::String nested_name = output_name + u8"_" + member->identifier->identifier;
                write_fragment_outputs(ctx, codegen_ctx, out, *member->type, nested_name, output_location, output_names);
            }
        } else {
            Builtin_Type const& node = (Builtin_Type const&)type;
            out += u8"layout(location = ";
            out += anton::to_string(output_location);
            out += u8") out ";
            out += stringify(node.type);
            out += u8" ";
            anton::String name = u8"_pass_" + codegen_ctx.current_pass + u8"_" + output_name + "_out";
            out += name;
            out += u8";\n";
            output_names.emplace_back(anton::move(name));
            output_location += 1;
        }
    }

    static void write_fragment_output_assignments(Context const& ctx, Codegen_Context& codegen_ctx, anton::String& out, Type const& type,
                                                  anton::String const& name, anton::String const*& output_names) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type, "unknown ast node type");
        if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& node = (User_Defined_Type const&)type;
            Symbol const* symbol = find_symbol(ctx, node.name);
            ANTON_ASSERT(symbol, "undefined symbol");
            Struct_Decl const* struct_decl = (Struct_Decl const*)symbol->declaration;
            for(auto& member: struct_decl->members) {
                anton::String nested_name = name + u8"." + member->identifier->identifier;
                write_fragment_output_assignments(ctx, codegen_ctx, out, *member->type, nested_name, output_names);
            }
        } else {
            // Builtin_Type
            write_indent(out, codegen_ctx.indent);
            out += *output_names;
            out += u8" = ";
            out += name;
            out += u8";\n";
            output_names += 1;
        }
    }

    Expected<anton::Array<GLSL_File>, anton::String> generate_glsl(Context const& ctx, Declaration_List& node, Format_Options const& format) {
        anton::Array<Declaration*> structs_and_consts;
        anton::Array<Declaration*> functions;
        anton::Array<Declaration*> pass_stages;
        anton::Flat_Hash_Map<anton::String, anton::Array<Sourced_Function_Param*>> sourced_params;
        anton::Array<Source_Definition*> source_templates;
        for(auto& decl: node.declarations) {
            switch(decl->node_type) {
                case AST_Node_Type::struct_decl:
                case AST_Node_Type::constant_declaration: {
                    structs_and_consts.emplace_back(decl.get());
                } break;

                case AST_Node_Type::function_declaration: {
                    functions.emplace_back(decl.get());
                } break;

                case AST_Node_Type::pass_stage_declaration: {
                    pass_stages.emplace_back(decl.get());
                    Owning_Ptr<Pass_Stage_Declaration>& pass_stage = (Owning_Ptr<Pass_Stage_Declaration>&)decl;
                    for(auto& param: pass_stage->param_list->params) {
                        if(param->node_type == AST_Node_Type::sourced_function_param) {
                            Sourced_Function_Param* sourced_param = (Sourced_Function_Param*)param.get();
                            auto iter = sourced_params.find_or_emplace(sourced_param->source->identifier);
                            iter->value.emplace_back(sourced_param);
                        }
                    }
                } break;

                case AST_Node_Type::source_definition: {
                    Source_Definition* source = (Source_Definition*)decl.get();
                    source_templates.emplace_back(source);
                    break;
                }
            }
        }

        Codegen_Context codegen_ctx;
        codegen_ctx.indent = 0;

        anton::String common;
        common += "#version 450 core\n";

        for(Declaration* decl: structs_and_consts) {
            stringify(common, *decl, format, codegen_ctx);
        }

        common += u8"\n";

        for(Declaration* decl: functions) {
            stringify_function_forward_decl(common, (Function_Declaration&)*decl, format, codegen_ctx);
        }

        common += u8"\n";

        for(Declaration* decl: functions) {
            stringify(common, (Function_Declaration&)*decl, format, codegen_ctx);
        }

        common += u8"\n";

        {
            i64 binding = 0;
            anton::Flat_Hash_Map<anton::String, void*> symbols;
            symbols.emplace(u8"$binding", &binding);

            for(Source_Definition* source_template: source_templates) {
                auto iter = sourced_params.find(source_template->name->identifier);
                if(iter != sourced_params.end()) {
                    // TODO: Use stable sort to preserve the order and report duplicates in the correct order.
                    anton::quick_sort(iter->value.begin(), iter->value.end(), [](Sourced_Function_Param* const lhs, Sourced_Function_Param* const rhs) {
                        // TODO: String comparison function.
                        anton::String const& lhs_str = lhs->identifier->identifier;
                        anton::String const& rhs_str = rhs->identifier->identifier;
                        auto lhs_i = lhs_str.bytes_begin(), rhs_i = rhs_str.bytes_begin(), lhs_end = lhs_str.bytes_end(), rhs_end = rhs_str.bytes_end();
                        for(; lhs_i != lhs_end && rhs_i != rhs_end; ++lhs_i, ++rhs_i) {
                            if(*lhs_i < *rhs_i) {
                                return true;
                            }
                        }

                        return lhs_i == lhs_end && rhs_i != rhs_end;
                    });

                    // Ensure there are no name duplicates with different types
                    for(auto i = iter->value.begin(), j = iter->value.begin() + 1, end = iter->value.end(); j != end; ++i, ++j) {
                        anton::String_View const i_type = stringify_type(*(*i)->type);
                        anton::String_View const j_type = stringify_type(*(*j)->type);
                        if((*i)->identifier->identifier == (*j)->identifier->identifier && i_type != j_type) {
                            Source_Info const& src = (*j)->identifier->source_info;
                            return {expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"duplicate sourced parameter name with a different type")};
                        }
                    }

                    // Remove type duplicates
                    auto end = anton::unique(iter->value.begin(), iter->value.end(), [](Sourced_Function_Param* lhs, Sourced_Function_Param* rhs) {
                        anton::String_View const i_type = stringify_type(*lhs->type);
                        anton::String_View const j_type = stringify_type(*rhs->type);
                        return i_type == j_type;
                    });

                    Expected<bool, anton::String> result =
                        instantiate_source_template(common, *source_template, {iter->value.begin(), end}, ctx, format, codegen_ctx, symbols);
                    if(!result) {
                        return {expected_error, anton::move(result.error())};
                    }
                    common += U'\n';
                }
            }
        }

        common += u8"\n";

        anton::Array<GLSL_File> files(anton::reserve, pass_stages.size());
        for(Declaration* decl: pass_stages) {
            Pass_Stage_Declaration& stage = (Pass_Stage_Declaration&)*decl;
            anton::String pass_function_name = u8"_pass_" + stage.pass->identifier + u8"_stage_" + stringify(stage.stage);
            anton::String out = common;

            codegen_ctx.current_pass = stage.pass->identifier;
            codegen_ctx.current_stage = stage.stage;

            switch(stage.stage) {
                case Pass_Stage_Type::vertex: {
                    i64 in_location = 0;
                    anton::Array<anton::String> input_names;
                    for(auto& param: stage.param_list->params) {
                        if(param->node_type == AST_Node_Type::vertex_input_param) {
                            Owning_Ptr<Vertex_Input_Param>& node = (Owning_Ptr<Vertex_Input_Param>&)param;
                            write_vertex_inputs(ctx, codegen_ctx, out, *node->type, node->identifier->identifier, in_location, input_names);
                        } else {
                        }
                    }
                    out += u8"\n";

                    // Stringify the stage function
                    stringify(out, *stage.return_type, format, codegen_ctx);
                    out += u8" ";
                    out += pass_function_name;
                    stringify(out, *stage.param_list, format, codegen_ctx);
                    out += u8" {\n";
                    codegen_ctx.indent += 1;
                    stringify(out, *stage.body, format, codegen_ctx);
                    codegen_ctx.indent -= 1;
                    out += u8"}\n\n";

                    // Write output
                    anton::String const shader_return_name =
                        u8"_pass_" + codegen_ctx.current_pass + u8"_stage_" + stringify(codegen_ctx.current_stage) + u8"_out";
                    out += u8"out ";
                    out += stringify_type(*stage.return_type);
                    out += u8" ";
                    out += shader_return_name;
                    out += u8";\n\n";

                    // Output main
                    out += u8"void main() {\n";
                    codegen_ctx.indent += 1;

                    // Output sourced parameters
                    anton::String const* input_names_iter = input_names.begin();
                    i64 param_index = 0;
                    anton::Array<anton::String> arguments;
                    for(auto& param: stage.param_list->params) {
                        if(param->node_type == AST_Node_Type::vertex_input_param) {
                            Owning_Ptr<Vertex_Input_Param>& node = (Owning_Ptr<Vertex_Input_Param>&)param;
                            write_indent(out, codegen_ctx.indent);
                            stringify(out, *node->type, format, codegen_ctx);
                            out += u8" ";
                            anton::String argument_name = "_arg" + anton::to_string(param_index);
                            out += argument_name;
                            out += u8";\n";
                            write_vertex_input_assignments(ctx, codegen_ctx, out, *node->type, argument_name, input_names_iter);
                            arguments.emplace_back(anton::move(argument_name));
                            param_index += 1;
                        } else {
                        }
                    }

                    write_indent(out, codegen_ctx.indent);
                    out += shader_return_name;
                    out += u8" = ";
                    out += pass_function_name;
                    out += u8"(";
                    if(arguments.size() > 0) {
                        out += arguments[0];
                        for(i64 i = 1; i < arguments.size(); ++i) {
                            out += u8", ";
                            out += arguments[i];
                        }
                    }
                    out += u8");\n";

                    codegen_ctx.indent -= 1;
                    out += u8"}\n";
                } break;

                case Pass_Stage_Type::fragment: {
                    // TODO: validate there are no vertex input parameters
                    // Stringify the stage function
                    stringify(out, *stage.return_type, format, codegen_ctx);
                    out += u8" ";
                    out += pass_function_name;
                    stringify(out, *stage.param_list, format, codegen_ctx);
                    out += u8" {\n";
                    codegen_ctx.indent += 1;
                    stringify(out, *stage.body, format, codegen_ctx);
                    codegen_ctx.indent -= 1;
                    out += u8"}\n\n";

                    // Decompose return type to individual outputs
                    anton::Array<anton::String> output_names;
                    i64 out_location = 0;
                    // write_fragment_outputs will append underscore to the name we supply and then prepend pass name ending with underscore.
                    // We use a dummy name ("frag") to avoid the double underscore.
                    write_fragment_outputs(ctx, codegen_ctx, out, *stage.return_type, u8"frag", out_location, output_names);
                    out += u8"\n";

                    // Output main
                    out += u8"void main() {\n";
                    codegen_ctx.indent += 1;

                    // Write stage function call
                    write_indent(out, codegen_ctx.indent);
                    out += stringify_type(*stage.return_type);
                    out += u8" ";
                    // Write result name
                    anton::String const shader_return_name = u8"_res";
                    out += shader_return_name;
                    out += u8" = ";
                    out += pass_function_name;
                    out += u8"();\n";

                    anton::String const* output_names_iter = output_names.begin();
                    write_fragment_output_assignments(ctx, codegen_ctx, out, *stage.return_type, shader_return_name, output_names_iter);

                    codegen_ctx.indent -= 1;
                    out += u8"}\n";
                } break;
            }

            files.emplace_back(anton::move(out));
        }

        return {expected_value, anton::move(files)};
    }
} // namespace vush
