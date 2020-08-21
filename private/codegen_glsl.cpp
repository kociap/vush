#include <codegen.hpp>

#include <anton/algorithm.hpp>
#include <anton/assert.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/intrinsics.hpp>
#include <anton/math/math.hpp>
#include <anton/slice.hpp>
#include <ast.hpp>
#include <diagnostics.hpp>

namespace vush {
    struct Codegen_Context {
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
                stringify(out, *node.base, ctx);
                out += u8"++";
                return;
            }

            case AST_Node_Type::postfix_dec_expr: {
                Postfix_Dec_Expr& node = (Postfix_Dec_Expr&)ast_node;
                stringify(out, *node.base, ctx);
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
                stringify(out, *node.expr, ctx);
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

    static anton::Expected<anton::String, anton::String> format_string(String_Literal const& string,
                                                                       anton::Flat_Hash_Map<anton::String, void const*>& symbols) {
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
                return {anton::expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unterminated placeholder")};
            }

            anton::String_View const symbol_name = {iter1, iter2};
            if(symbol_name == u8"$binding") {
                auto iter = symbols.find(u8"$binding");
                // Casting the const away is legal because the pointed to variable is not const.
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
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"invalid placeholder")};
                }

                anton::String_View const iterator_name = {symbol_name.data(), dot_pos};
                anton::String_View const property_name = {symbol_name.data() + dot_pos + 1, symbol_name.bytes_end()};
                auto iter = symbols.find(iterator_name);
                if(iter == symbols.end()) {
                    Source_Info const& src = string.source_info;
                    // We add 1 to account for the opening double quote ( " )
                    i64 const string_offset = 1 + (iter1 - string.value.bytes_begin());
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unknown placeholder")};
                }

                Sourced_Data const* const data = static_cast<Sourced_Data const*>(iter->value);
                if(property_name == u8"type") {
                    out += stringify_type(*data->type);
                } else if(property_name == u8"name") {
                    out += data->name->value;
                } else {
                    Source_Info const& src = string.source_info;
                    // We add 1 to account for the opening double quote ( " )
                    i64 const string_offset = 1 + (iter1 - string.value.bytes_begin()) + dot_pos + 1;
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unknown property name")};
                }
            }

            // Skip the terminating }}
            iter2 = iter2 + 2;
            iter1 = iter2;
        }

        return {anton::expected_value, anton::move(out)};
    }

    static anton::Expected<void, anton::String> process_source_definition_statement(anton::String& out, Source_Definition_Statement const& statement,
                                                                                    Sourced_Data_Buffers const& sourced_data, Context const& ctx,
                                                                                    Codegen_Context& codegen_ctx,
                                                                                    anton::Flat_Hash_Map<anton::String, void const*>& symbols) {
        ANTON_ASSERT(statement.node_type == AST_Node_Type::source_definition_emit_statement ||
                         statement.node_type == AST_Node_Type::source_definition_for_statement ||
                         statement.node_type == AST_Node_Type::source_definition_if_statement,
                     u8"unknown node type");
        switch(statement.node_type) {
            case AST_Node_Type::source_definition_emit_statement: {
                Source_Definition_Emit_Statement const& node = (Source_Definition_Emit_Statement const&)statement;
                write_indent(out, codegen_ctx.indent);
                anton::Expected<anton::String, anton::String> result = format_string(*node.string, symbols);
                if(!result) {
                    return {anton::expected_error, anton::move(result.error())};
                }

                out += result.value();
                out += U'\n';
            } break;

            case AST_Node_Type::source_definition_if_statement: {
                Source_Definition_If_Statement const& node = (Source_Definition_If_Statement const&)statement;
                i64 count = 0;
                if(node.condition->value == u8"$variables") {
                    count = sourced_data.variables.size();
                } else if(node.condition->value == u8"$opaque_variables") {
                    count = sourced_data.opaque_variables.size();
                } else if(node.condition->value == u8"$unsized_variables") {
                    count = sourced_data.unsized_variables.size();
                } else {
                    Source_Info const& src = node.condition->source_info;
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"invalid condition expression")};
                }

                if(count) {
                    for(auto& nested_statement: node.true_branch) {
                        process_source_definition_statement(out, *nested_statement, sourced_data, ctx, codegen_ctx, symbols);
                    }
                } else {
                    for(auto& nested_statement: node.false_branch) {
                        process_source_definition_statement(out, *nested_statement, sourced_data, ctx, codegen_ctx, symbols);
                    }
                }
            } break;

            case AST_Node_Type::source_definition_for_statement: {
                Source_Definition_For_Statement const& node = (Source_Definition_For_Statement const&)statement;
                if(node.range_expr->value == u8"$variables") {
                    for(Sourced_Data const& data: sourced_data.variables) {
                        symbols.emplace(node.iterator->value, &data);
                        for(auto& nested_statement: node.statements) {
                            process_source_definition_statement(out, *nested_statement, sourced_data, ctx, codegen_ctx, symbols);
                        }
                    }
                } else if(node.range_expr->value == u8"$opaque_variables") {
                    for(Sourced_Data const& data: sourced_data.opaque_variables) {
                        symbols.emplace(node.iterator->value, &data);
                        for(auto& nested_statement: node.statements) {
                            process_source_definition_statement(out, *nested_statement, sourced_data, ctx, codegen_ctx, symbols);
                        }
                    }
                } else if(node.range_expr->value == u8"$unsized_variables") {
                    for(Sourced_Data const& data: sourced_data.unsized_variables) {
                        symbols.emplace(node.iterator->value, &data);
                        for(auto& nested_statement: node.statements) {
                            process_source_definition_statement(out, *nested_statement, sourced_data, ctx, codegen_ctx, symbols);
                        }
                    }
                } else {
                    Source_Info const& src = node.range_expr->source_info;
                    return {anton::expected_error, build_error_message(src.file_path, src.line, src.column, u8"invalid range expression")};
                }
            } break;

            default:
                ANTON_UNREACHABLE();
        }

        return {anton::expected_value};
    }

    static anton::Expected<anton::String, anton::String>
    instantiate_pass_source_templates(anton::Slice<Source_Definition_Decl const* const> const source_templates,
                                      anton::Flat_Hash_Map<anton::String, Sourced_Data_Buffers> const& sourced_data, Context const& ctx,
                                      Codegen_Context& codegen_ctx) {
        anton::Flat_Hash_Map<anton::String, void const*> symbols;
        i64 binding = 0;
        symbols.emplace(u8"$binding", &binding);
        anton::String out;
        for(Source_Definition_Decl const* source_template: source_templates) {
            auto iter = sourced_data.find(source_template->name->value);
            if(iter == sourced_data.end()) {
                continue;
            }

            for(auto& statement: source_template->decl_prop->statements) {
                anton::Expected<void, anton::String> res = process_source_definition_statement(out, *statement, iter->value, ctx, codegen_ctx, symbols);
                if(!res) {
                    return {anton::expected_error, anton::move(res.error())};
                }
            }

            out += U'\n';
        }

        return {anton::expected_value, anton::move(out)};
    }

    // TODO: Arrays in vertex inputs/fragment outputs are illegal (for now). Add error handling.

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
                anton::String nested_name = input_name + u8"_" + member->identifier->value;
                write_vertex_inputs(ctx, codegen_ctx, out, *member->type, nested_name, input_location, input_names);
            }
        } else {
            Builtin_Type const& node = (Builtin_Type const&)type;
            out += u8"layout(location = ";
            out += anton::to_string(input_location);
            out += u8") in ";
            out += stringify(node.type);
            out += u8" ";
            anton::String name = anton::String{u8"_pass_"} + codegen_ctx.current_pass + u8"_" + input_name;
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
                anton::String nested_name = name + u8"." + member->identifier->value;
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
                anton::String nested_name = output_name + u8"_" + member->identifier->value;
                write_fragment_outputs(ctx, codegen_ctx, out, *member->type, nested_name, output_location, output_names);
            }
        } else {
            Builtin_Type const& node = (Builtin_Type const&)type;
            out += u8"layout(location = ";
            out += anton::to_string(output_location);
            out += u8") out ";
            out += stringify(node.type);
            out += u8" ";
            anton::String name = anton::String{u8"_pass_"} + codegen_ctx.current_pass + u8"_" + output_name + "_out";
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
                anton::String nested_name = name + u8"." + member->identifier->value;
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

    static anton::Expected<anton::String, anton::String> format_bind_string(String_Literal const& string, Sourced_Data const& symbol) {
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
                return {anton::expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unterminated placeholder")};
            }

            anton::String_View const symbol_name = {iter1, iter2};
            i64 const dot_pos = anton::find_substring(symbol_name, u8".");
            if(dot_pos == anton::npos) {
                // If it's not a builtin and doesn't have a dot, what is it?
                Source_Info const& src = string.source_info;
                // We add 1 to account for the opening double quote ( " )
                i64 const string_offset = 1 + (iter1 - string.value.bytes_begin());
                return {anton::expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"invalid placeholder")};
            }

            anton::String_View const iterator_name = {symbol_name.data(), dot_pos};
            anton::String_View const property_name = {symbol_name.data() + dot_pos + 1, symbol_name.bytes_end()};
            if(iterator_name != u8"$variable") {
                Source_Info const& src = string.source_info;
                // We add 1 to account for the opening double quote ( " )
                i64 const string_offset = 1 + (iter1 - string.value.bytes_begin());
                return {anton::expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unknown placeholder")};
            }

            if(property_name == u8"name") {
                out += symbol.name->value;
            } else {
                Source_Info const& src = string.source_info;
                // We add 1 to account for the opening double quote ( " )
                i64 const string_offset = 1 + (iter1 - string.value.bytes_begin()) + dot_pos + 1;
                return {anton::expected_error, build_error_message(src.file_path, src.line, src.column + string_offset, u8"unknown property name")};
            }

            // Skip the terminating }}
            iter2 = iter2 + 2;
            iter1 = iter2;
        }

        return {anton::expected_value, anton::move(out)};
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

    anton::Expected<anton::Array<Pass_Data>, anton::String> generate_glsl(Context const& ctx, Declaration_List& node, Format_Options const& format) {
        anton::Array<Declaration*> structs_and_consts;
        anton::Array<Declaration*> functions;
        anton::Array<Pass_Context> passes;
        anton::Array<Source_Definition_Decl*> source_templates;
        {
            // We push sourced globals to separate array so we can later add them to all passes
            anton::Array<Sourced_Global_Decl*> sourced_globals;
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

                    case AST_Node_Type::source_definition_decl: {
                        Source_Definition_Decl* source = (Source_Definition_Decl*)decl.get();
                        source_templates.emplace_back(source);
                    } break;

                    case AST_Node_Type::sourced_global_decl: {
                        Sourced_Global_Decl* source = (Sourced_Global_Decl*)decl.get();
                        sourced_globals.emplace_back(source);
                    } break;

                    default:
                        break;
                }
            }

            for(Pass_Context& pass: passes) {
                for(Sourced_Global_Decl* global: sourced_globals) {
                    if(pass.name == global->pass_name->value) {
                        auto iter = pass.sourced_data.find_or_emplace(global->source->value);
                        Sourced_Data data{global->type.get(), global->name.get(), global->source.get()};
                        iter->value.all.emplace_back(data);
                    }
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

            // Validate that a source is available for each sourced data
            for(auto& [source_name, data]: pass.sourced_data) {
                auto iter = anton::find_if(source_templates.begin(), source_templates.end(),
                                           [&n = source_name](Source_Definition_Decl const* const v) { return v->name->value == n; });
                if(iter == source_templates.end()) {
                    Source_Info const& src = data.all[0].source->source_info;
                    return {anton::expected_error,
                            build_error_message(src.file_path, src.line, src.column, u8"unknown source definition '" + source_name + "'")};
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
                    data.variables = anton::move(perm_data);
                    layout_info = anton::move(perm_layout_info);
                }
            }
        }

        Codegen_Context codegen_ctx;
        codegen_ctx.format = format;
        codegen_ctx.indent = 0;

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
            auto instantiation_res = instantiate_pass_source_templates(source_templates, pass.sourced_data, ctx, codegen_ctx);
            if(!instantiation_res) {
                return {anton::expected_error, anton::move(instantiation_res.error())};
            }

            anton::String const& stringified_sources = instantiation_res.value();
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
                        i64 in_location = 0;
                        anton::Array<anton::String> input_names;
                        for(auto& param: stage->params) {
                            if(param->node_type == AST_Node_Type::vertex_input_param) {
                                Owning_Ptr<Vertex_Input_Param>& node = (Owning_Ptr<Vertex_Input_Param>&)param;
                                write_vertex_inputs(ctx, codegen_ctx, out, *node->type, node->identifier->value, in_location, input_names);
                            }
                        }
                        out += u8"\n";

                        anton::String const shader_return_name =
                            anton::String{u8"_pass_"} + codegen_ctx.current_pass + u8"_stage_" + stringify(codegen_ctx.current_stage) + u8"_out";
                        if(!return_type_is_void) {
                            // Write output
                            out += u8"layout(location = 0) out ";
                            out += stringify_type(*stage->return_type);
                            out += u8" ";
                            out += shader_return_name;
                            out += u8";\n\n";
                        }

                        // Output main
                        out += u8"void main() {\n";
                        codegen_ctx.indent += 1;

                        // Generate sourced parameters
                        anton::Array<anton::String> arguments;
                        {
                            anton::String const* input_names_iter = input_names.begin();
                            i64 param_index = 0;
                            for(auto& param: stage->params) {
                                ANTON_ASSERT(param->node_type == AST_Node_Type::vertex_input_param || param->node_type == AST_Node_Type::sourced_function_param,
                                             u8"invalid parameter type");
                                switch(param->node_type) {
                                    case AST_Node_Type::vertex_input_param: {
                                        Vertex_Input_Param* const node = (Vertex_Input_Param*)param.get();
                                        write_indent(out, codegen_ctx.indent);
                                        stringify(out, *node->type, codegen_ctx);
                                        out += u8" ";
                                        anton::String argument_name = "_arg" + anton::to_string(param_index);
                                        out += argument_name;
                                        out += u8";\n";
                                        write_vertex_input_assignments(ctx, codegen_ctx, out, *node->type, argument_name, input_names_iter);
                                        arguments.emplace_back(anton::move(argument_name));
                                        param_index += 1;
                                    } break;

                                    case AST_Node_Type::sourced_function_param: {
                                        Sourced_Function_Param* const node = (Sourced_Function_Param*)param.get();
                                        auto iter =
                                            anton::find_if(source_templates.cbegin(), source_templates.cend(),
                                                           [node](Source_Definition_Decl const* const v) { return v->name->value == node->source->value; });
                                        ANTON_ASSERT(iter != source_templates.cend(), u8"sourced parameter doesn't have an existing source");
                                        Sourced_Data data{node->type.get(), node->identifier.get(), node->source.get()};
                                        String_Literal const& string = *(*iter)->bind_prop->string;
                                        anton::Expected<anton::String, anton::String> res = format_bind_string(string, data);
                                        if(res) {
                                            arguments.emplace_back(anton::move(res.value()));
                                        } else {
                                            return {anton::expected_error, anton::move(res.error())};
                                        }
                                    } break;

                                    default:
                                        ANTON_UNREACHABLE();
                                }
                            }
                        }

                        write_indent(out, codegen_ctx.indent);
                        if(!return_type_is_void) {
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

                        codegen_ctx.indent -= 1;
                        out += u8"}\n";
                    } break;

                    case Stage_Type::fragment: {
                        // Decompose return type to individual outputs
                        anton::Array<anton::String> output_names;
                        if(!return_type_is_void) {
                            i64 out_location = 0;
                            // write_fragment_outputs will append underscore to the name we supply and then prepend pass name ending with underscore.
                            // We use a dummy name ("frag") to avoid the double underscore.
                            write_fragment_outputs(ctx, codegen_ctx, out, *stage->return_type, anton::String{u8"frag"}, out_location, output_names);
                            out += u8"\n";
                        }

                        // Generate parameters
                        anton::Array<anton::String> arguments;
                        {
                            bool const has_prev_stage_input = stage->params.size() > 0 && stage->params[0]->node_type == AST_Node_Type::ordinary_function_param;
                            // Write input from the previous stage if the first parameter is an ordinary parameter
                            if(has_prev_stage_input) {
                                Ordinary_Function_Param const& param = (Ordinary_Function_Param const&)*stage->params[0];
                                out += u8"layout(location = 0) ";
                                out += u8"in ";
                                out += stringify_type(*param.type);
                                anton::String name = u8"_pass_" + stage->pass->value + u8"_" + param.identifier->value + u8"_in";
                                out += u8" ";
                                out += name;
                                out += u8";\n\n";
                                arguments.emplace_back(name);
                            }

                            for(i64 i = has_prev_stage_input; i < stage->params.size(); ++i) {
                                ANTON_ASSERT(stage->params[i]->node_type == AST_Node_Type::sourced_function_param, u8"invalid parameter type");
                                Sourced_Function_Param const& param = (Sourced_Function_Param const&)*stage->params[i];
                                auto iter = anton::find_if(source_templates.cbegin(), source_templates.cend(),
                                                           [&param](Source_Definition_Decl const* const v) { return v->name->value == param.source->value; });
                                ANTON_ASSERT(iter != source_templates.cend(), u8"sourced parameter doesn't have an existing source");
                                Sourced_Data data{param.type.get(), param.identifier.get(), param.source.get()};
                                String_Literal const& string = *(*iter)->bind_prop->string;
                                anton::Expected<anton::String, anton::String> res = format_bind_string(string, data);
                                if(res) {
                                    arguments.emplace_back(anton::move(res.value()));
                                } else {
                                    return {anton::expected_error, anton::move(res.error())};
                                }
                            }
                        }

                        // Output main
                        out += u8"void main() {\n";
                        codegen_ctx.indent += 1;

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
                            anton::String const* output_names_iter = output_names.begin();
                            write_fragment_output_assignments(ctx, codegen_ctx, out, *stage->return_type, shader_return_name, output_names_iter);
                        }

                        codegen_ctx.indent -= 1;
                        out += u8"}\n";
                    } break;

                    case Stage_Type::compute: {
                        // TODO: Move return type validation to vush.cpp
                        if(!return_type_is_void) {
                            Source_Info const& src = stage->return_type->source_info;
                            return {anton::expected_error,
                                    build_error_message(src.file_path, src.line, src.column, u8"the return type of compute stage must be void")};
                        }

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
                        for(auto& param: stage->params) {
                            ANTON_ASSERT(param->node_type == AST_Node_Type::sourced_function_param, u8"invalid parameter type");
                            Sourced_Function_Param* const node = (Sourced_Function_Param*)param.get();
                            auto iter = anton::find_if(source_templates.cbegin(), source_templates.cend(),
                                                       [node](Source_Definition_Decl const* const v) { return v->name->value == node->source->value; });
                            ANTON_ASSERT(iter != source_templates.cend(), u8"sourced parameter doesn't have an existing source");
                            Sourced_Data data{node->type.get(), node->identifier.get(), node->source.get()};
                            String_Literal const& string = *(*iter)->bind_prop->string;
                            anton::Expected<anton::String, anton::String> res = format_bind_string(string, data);
                            if(res) {
                                out += res.value();
                            } else {
                                return {anton::expected_error, anton::move(res.error())};
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

                i->files.emplace_back(GLSL_File{anton::move(out), codegen_ctx.current_stage});
            }
        }

        return {anton::expected_value, anton::move(pass_data)};
    }
} // namespace vush
