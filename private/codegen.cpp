#include <codegen.hpp>

#include <ast.hpp>
#include <utility.hpp>

namespace vush {
    static void write_indent(anton::String& out, i64 indent) {
        for(i64 i = 0; i < indent; ++i) {
            out += u8"    ";
        }
    }

    static void stringify(anton::String& out, Syntax_Tree_Node& ast_node) {
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
                stringify(out, *node.type);
                out += u8" ";
                stringify(out, *node.identifier);
                out += u8" = ";
                stringify(out, *node.initializer);
                out += u8";\n";
                return;
            }

            case AST_Node_Type::variable_declaration: {
                Variable_Declaration& node = (Variable_Declaration&)ast_node;
                stringify(out, *node.type);
                out += u8" ";
                stringify(out, *node.identifier);
                if(node.initializer) {
                    out += u8" = ";
                    stringify(out, *node.initializer);
                }
                out += u8";\n";
                return;
            }

            case AST_Node_Type::struct_decl: {
                Struct_Decl& node = (Struct_Decl&)ast_node;
                out += u8"struct ";
                stringify(out, *node.name);
                out += u8" {\n";
                for(auto& member: node.members) {
                    out += u8"    ";
                    stringify(out, *member->type);
                    out += u8" ";
                    stringify(out, *member->identifier);
                    out += u8";\n";
                }
                out += u8"};\n";
                return;
            }

            case AST_Node_Type::function_declaration: {
                Function_Declaration& node = (Function_Declaration&)ast_node;
                stringify(out, *node.return_type);
                out += u8" ";
                stringify(out, *node.name);
                stringify(out, *node.param_list);
                out += u8" ";
                stringify(out, *node.body);
                out += u8"\n";
                return;
            }

            case AST_Node_Type::function_param_list: {
                Function_Param_List& node = (Function_Param_List&)ast_node;
                out += u8"(";
                if(node.params.size() > 0) {
                    stringify(out, *node.params[0]);

                    for(i64 i = 1; i != node.params.size(); ++i) {
                        out += u8", ";
                        stringify(out, *node.params[i]);
                    }
                }

                out += u8")";
                return;
            }

            case AST_Node_Type::ordinary_function_param: {
                Ordinary_Function_Param& node = (Ordinary_Function_Param&)ast_node;
                stringify(out, *node.type);
                out += u8" ";
                stringify(out, *node.identifier);
                return;
            }

            case AST_Node_Type::function_body: {
                Function_Body& node = (Function_Body&)ast_node;
                out += u8"{\n";
                stringify(out, *node.statement_list);
                out += u8"}";
                return;
            }

            case AST_Node_Type::statement_list: {
                Statement_List& node = (Statement_List&)ast_node;
                for(auto& statement: node.statements) {
                    stringify(out, *statement);
                }
                return;
            }

            case AST_Node_Type::declaration_statement: {
                Declaration_Statement& node = (Declaration_Statement&)ast_node;
                stringify(out, *node.declaration);
                return;
            }

            case AST_Node_Type::if_statement: {
                If_Statement& node = (If_Statement&)ast_node;
                out += u8"if(";
                stringify(out, *node.condition);
                out += u8") {\n";
                stringify(out, *node.true_statement);
                out += u8"}";
                if(node.false_statement) {
                    out += u8" else {\n";
                    stringify(out, *node.false_statement);
                }
                out += u8"\n";
                return;
            }

            case AST_Node_Type::block_statement: {
                Block_Statement& node = (Block_Statement&)ast_node;
                stringify(out, *node.statements);
                return;
            }

            case AST_Node_Type::expression_statement: {
                Expression_Statement& node = (Expression_Statement&)ast_node;
                stringify(out, *node.expr);
                out += u8";\n";
                return;
            }

            case AST_Node_Type::assignment_expression: {
                Assignment_Expression& node = (Assignment_Expression&)ast_node;
                stringify(out, *node.lhs);
                out += u8" = ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::arithmetic_assignment_expression: {
                Arithmetic_Assignment_Expression& node = (Arithmetic_Assignment_Expression&)ast_node;
                stringify(out, *node.lhs);
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
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::logic_or_expr: {
                Logic_Or_Expr& node = (Logic_Or_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" || ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::logic_xor_expr: {
                Logic_Xor_Expr& node = (Logic_Xor_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" ^^ ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::logic_and_expr: {
                Logic_And_Expr& node = (Logic_And_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" && ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::relational_equality_expression: {
                Relational_Equality_Expression& node = (Relational_Equality_Expression&)ast_node;
                stringify(out, *node.lhs);
                out += (node.is_equality ? u8" == " : u8" != ");
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::relational_expression: {
                Relational_Expression& node = (Relational_Expression&)ast_node;
                stringify(out, *node.lhs);
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
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::bit_or_expr: {
                Bit_Or_Expr& node = (Bit_Or_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" | ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::bit_xor_expr: {
                Bit_Xor_Expr& node = (Bit_Xor_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" ^ ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::bit_and_expr: {
                Bit_And_Expr& node = (Bit_And_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" & ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::lshift_expr: {
                LShift_Expr& node = (LShift_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" << ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::rshift_expr: {
                RShift_Expr& node = (RShift_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" >> ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::add_expr: {
                Add_Expr& node = (Add_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" + ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::sub_expr: {
                Sub_Expr& node = (Sub_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" - ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::mul_expr: {
                Mul_Expr& node = (Mul_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" * ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::div_expr: {
                Div_Expr& node = (Div_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" / ";
                stringify(out, *node.rhs);
                return;
            }

            case AST_Node_Type::mod_expr: {
                Mod_Expr& node = (Mod_Expr&)ast_node;
                stringify(out, *node.lhs);
                out += u8" % ";
                stringify(out, *node.rhs);
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
                stringify(out, *node.expression);
                return;
            }

            case AST_Node_Type::prefix_inc_expr: {
                Prefix_Inc_Expr& node = (Prefix_Inc_Expr&)ast_node;
                out += u8"++";
                stringify(out, *node.expression);
                return;
            }

            case AST_Node_Type::prefix_dec_expr: {
                Prefix_Dec_Expr& node = (Prefix_Dec_Expr&)ast_node;
                out += u8"--";
                stringify(out, *node.expression);
                return;
            }

            case AST_Node_Type::argument_list: {
                Argument_List& node = (Argument_List&)ast_node;
                if(node.arguments.size() > 0) {
                    stringify(out, *node.arguments[0]);

                    for(i64 i = 1; i != node.arguments.size(); ++i) {
                        out += u8", ";
                        stringify(out, *node.arguments[i]);
                    }
                }
                return;
            }

            case AST_Node_Type::function_call_expression: {
                Function_Call_Expression& node = (Function_Call_Expression&)ast_node;
                stringify(out, *node.identifier);
                out += u8"(";
                stringify(out, *node.arg_list);
                out += u8")";
                return;
            }

            case AST_Node_Type::member_access_expression: {
                Member_Access_Expression& node = (Member_Access_Expression&)ast_node;
                stringify(out, *node.base);
                out += u8".";
                stringify(out, *node.member);
                return;
            }

            case AST_Node_Type::array_access_expression: {
                Array_Access_Expression& node = (Array_Access_Expression&)ast_node;
                stringify(out, *node.base);
                out += u8"[";
                stringify(out, *node.index);
                out += u8"]";
                return;
            }

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& node = (Identifier_Expression&)ast_node;
                stringify(out, *node.identifier);
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
        }
    }

    void stringify_function_forward_decl(anton::String& out, Function_Declaration& node) {
        stringify(out, *node.return_type);
        out += u8" ";
        stringify(out, *node.name);
        stringify(out, *node.param_list);
        out += u8";\n";
    }

    Expected<anton::Array<GLSL_File>, anton::String> generate_glsl(Declaration_List& node) {
        anton::Array<Declaration*> structs_and_consts;
        anton::Array<Declaration*> functions;
        anton::Array<Declaration*> pass_stages;
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
                } break;
            }
        }

        anton::String common;
        common += "#version 450 core\n";

        for(Declaration* decl: structs_and_consts) {
            stringify(common, *decl);
        }

        for(Declaration* decl: functions) {
            stringify_function_forward_decl(common, (Function_Declaration&)*decl);
        }

        for(Declaration* decl: functions) {
            stringify(common, (Function_Declaration&)*decl);
        }

        anton::Array<GLSL_File> files(anton::reserve, pass_stages.size());
        for(Declaration* decl: pass_stages) {
            // Pass_Stage_Declaration& stage = (Pass_Stage_Declaration&)*decl;
            files.emplace_back(common);
        }

        return {expected_value, anton::move(files)};
    }
} // namespace vush
