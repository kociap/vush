#pragma once

#include <ast.hpp>

#include <ostream>

namespace vush {
    struct Indent {
        i64 indent_count = 0;
    };

    Indent& operator+=(Indent& lhs, i64 rhs) {
        lhs.indent_count += rhs;
        return lhs;
    }

    Indent& operator-=(Indent& lhs, i64 rhs) {
        lhs.indent_count -= rhs;
        return lhs;
    }

    Indent operator+(Indent const& lhs, i64 rhs) {
        return Indent{lhs.indent_count + rhs};
    }

    Indent operator-(Indent const& lhs, i64 rhs) {
        return Indent{lhs.indent_count - rhs};
    }

    std::ostream& operator<<(std::ostream& stream, Indent indent) {
        for(i64 i = 0; i < indent.indent_count; ++i) {
            stream << "  ";
        }
        return stream;
    }

    struct Hierarchy_Printer {
    public:
        Hierarchy_Printer(std::ostream& ostream): stream(ostream), indent{0} {}

        void print_hierarchy(Syntax_Tree_Node& ast_node) {
            switch(ast_node.node_type) {
                case AST_Node_Type::identifier: {
                    Identifier& node = (Identifier&)ast_node;
                    stream << indent << "Identifier: '" << node.identifier << "'\n";
                    return;
                }

                case AST_Node_Type::builtin_type: {
                    Builtin_Type& node = (Builtin_Type&)ast_node;
                    stream << indent << "Builtin_Type: '" << stringify(node.type) << "'\n";
                    return;
                }

                case AST_Node_Type::user_defined_type: {
                    User_Defined_Type& node = (User_Defined_Type&)ast_node;
                    stream << indent << "User_Defined_Type: '" << node.name << "'\n";
                    return;
                }

                case AST_Node_Type::declaration_list: {
                    Declaration_List& node = (Declaration_List&)ast_node;
                    for(auto& declaration: node.declarations) {
                        print_hierarchy(*declaration);
                    }
                    return;
                }

                case AST_Node_Type::declaration_if: {
                    Declaration_If& node = (Declaration_If&)ast_node;
                    stream << indent << "Declaration_If:\n";
                    indent += 1;
                    print_hierarchy(*node.condition);
                    print_hierarchy(*node.true_declarations);
                    if(node.false_declarations) {
                        print_hierarchy(*node.false_declarations);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::import_decl: {
                    Import_Decl& node = (Import_Decl&)ast_node;
                    stream << indent << "Import_Decl: '" << node.path << "'\n";
                }

                case AST_Node_Type::variable_declaration: {
                    Variable_Declaration& node = (Variable_Declaration&)ast_node;
                    stream << indent << "Variable_Declaration:\n";
                    indent += 1;
                    print_hierarchy(*node.type);
                    print_hierarchy(*node.identifier);
                    if(node.initializer) {
                        print_hierarchy(*node.initializer);
                    }
                    indent -= 1;
                    return;
                }
                case AST_Node_Type::constant_declaration: {
                    Constant_Declaration& node = (Constant_Declaration&)ast_node;
                    stream << indent << "Constant_Declaration:\n";
                    indent += 1;
                    print_hierarchy(*node.type);
                    print_hierarchy(*node.identifier);
                    if(node.initializer) {
                        print_hierarchy(*node.initializer);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::struct_decl: {
                    Struct_Decl& node = (Struct_Decl&)ast_node;
                    stream << indent << "Struct_Decl:\n";
                    indent += 1;
                    print_hierarchy(*node.name);
                    stream << indent << "Member Variables:\n";
                    indent += 1;
                    for(auto& member: node.members) {
                        print_hierarchy(*member);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::function_body: {
                    Function_Body& node = (Function_Body&)ast_node;
                    stream << indent << "Function_Body:\n";
                    if(node.statement_list) {
                        indent += 1;
                        print_hierarchy(*node.statement_list);
                        indent -= 1;
                    }
                    return;
                }

                case AST_Node_Type::function_param_list: {
                    Function_Param_List& node = (Function_Param_List&)ast_node;
                    stream << indent << "Function_Param_List:\n";
                    indent += 1;
                    for(Owning_Ptr<Function_Param> const& param: node.params) {
                        print_hierarchy(*param);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::function_param_if: {
                    Function_Param_If& node = (Function_Param_If&)ast_node;
                    stream << indent << "Function_Param_If:\n";
                    indent += 1;
                    print_hierarchy(*node.condition);
                    print_hierarchy(*node.true_param);
                    if(node.false_param) {
                        print_hierarchy(*node.false_param);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::ordinary_function_param: {
                    Ordinary_Function_Param& node = (Ordinary_Function_Param&)ast_node;
                    stream << indent << "Ordinary_Function_Param:\n";
                    indent += 1;
                    print_hierarchy(*node.identifier);
                    print_hierarchy(*node.type);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::function_declaration: {
                    Function_Declaration& node = (Function_Declaration&)ast_node;
                    stream << indent << "Function Declaration:\n";
                    indent += 2;
                    stream << (indent - 1) << "Name:\n";
                    print_hierarchy(*node.name);
                    stream << (indent - 1) << "Parameter List:\n";
                    print_hierarchy(*node.param_list);
                    stream << (indent - 1) << "Return_Type:\n";
                    print_hierarchy(*node.return_type);
                    stream << (indent - 1) << "Body:\n";
                    print_hierarchy(*node.body);
                    indent -= 2;
                    return;
                }

                case AST_Node_Type::sourced_function_param: {
                    Sourced_Function_Param& node = (Sourced_Function_Param&)ast_node;
                    stream << indent << "Sourced_Function_Param:\n";
                    indent += 2;
                    stream << (indent - 1) << "Name:\n";
                    print_hierarchy(*node.identifier);
                    stream << (indent - 1) << "Type:\n";
                    print_hierarchy(*node.type);
                    stream << (indent - 1) << "Source:\n";
                    print_hierarchy(*node.source);
                    indent -= 2;
                    return;
                }

                case AST_Node_Type::pass_stage_declaration: {
                    Pass_Stage_Declaration& node = (Pass_Stage_Declaration&)ast_node;
                    stream << indent << "Pass_Stage_Declaration:\n";
                    indent += 2;
                    stream << (indent - 1) << "Pass:\n";
                    print_hierarchy(*node.pass);
                    stream << (indent - 1) << "Name:\n";
                    print_hierarchy(*node.name);
                    stream << (indent - 1) << "Parameter List:\n";
                    print_hierarchy(*node.param_list);
                    stream << (indent - 1) << "Return_Type:\n";
                    print_hierarchy(*node.return_type);
                    stream << (indent - 1) << "Body:\n";
                    print_hierarchy(*node.body);
                    indent -= 2;
                    return;
                }

                case AST_Node_Type::expression_if: {
                    Expression_If& node = (Expression_If&)ast_node;
                    stream << indent << "Expression_If:\n";
                    indent += 1;
                    print_hierarchy(*node.condition);
                    print_hierarchy(*node.true_expr);
                    if(node.false_expr) {
                        print_hierarchy(*node.false_expr);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::identifier_expression: {
                    Identifier_Expression& node = (Identifier_Expression&)ast_node;
                    stream << indent << "Identifier_Expression:\n";
                    indent += 1;
                    print_hierarchy(*node.identifier);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::assignment_expression: {
                    Assignment_Expression& node = (Assignment_Expression&)ast_node;
                    stream << indent << "Assignment_Expression:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::arithmetic_assignment_expression: {
                    Arithmetic_Assignment_Expression& node = (Arithmetic_Assignment_Expression&)ast_node;
                    char const* repr = nullptr;
                    switch(node.type) {
                        case Arithmetic_Assignment_Type::plus:
                            repr = u8"+=";
                            break;
                        case Arithmetic_Assignment_Type::minus:
                            repr = u8"-=";
                            break;
                        case Arithmetic_Assignment_Type::multiply:
                            repr = u8"*=";
                            break;
                        case Arithmetic_Assignment_Type::divide:
                            repr = u8"/=";
                            break;
                        case Arithmetic_Assignment_Type::remainder:
                            repr = u8"%=";
                            break;
                        case Arithmetic_Assignment_Type::lshift:
                            repr = u8"<<=";
                            break;
                        case Arithmetic_Assignment_Type::rshift:
                            repr = u8">>=";
                            break;
                        case Arithmetic_Assignment_Type::bit_and:
                            repr = u8"&=";
                            break;
                        case Arithmetic_Assignment_Type::bit_or:
                            repr = u8"|=";
                            break;
                        case Arithmetic_Assignment_Type::bit_xor:
                            repr = u8"^=";
                            break;
                    }

                    stream << indent << "Arithmetic_Assignment_Expression (" << repr << "):\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::logic_or_expr: {
                    Logic_Or_Expr& node = (Logic_Or_Expr&)ast_node;
                    stream << indent << "Logic_Or_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::logic_xor_expr: {
                    Logic_Xor_Expr& node = (Logic_Xor_Expr&)ast_node;
                    stream << indent << "Logic_Xor_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::logic_and_expr: {
                    Logic_And_Expr& node = (Logic_And_Expr&)ast_node;
                    stream << indent << "Logic_And_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::relational_equality_expression: {
                    Relational_Equality_Expression& node = (Relational_Equality_Expression&)ast_node;
                    stream << indent << "Relational_Expression (" << (node.is_equality ? "==" : "!=") << "):\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::relational_expression: {
                    Relational_Expression& node = (Relational_Expression&)ast_node;
                    char const* repr = nullptr;
                    if(node.type == Relational_Type::greater_than) {
                        repr = u8">";
                    } else if(node.type == Relational_Type::greater_equal) {
                        repr = u8">=";
                    } else if(node.type == Relational_Type::less_than) {
                        repr = u8"<";
                    } else {
                        repr = u8"<=";
                    }

                    stream << indent << "Relational_Expression (" << repr << "):\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::bit_or_expr: {
                    Bit_Or_Expr& node = (Bit_Or_Expr&)ast_node;
                    stream << indent << "Bit_Or_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::bit_xor_expr: {
                    Bit_Xor_Expr& node = (Bit_Xor_Expr&)ast_node;
                    stream << indent << "Bit_Xor_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::bit_and_expr: {
                    Bit_And_Expr& node = (Bit_And_Expr&)ast_node;
                    stream << indent << "Bit_And_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::lshift_expr: {
                    LShift_Expr& node = (LShift_Expr&)ast_node;
                    stream << indent << "LShift_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::rshift_expr: {
                    RShift_Expr& node = (RShift_Expr&)ast_node;
                    stream << indent << "RShift_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::add_expr: {
                    Add_Expr& node = (Add_Expr&)ast_node;
                    stream << indent << "Add_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::sub_expr: {
                    Sub_Expr& node = (Sub_Expr&)ast_node;
                    stream << indent << "Sub_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::mul_expr: {
                    Mul_Expr& node = (Mul_Expr&)ast_node;
                    stream << indent << "Mul_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::div_expr: {
                    Div_Expr& node = (Div_Expr&)ast_node;
                    stream << indent << "Div_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::mod_expr: {
                    Mod_Expr& node = (Mod_Expr&)ast_node;
                    stream << indent << "Mod_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.lhs);
                    print_hierarchy(*node.rhs);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::unary_expression: {
                    Unary_Expression& node = (Unary_Expression&)ast_node;
                    char const* repr = nullptr;
                    switch(node.type) {
                        case Unary_Type::plus:
                            repr = u8"+";
                            break;
                        case Unary_Type::minus:
                            repr = u8"-";
                            break;
                        case Unary_Type::bit_not:
                            repr = u8"~";
                            break;
                        case Unary_Type::logic_not:
                            repr = u8"!";
                            break;
                    }

                    stream << indent << "Unary_Expression (" << repr << "):\n";
                    indent += 1;
                    print_hierarchy(*node.expression);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::prefix_inc_expr: {
                    Prefix_Inc_Expr& node = (Prefix_Inc_Expr&)ast_node;
                    stream << indent << "Prefix_Inc_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.expression);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::prefix_dec_expr: {
                    Prefix_Dec_Expr& node = (Prefix_Dec_Expr&)ast_node;
                    stream << indent << "Prefix_Dec_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.expression);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::argument_list: {
                    Argument_List& node = (Argument_List&)ast_node;
                    stream << indent << "Argument_List:\n";
                    indent += 1;
                    for(auto& argument: node.arguments) {
                        print_hierarchy(*argument);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::function_call_expression: {
                    Function_Call_Expression& node = (Function_Call_Expression&)ast_node;
                    stream << indent << "Function_Call_Expression:\n";
                    indent += 1;
                    print_hierarchy(*node.identifier);
                    print_hierarchy(*node.arg_list);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::member_access_expression: {
                    Member_Access_Expression& node = (Member_Access_Expression&)ast_node;
                    stream << indent << "Member_Access_Expression:\n";
                    indent += 1;
                    print_hierarchy(*node.base);
                    print_hierarchy(*node.member);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::array_access_expression: {
                    Array_Access_Expression& node = (Array_Access_Expression&)ast_node;
                    stream << indent << "Array_Access_Expression:\n";
                    indent += 1;
                    print_hierarchy(*node.base);
                    print_hierarchy(*node.index);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::postfix_inc_expr: {
                    Postfix_Inc_Expr& node = (Postfix_Inc_Expr&)ast_node;
                    stream << indent << "Postfix_Inc_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.base);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::postfix_dec_expr: {
                    Postfix_Dec_Expr& node = (Postfix_Dec_Expr&)ast_node;
                    stream << indent << "Postfix_Dec_Expr:\n";
                    indent += 1;
                    print_hierarchy(*node.base);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::string_literal: {
                    String_Literal& node = (String_Literal&)ast_node;
                    stream << indent << "String_Literal: \"" << node.value << "\"\n";
                    return;
                }

                case AST_Node_Type::bool_literal: {
                    Bool_Literal& node = (Bool_Literal&)ast_node;
                    stream << indent << "Bool_Literal: " << (node.value ? "true" : "false") << "\n";
                    return;
                }

                case AST_Node_Type::integer_literal: {
                    Integer_Literal& node = (Integer_Literal&)ast_node;
                    stream << indent << "Integer_Literal: " << node.value << "\n";
                    return;
                }

                case AST_Node_Type::float_literal: {
                    Float_Literal& node = (Float_Literal&)ast_node;
                    stream << indent << "Float_Literal: " << node.value << "\n";
                    return;
                }

                case AST_Node_Type::statement_list: {
                    Statement_List& node = (Statement_List&)ast_node;
                    for(auto& statement: node.statements) {
                        print_hierarchy(*statement);
                    }
                    return;
                }

                case AST_Node_Type::block_statement: {
                    Block_Statement& node = (Block_Statement&)ast_node;
                    stream << indent << "Block_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.statements);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::if_statement: {
                    If_Statement& node = (If_Statement&)ast_node;
                    stream << indent << "If_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.condition);
                    print_hierarchy(*node.true_statement);
                    if(node.false_statement) {
                        print_hierarchy(*node.false_statement);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::case_statement: {
                    Case_Statement& node = (Case_Statement&)ast_node;
                    stream << indent << "Case_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.condition);
                    print_hierarchy(*node.statements);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::default_case_statement: {
                    Default_Case_Statement& node = (Default_Case_Statement&)ast_node;
                    stream << indent << "Default_Case_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.statements);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::switch_statement: {
                    Switch_Statement& node = (Switch_Statement&)ast_node;
                    stream << indent << "Switch_Statement:\n";
                    indent += 1;
                    for(auto& statement: node.cases) {
                        print_hierarchy(*statement);
                    }
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::for_statement: {
                    For_Statement& node = (For_Statement&)ast_node;
                    stream << indent << "For_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.declaration);
                    print_hierarchy(*node.condition);
                    print_hierarchy(*node.post_expression);
                    print_hierarchy(*node.block);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::while_statement: {
                    While_Statement& node = (While_Statement&)ast_node;
                    stream << indent << "While_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.condition);
                    print_hierarchy(*node.block);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::do_while_statement: {
                    Do_While_Statement& node = (Do_While_Statement&)ast_node;
                    stream << indent << "Do_While_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.condition);
                    print_hierarchy(*node.block);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::return_statement: {
                    Return_Statement& node = (Return_Statement&)ast_node;
                    stream << indent << "Return_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.return_expr);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::break_statement: {
                    stream << indent << "Break_Statement\n";
                    return;
                }

                case AST_Node_Type::continue_statement: {
                    stream << indent << "Continue_Statement\n";
                    return;
                }

                case AST_Node_Type::declaration_statement: {
                    Declaration_Statement& node = (Declaration_Statement&)ast_node;
                    stream << indent << "Declaration_Statement (Variable Declaration):\n";
                    indent += 1;
                    print_hierarchy(*node.declaration);
                    indent -= 1;
                    return;
                }

                case AST_Node_Type::expression_statement: {
                    Expression_Statement& node = (Expression_Statement&)ast_node;
                    stream << indent << "Expression_Statement:\n";
                    indent += 1;
                    print_hierarchy(*node.expr);
                    indent -= 1;
                    return;
                }
            }
        }

    private:
        std::ostream& stream;
        Indent indent;
    };
} // namespace vush
