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

    struct Hierarchy_Printer final: public AST_Visitor {
    public:
        Hierarchy_Printer(std::ostream& ostream): stream(ostream), indent{0} {}

        virtual void visit(Identifier& node) override {
            stream << indent << "Identifier: '" << node.identifier << "'\n";
        }

        virtual void visit(Type& node) override {
            stream << indent << "Type: '" << node.type << "'\n";
        }

        virtual void visit(Declaration_List& node) override {
            for(auto& declaration: node.declarations) {
                declaration->visit(*this);
            }
        }

        virtual void visit(Declaration_If& node) override {
            stream << indent << "Declaration_If:\n";
            indent += 1;
            node.condition->visit(*this);
            node.true_declarations->visit(*this);
            if(node.false_declarations) {
                node.false_declarations->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Import_Decl& node) override {
            stream << indent << "Import_Decl: '" << node.path << "'\n";
        }

        virtual void visit(Variable_Declaration& node) override {
            stream << indent << "Variable_Declaration:\n";
            indent += 1;
            node.type->visit(*this);
            node.identifier->visit(*this);
            if(node.initializer) {
                node.initializer->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Constant_Declaration& node) override {
            stream << indent << "Constant_Declaration:\n";
            indent += 1;
            node.type->visit(*this);
            node.identifier->visit(*this);
            if(node.initializer) {
                node.initializer->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Struct_Decl& node) override {
            stream << indent << "Struct_Decl:\n";
            indent += 1;
            node.name->visit(*this);
            stream << indent << "Member Variables:\n";
            indent += 1;
            for(auto& member: node.members) {
                member->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Function_Body& node) override {
            stream << indent << "Function_Body:\n";
            if(node.statement_list) {
                indent += 1;
                node.statement_list->visit(*this);
                indent -= 1;
            }
        }

        virtual void visit(Function_Param_List& node) override {
            stream << indent << "Function_Param_List:\n";
            indent += 1;
            for(Owning_Ptr<Function_Param> const& param: node.params) {
                param->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Function_Param_If& node) override {
            stream << indent << "Function_Param_If:\n";
            indent += 1;
            node.condition->visit(*this);
            node.true_param->visit(*this);
            if(node.false_param) {
                node.false_param->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Ordinary_Function_Param& node) override {
            stream << indent << "Ordinary_Function_Param:\n";
            indent += 1;
            node.identifier->visit(*this);
            node.type->visit(*this);
            indent -= 1;
        }

        virtual void visit(Function_Declaration& node) override {
            stream << indent << "Function Declaration:\n";
            indent += 2;
            stream << (indent - 1) << "Name:\n";
            node.name->visit(*this);
            stream << (indent - 1) << "Parameter List:\n";
            node.param_list->visit(*this);
            stream << (indent - 1) << "Return_Type:\n";
            node.return_type->visit(*this);
            stream << (indent - 1) << "Body:\n";
            node.body->visit(*this);
            indent -= 2;
        }

        virtual void visit(Sourced_Function_Param& node) override {
            stream << indent << "Sourced_Function_Param:\n";
            indent += 2;
            stream << (indent - 1) << "Name:\n";
            node.identifier->visit(*this);
            stream << (indent - 1) << "Type:\n";
            node.type->visit(*this);
            stream << (indent - 1) << "Source:\n";
            node.source->visit(*this);
            indent -= 2;
        }

        virtual void visit(Pass_Stage_Declaration& node) override {
            stream << indent << "Pass_Stage_Declaration:\n";
            indent += 2;
            stream << (indent - 1) << "Pass:\n";
            node.pass->visit(*this);
            stream << (indent - 1) << "Name:\n";
            node.name->visit(*this);
            stream << (indent - 1) << "Parameter List:\n";
            node.param_list->visit(*this);
            stream << (indent - 1) << "Return_Type:\n";
            node.return_type->visit(*this);
            stream << (indent - 1) << "Body:\n";
            node.body->visit(*this);
            indent -= 2;
        }

        virtual void visit(Expression_If& node) override {
            stream << indent << "Expression_If:\n";
            indent += 1;
            node.condition->visit(*this);
            node.true_expr->visit(*this);
            if(node.false_expr) {
                node.false_expr->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Identifier_Expression& node) override {
            stream << indent << "Identifier_Expression:\n";
            indent += 1;
            node.identifier->visit(*this);
            indent -= 1;
        }

        virtual void visit(Assignment_Expression& node) override {
            stream << indent << "Assignment_Expression:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Arithmetic_Assignment_Expression& node) override {
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
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Logic_Or_Expr& node) override {
            stream << indent << "Logic_Or_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Logic_Xor_Expr& node) override {
            stream << indent << "Logic_Xor_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Logic_And_Expr& node) override {
            stream << indent << "Logic_And_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Relational_Equality_Expression& node) override {
            stream << indent << "Relational_Expression (" << (node.is_equality ? "==" : "!=") << "):\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Relational_Expression& node) override {
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
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Bit_Or_Expr& node) override {
            stream << indent << "Bit_Or_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Bit_Xor_Expr& node) override {
            stream << indent << "Bit_Xor_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Bit_And_Expr& node) override {
            stream << indent << "Bit_And_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(LShift_Expr& node) override {
            stream << indent << "LShift_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(RShift_Expr& node) override {
            stream << indent << "RShift_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Add_Expr& node) override {
            stream << indent << "Add_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Sub_Expr& node) override {
            stream << indent << "Sub_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Mul_Expr& node) override {
            stream << indent << "Mul_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Div_Expr& node) override {
            stream << indent << "Div_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Mod_Expr& node) override {
            stream << indent << "Mod_Expr:\n";
            indent += 1;
            node.lhs->visit(*this);
            node.rhs->visit(*this);
            indent -= 1;
        }

        virtual void visit(Unary_Expression& node) override {
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
            node.expression->visit(*this);
            indent -= 1;
        }

        virtual void visit(Prefix_Inc_Dec_Expression& node) override {
            stream << indent << "Prefix_Inc_Dec_Expression (" << (node.is_inc ? "++" : "--") << "):\n";
            indent += 1;
            node.expression->visit(*this);
            indent -= 1;
        }

        virtual void visit(Argument_List& node) override {
            stream << indent << "Argument_List:\n";
            indent += 1;
            for(auto& argument: node.arguments) {
                argument->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Function_Call_Expression& node) override {
            stream << indent << "Function_Call_Expression:\n";
            indent += 1;
            node.identifier->visit(*this);
            node.arg_list->visit(*this);
            indent -= 1;
        }

        virtual void visit(Member_Access_Expression& node) override {
            stream << indent << "Member_Access_Expression:\n";
            indent += 1;
            node.base->visit(*this);
            node.member->visit(*this);
            indent -= 1;
        }

        virtual void visit(Array_Access_Expression& node) override {
            stream << indent << "Array_Access_Expression:\n";
            indent += 1;
            node.base->visit(*this);
            node.index->visit(*this);
            indent -= 1;
        }

        virtual void visit(Postfix_Inc_Dec_Expression& node) override {
            stream << indent << "Postfix_Inc_Dec_Expression (" << (node.is_inc ? u8"++" : u8"--") << "):\n";
            indent += 1;
            node.base->visit(*this);
            indent -= 1;
        }

        virtual void visit(String_Literal& node) override {
            stream << indent << "String_Literal: \"" << node.value << "\"\n";
        }

        virtual void visit(Bool_Literal& node) override {
            stream << indent << "Bool_Literal: " << (node.value ? "true" : "false") << "\n";
        }

        virtual void visit(Integer_Literal& node) override {
            stream << indent << "Integer_Literal: " << node.value << "\n";
        }

        virtual void visit(Float_Literal& node) override {
            stream << indent << "Float_Literal: " << node.value << "\n";
        }

        virtual void visit(Statement_List& node) override {
            for(auto& statement: node.statements) {
                statement->visit(*this);
            }
        }

        virtual void visit(Block_Statement& node) override {
            stream << indent << "Block_Statement:\n";
            indent += 1;
            node.statements->visit(*this);
            indent -= 1;
        }

        virtual void visit(If_Statement& node) override {
            stream << indent << "If_Statement:\n";
            indent += 1;
            node.condition->visit(*this);
            node.true_statement->visit(*this);
            if(node.false_statement) {
                node.false_statement->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(Case_Statement& node) override {
            stream << indent << "Case_Statement:\n";
            indent += 1;
            node.condition->visit(*this);
            node.statements->visit(*this);
            indent -= 1;
        }

        virtual void visit(Default_Case_Statement& node) override {
            stream << indent << "Default_Case_Statement:\n";
            indent += 1;
            node.statements->visit(*this);
            indent -= 1;
        }

        virtual void visit(Switch_Statement& node) override {
            stream << indent << "Switch_Statement:\n";
            indent += 1;
            for(auto& statement: node.cases) {
                statement->visit(*this);
            }
            indent -= 1;
        }

        virtual void visit(For_Statement& node) override {
            stream << indent << "For_Statement:\n";
            indent += 1;
            node.declaration->visit(*this);
            node.condition->visit(*this);
            node.post_expression->visit(*this);
            node.block->visit(*this);
            indent -= 1;
        }

        virtual void visit(While_Statement& node) override {
            stream << indent << "While_Statement:\n";
            indent += 1;
            node.condition->visit(*this);
            node.block->visit(*this);
            indent -= 1;
        }

        virtual void visit(Do_While_Statement& node) override {
            stream << indent << "Do_While_Statement:\n";
            indent += 1;
            node.condition->visit(*this);
            node.block->visit(*this);
            indent -= 1;
        }

        virtual void visit(Return_Statement& node) override {
            stream << indent << "Return_Statement:\n";
            indent += 1;
            node.return_expr->visit(*this);
            indent -= 1;
        }

        virtual void visit(Break_Statement&) override {
            stream << indent << "Break_Statement\n";
        }

        virtual void visit(Continue_Statement&) override {
            stream << indent << "Continue_Statement\n";
        }

        virtual void visit(Declaration_Statement& node) override {
            stream << indent << "Declaration_Statement (Variable Declaration):\n";
            indent += 1;
            node.declaration->visit(*this);
            indent -= 1;
        }

        virtual void visit(Expression_Statement& node) override {
            stream << indent << "Expression_Statement:\n";
            indent += 1;
            node.expr->visit(*this);
            indent -= 1;
        }

    private:
        std::ostream& stream;
        Indent indent;
    };
} // namespace vush
