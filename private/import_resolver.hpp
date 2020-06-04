#pragma once

#include <ast.hpp>
#include <vector>

namespace vush {
    struct Import_Resolver final: public AST_Visitor {
        std::vector<std::string> imports;

        virtual void visit(Declaration_List& node) {
            for(auto& declaration: node.declarations) {
                declaration->visit(*this);
            }
        }

        virtual void visit(Declaration_If& node) {
            node.true_declarations->visit(*this);
            node.true_declarations->visit(*this);
        }

        virtual void visit(Import_Decl& node) {
            imports.emplace_back(node.path);
        }

        virtual void visit(Identifier&) {}
        virtual void visit(Type&) {}
        virtual void visit(Variable_Declaration&) {}
        virtual void visit(Struct_Decl&) {}
        virtual void visit(Function_Body&) {}
        virtual void visit(Function_Param_List&) {}
        virtual void visit(Function_Param_If&) {}
        virtual void visit(Ordinary_Function_Param&) {}
        virtual void visit(Function_Declaration&) {}
        virtual void visit(Sourced_Function_Param&) {}
        virtual void visit(Pass_Stage_Declaration&) {}
        virtual void visit(Expression_If&) {}
        virtual void visit(Identifier_Expression&) {}
        virtual void visit(Assignment_Expression&) {}
        virtual void visit(Arithmetic_Assignment_Expression&) {}
        virtual void visit(Logic_Or_Expr&) {}
        virtual void visit(Logic_Xor_Expr&) {}
        virtual void visit(Logic_And_Expr&) {}
        virtual void visit(Relational_Equality_Expression&) {}
        virtual void visit(Relational_Expression&) {}
        virtual void visit(Bit_Or_Expr&) {}
        virtual void visit(Bit_Xor_Expr&) {}
        virtual void visit(Bit_And_Expr&) {}
        virtual void visit(LShift_Expr&) {}
        virtual void visit(RShift_Expr&) {}
        virtual void visit(Add_Expr&) {}
        virtual void visit(Sub_Expr&) {}
        virtual void visit(Mul_Expr&) {}
        virtual void visit(Div_Expr&) {}
        virtual void visit(Mod_Expr&) {}
        virtual void visit(Unary_Expression&) {}
        virtual void visit(Prefix_Inc_Dec_Expression&) {}
        virtual void visit(Argument_List&) {}
        virtual void visit(Function_Call_Expression&) {}
        virtual void visit(Member_Access_Expression&) {}
        virtual void visit(Array_Access_Expression&) {}
        virtual void visit(Postfix_Inc_Dec_Expression&) {}
        virtual void visit(String_Literal&) {}
        virtual void visit(Bool_Literal&) {}
        virtual void visit(Integer_Literal&) {}
        virtual void visit(Float_Literal&) {}
        virtual void visit(Statement_List&) {}
        virtual void visit(Block_Statement&) {}
        virtual void visit(If_Statement&) {}
        virtual void visit(Case_Statement&) {}
        virtual void visit(Default_Case_Statement&) {}
        virtual void visit(Switch_Statement&) {}
        virtual void visit(For_Statement&) {}
        virtual void visit(While_Statement&) {}
        virtual void visit(Do_While_Statement&) {}
        virtual void visit(Return_Statement&) {}
        virtual void visit(Break_Statement&) {}
        virtual void visit(Continue_Statement&) {}
        virtual void visit(Declaration_Statement&) {}
        virtual void visit(Expression_Statement&) {}
    };
} // namespace vush
