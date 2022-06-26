#pragma once

#include <ast_fwd.hpp>

namespace vush {
    struct Recursive_AST_Matcher {
        virtual ~Recursive_AST_Matcher() = default;

        struct Match_Result {
            bool matched = false;
            // Do not traverse the child nodes and do not call the matching functions on them.
            bool ignore_children = false;
            // End the matching process immediately.
            bool break_matching = false;
        };

        [[nodiscard]] virtual Match_Result match(Identifier const& node);
        [[nodiscard]] virtual Match_Result match(Builtin_Type const& node);
        [[nodiscard]] virtual Match_Result match(User_Defined_Type const& node);
        [[nodiscard]] virtual Match_Result match(Array_Type const& node);
        [[nodiscard]] virtual Match_Result match(Declaration_If const& node);
        [[nodiscard]] virtual Match_Result match(Import_Declaration const& node);
        [[nodiscard]] virtual Match_Result match(Variable_Declaration const& node);
        [[nodiscard]] virtual Match_Result match(Constant_Declaration const& node);
        [[nodiscard]] virtual Match_Result match(Struct_Member const& node);
        [[nodiscard]] virtual Match_Result match(Struct_Declaration const& node);
        [[nodiscard]] virtual Match_Result match(Settings_Declaration const& node);
        [[nodiscard]] virtual Match_Result match(Workgroup_Attribute const& node);
        [[nodiscard]] virtual Match_Result match(Function_Param_If const& node);
        [[nodiscard]] virtual Match_Result match(Image_Layout_Qualifier const& node);
        [[nodiscard]] virtual Match_Result match(Function_Parameter const& node);
        [[nodiscard]] virtual Match_Result match(Function_Declaration const& node);
        [[nodiscard]] virtual Match_Result match(Overloaded_Function_Declaration const& node);
        [[nodiscard]] virtual Match_Result match(Pass_Stage_Declaration const& node);
        [[nodiscard]] virtual Match_Result match(Expression_If const& node);
        [[nodiscard]] virtual Match_Result match(Identifier_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Assignment_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Arithmetic_Assignment_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Binary_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Unary_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Prefix_Increment_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Prefix_Decrement_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Function_Call_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Member_Access_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Array_Access_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Postfix_Increment_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Postfix_Decrement_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Parenthesised_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Reinterpret_Expression const& node);
        [[nodiscard]] virtual Match_Result match(Default_Expression const& node);
        [[nodiscard]] virtual Match_Result match(String_Literal const& node);
        [[nodiscard]] virtual Match_Result match(Bool_Literal const& node);
        [[nodiscard]] virtual Match_Result match(Integer_Literal const& node);
        [[nodiscard]] virtual Match_Result match(Float_Literal const& node);
        [[nodiscard]] virtual Match_Result match(Block_Statement const& node);
        [[nodiscard]] virtual Match_Result match(If_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Case_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Switch_Statement const& node);
        [[nodiscard]] virtual Match_Result match(For_Statement const& node);
        [[nodiscard]] virtual Match_Result match(While_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Do_While_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Return_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Break_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Continue_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Discard_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Declaration_Statement const& node);
        [[nodiscard]] virtual Match_Result match(Expression_Statement const& node);
    };

    struct AST_Action {
        virtual ~AST_Action() = default;

        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Identifier> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Builtin_Type> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<User_Defined_Type> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Array_Type> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Declaration_If> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Import_Declaration> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Variable_Declaration> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Constant_Declaration> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Struct_Member> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Struct_Declaration> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Settings_Declaration> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Workgroup_Attribute> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Function_Param_If> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Image_Layout_Qualifier> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Function_Parameter> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Function_Declaration> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Overloaded_Function_Declaration> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Pass_Stage_Declaration> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Expression_If> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Identifier_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Assignment_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Arithmetic_Assignment_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Binary_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Unary_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Prefix_Increment_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Prefix_Decrement_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Function_Call_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Member_Access_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Array_Access_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Postfix_Increment_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Postfix_Decrement_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Parenthesised_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Reinterpret_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Default_Expression> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<String_Literal> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Bool_Literal> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Integer_Literal> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Float_Literal> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Block_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<If_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Case_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Switch_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<For_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<While_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Do_While_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Return_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Break_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Continue_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Discard_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Declaration_Statement> node);
        virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Expression_Statement> node);
    };

    void traverse_node(Recursive_AST_Matcher& matchor, AST_Action& action, Owning_Ptr<AST_Node>& node);
} // namespace vush
