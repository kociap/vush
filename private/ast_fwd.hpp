#pragma once

#include <anton/array.hpp>
#include <anton/slice.hpp>

#include <owning_ptr.hpp>
#include <vush/types.hpp>

namespace vush {
    enum struct AST_Node_Type;
    enum struct Stage_Type;
    struct Source_Info;
    struct AST_Node;
    struct Declaration;
    struct Statement;
    struct Expression;
    struct Type;
    struct Attribute;
    struct Function_Parameter_Node;

    struct Identifier;
    struct Builtin_Type;
    struct User_Defined_Type;
    struct Array_Type;
    struct Declaration_If;
    struct Import_Declaration;
    struct Variable_Declaration;
    struct Constant_Declaration;
    struct Struct_Member;
    struct Struct_Declaration;
    struct Settings_Declaration;
    struct Workgroup_Attribute;
    struct Function_Param_If;
    struct Image_Layout_Qualifier;
    struct Function_Parameter;
    struct Function_Declaration;
    struct Overloaded_Function_Declaration;
    struct Pass_Stage_Declaration;
    struct Expression_If;
    struct Identifier_Expression;
    struct Assignment_Expression;
    struct Arithmetic_Assignment_Expression;
    struct Binary_Expression;
    struct Prefix_Expression;
    struct Function_Call_Expression;
    struct Member_Access_Expression;
    struct Array_Access_Expression;
    struct Postfix_Expression;
    struct Parenthesised_Expression;
    struct Reinterpret_Expression;
    struct Default_Expression;
    struct String_Literal;
    struct Bool_Literal;
    struct Integer_Literal;
    struct Float_Literal;
    struct Block_Statement;
    struct If_Statement;
    struct Case_Statement;
    struct Switch_Statement;
    struct For_Statement;
    struct While_Statement;
    struct Do_While_Statement;
    struct Return_Statement;
    struct Break_Statement;
    struct Continue_Statement;
    struct Discard_Statement;
    struct Declaration_Statement;
    struct Expression_Statement;

    using Declaration_List = Array<Owning_Ptr<Declaration>>;
    using Statement_List = Array<Owning_Ptr<Statement>>;
    using Parameter_List = Array<Owning_Ptr<Function_Parameter_Node>>;
    using Attribute_List = Array<Owning_Ptr<Attribute>>;
    using Expression_List = Array<Owning_Ptr<Expression>>;

    namespace ast {
        struct Node;

        struct Identifier;

        struct Type;
        struct Type_Builtin;
        struct Type_User_Defined;
        struct Type_Array;

        struct Attr;
        struct Attr_Workgroup;
        struct Attr_Invariant;
        struct Attr_Flat;
        struct Attr_Smooth;
        struct Attr_Noperspective;

        struct Func_Parameter;
        struct Struct_Member;

        struct Decl;
        struct Decl_Constant;
        struct Decl_Struct;
        struct Decl_Function;
        struct Decl_Overloaded_Function;
        struct Decl_Stage_Function;

        struct Expr;
        struct Expr_If;
        struct Expr_Identifier;
        struct Expr_Binary;
        struct Expr_Prefix;
        struct Expr_Postfix;
        struct Expr_Call;
        struct Expr_Member_Access;
        struct Expr_Array_Access;
        struct Expr_Parentheses;
        struct Expr_Reinterpret;
        struct Expr_Default;
        struct Lt_Bool;
        struct Lt_Integer;
        struct Lt_Float;

        struct Switch_Arm;

        struct Stmt;
        struct Stmt_Block;
        struct Stmt_If;
        struct Stmt_Switch;
        struct Stmt_While;
        struct Stmt_Do_While;
        struct Stmt_Return;
        struct Stmt_Break;
        struct Stmt_Continue;
        struct Stmt_Discard;
        struct Stmt_Variable;
        struct Stmt_Expression;

        using Attr_List = anton::Slice<Attr const* const>;
        using Decl_List = anton::Slice<Decl const* const>;
        using Stmt_List = anton::Slice<Stmt const* const>;
        using Expr_List = anton::Slice<Expr const* const>;
        using Func_Parameter_List = anton::Slice<Func_Parameter const* const>;
    } // namespace ast
} // namespace vush
