#pragma once

#include <anton/array.hpp>
#include <anton/slice.hpp>

#include <vush/types.hpp>

namespace vush {
    enum struct Stage_Kind : u8;
    struct Source_Info;

    namespace ast {
        enum struct Node_Kind : u8;

        struct Node;

        struct Identifier;

        struct Type;
        struct Type_Builtin;
        struct Type_User_Defined;
        struct Type_Array;

        struct Attribute;
        struct Variable;

        struct Func_Parameter;
        struct Struct_Member;

        struct Decl_Struct;
        struct Decl_Function;
        struct Decl_Overloaded_Function;
        struct Decl_Stage_Function;

        struct Initializer;
        struct Named_Initializer;
        struct Indexed_Initialzier;
        struct Basic_Initializer;

        struct Expr;
        struct Expr_If;
        struct Expr_Identifier;
        struct Expr_Binary;
        struct Expr_Prefix;
        struct Expr_Init;
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

        struct Stmt_Block;
        struct Stmt_If;
        struct Stmt_Switch;
        struct Stmt_Loop;
        struct Stmt_Return;
        struct Stmt_Break;
        struct Stmt_Continue;
        struct Stmt_Discard;
        struct Stmt_Expression;

        using Node_List = anton::Slice<Node const* const>;
        using Attr_List = anton::Slice<Attribute const* const>;
        using Expr_List = anton::Slice<Expr const* const>;
        using Func_Parameter_List = anton::Slice<Func_Parameter const* const>;
        using Initializer_List = anton::Slice<Initializer const* const>;
        using Member_List = anton::Slice<Struct_Member const* const>;
    } // namespace ast
} // namespace vush
