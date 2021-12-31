#pragma once

#include <anton/array.hpp>
#include <owning_ptr.hpp>

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
    struct Elvis_Expression;
    struct Binary_Expression;
    struct Unary_Expression;
    struct Prefix_Increment_Expression;
    struct Prefix_Decrement_Expression;
    struct Function_Call_Expression;
    struct Member_Access_Expression;
    struct Array_Access_Expression;
    struct Postfix_Increment_Expression;
    struct Postfix_Decrement_Expression;
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

    using Declaration_List = anton::Array<Owning_Ptr<Declaration>>;
    using Statement_List = anton::Array<Owning_Ptr<Statement>>;
    using Parameter_List = anton::Array<Owning_Ptr<Function_Parameter_Node>>;
    using Attribute_List = anton::Array<Owning_Ptr<Attribute>>;
    using Expression_List = anton::Array<Owning_Ptr<Expression>>;
} // namespace vush
