#pragma once

#include <anton/array.hpp>
#include <owning_ptr.hpp>

namespace vush {
    enum struct AST_Node_Type;
    enum struct Stage_Type;
    struct Source_Info;
    struct AST_Node;
    struct Import_Decl;
    struct Declaration;
    struct Statement;
    struct Expression;
    struct Identifier;
    struct Type;
    struct Function_Param;
    struct Ordinary_Function_Param;
    struct Sourced_Function_Param;
    struct Vertex_Input_Param;
    struct Function_Declaration;
    struct Pass_Stage_Declaration;
    struct String_Literal;
    struct Integer_Literal;

    using Declaration_List = anton::Array<Owning_Ptr<Declaration>>;
    using Statement_List = anton::Array<Owning_Ptr<Statement>>;
    using Parameter_List = anton::Array<Owning_Ptr<Function_Param>>;
} // namespace vush
