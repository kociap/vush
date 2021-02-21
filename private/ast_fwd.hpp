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
    struct String_Literal;
    struct Integer_Literal;

    using Declaration_List = anton::Array<Owning_Ptr<Declaration>>;
    using Statement_List = anton::Array<Owning_Ptr<Statement>>;
} // namespace vush
