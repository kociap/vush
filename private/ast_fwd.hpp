#pragma once

#include <anton/array.hpp>
#include <owning_ptr.hpp>

namespace vush {
    struct Source_Info;
    enum struct AST_Node_Type;
    struct AST_Node;
    struct Import_Decl;
    struct Declaration;
    struct Statement;
    enum struct Stage_Type;

    using Declaration_List = anton::Array<Owning_Ptr<Declaration>>;
    using Statement_List = anton::Array<Owning_Ptr<Statement>>;
} // namespace vush
