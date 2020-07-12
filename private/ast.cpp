#include <ast.hpp>

namespace vush {
    bool is_opaque_type(Type const& type) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type ||
                         type.node_type == AST_Node_Type::array_type,
                     u8"unknown ast node type");
        if(type.node_type == AST_Node_Type::builtin_type) {
            Builtin_Type const& t = static_cast<Builtin_Type const&>(type);
            return is_opaque_type(t.type);
        } else if(type.node_type == AST_Node_Type::user_defined_type) {
            return false;
        } else {
            Array_Type const& t = static_cast<Array_Type const&>(type);
            return is_opaque_type(*t.base);
        }
    }

    anton::String stringify_type(Type const& type) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type ||
                         type.node_type == AST_Node_Type::array_type,
                     u8"unknown ast node type");
        if(type.node_type == AST_Node_Type::builtin_type) {
            Builtin_Type const& t = static_cast<Builtin_Type const&>(type);
            anton::String_View sv = stringify(t.type);
            return anton::String(sv);
        } else if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& t = static_cast<User_Defined_Type const&>(type);
            return t.name;
        } else {
            Array_Type const& t = static_cast<Array_Type const&>(type);
            anton::String str = stringify_type(*t.base);
            str += u8"[";
            if(t.size) {
                str += t.size->value;
            }
            str += u8"]";
            return str;
        }
    }
} // namespace vush
