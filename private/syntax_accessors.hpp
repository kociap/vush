#include <ast2.hpp>

namespace vush {
    [[nodiscard]] static Syntax_Token const& get_type_builtin_value(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::type_builtin, "node is not type_builtin");
        ANTON_ASSERT(node.children.size() > (0), "type_builtin has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "value in type_builtin is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Token const& get_type_user_defined_value(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::type_user_defined, "node is not type_user_defined");
        ANTON_ASSERT(node.children.size() > (0), "type_user_defined has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "value in type_user_defined is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_type_array_base(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::type_array, "node is not type_array");
        ANTON_ASSERT(node.children.size() > (0), "type_array has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "base in type_array is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Node const& get_type_array_size(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::type_array, "node is not type_array");
        ANTON_ASSERT(node.children.size() > (2), "type_array has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "size in type_array is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_attr_workgroup_x(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::attr_workgroup, "node is not attr_workgroup");
        ANTON_ASSERT(node.children.size() > (2), "attr_workgroup has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "x in attr_workgroup is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_attr_workgroup_y(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::attr_workgroup, "node is not attr_workgroup");
        if(node.children.size() > (4)) {
            ANTON_ASSERT(node.children[4].is_left(), "y in attr_workgroup is not Syntax_Node");
            return node.children[4].left();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_attr_workgroup_z(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::attr_workgroup, "node is not attr_workgroup");
        if(node.children.size() > (6)) {
            ANTON_ASSERT(node.children[6].is_left(), "z in attr_workgroup is not Syntax_Node");
            return node.children[6].left();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static Syntax_Node const& get_decl_if_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_if, "node is not decl_if");
        ANTON_ASSERT(node.children.size() > (1), "decl_if has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "condition in decl_if is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_if_then_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_if, "node is not decl_if");
        ANTON_ASSERT(node.children.size() > (2), "decl_if has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "then_branch in decl_if is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_decl_if_else_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_if, "node is not decl_if");
        if(node.children.size() > (4)) {
            ANTON_ASSERT(node.children[4].is_left(), "else_branch in decl_if is not Syntax_Node");
            return node.children[4].left();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static Syntax_Node const& get_decl_import_path(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_import, "node is not decl_import");
        ANTON_ASSERT(node.children.size() > (1), "decl_import has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "path in decl_import is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_constant_type(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_constant, "node is not decl_constant");
        ANTON_ASSERT(node.children.size() > (1), "decl_constant has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "type in decl_constant is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Token const& get_decl_constant_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_constant, "node is not decl_constant");
        ANTON_ASSERT(node.children.size() > (2), "decl_constant has too few children");
        ANTON_ASSERT(node.children[2].is_right(), "identifier in decl_constant is not Syntax_Token");
        return node.children[2].right();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_constant_initializer(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_constant, "node is not decl_constant");
        ANTON_ASSERT(node.children.size() > (4), "decl_constant has too few children");
        ANTON_ASSERT(node.children[4].is_left(), "initializer in decl_constant is not Syntax_Node");
        return node.children[4].left();
    }

    [[nodiscard]] static Syntax_Token const& get_decl_struct_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_struct, "node is not decl_struct");
        ANTON_ASSERT(node.children.size() > (1), "decl_struct has too few children");
        ANTON_ASSERT(node.children[1].is_right(), "identifier in decl_struct is not Syntax_Token");
        return node.children[1].right();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_struct_members(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_struct, "node is not decl_struct");
        ANTON_ASSERT(node.children.size() > (2), "decl_struct has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "members in decl_struct is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_function_return_type(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_function, "node is not decl_function");
        ANTON_ASSERT(node.children.size() > (offset + 0), "decl_function has too few children");
        ANTON_ASSERT(node.children[offset + 0].is_left(), "return_type in decl_function is not Syntax_Node");
        return node.children[offset + 0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_decl_function_identifier(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_function, "node is not decl_function");
        ANTON_ASSERT(node.children.size() > (offset + 1), "decl_function has too few children");
        ANTON_ASSERT(node.children[offset + 1].is_right(), "identifier in decl_function is not Syntax_Token");
        return node.children[offset + 1].right();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_function_parameter_list(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_function, "node is not decl_function");
        ANTON_ASSERT(node.children.size() > (offset + 2), "decl_function has too few children");
        ANTON_ASSERT(node.children[offset + 2].is_left(), "parameter_list in decl_function is not Syntax_Node");
        return node.children[offset + 2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_function_body(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_function, "node is not decl_function");
        ANTON_ASSERT(node.children.size() > (offset + 3), "decl_function has too few children");
        ANTON_ASSERT(node.children[offset + 3].is_left(), "body in decl_function is not Syntax_Node");
        return node.children[offset + 3].left();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_stage_function_return_type(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_stage_function, "node is not decl_stage_function");
        ANTON_ASSERT(node.children.size() > (offset + 0), "decl_stage_function has too few children");
        ANTON_ASSERT(node.children[offset + 0].is_left(), "return_type in decl_stage_function is not Syntax_Node");
        return node.children[offset + 0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_decl_stage_function_pass(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_stage_function, "node is not decl_stage_function");
        ANTON_ASSERT(node.children.size() > (offset + 1), "decl_stage_function has too few children");
        ANTON_ASSERT(node.children[offset + 1].is_right(), "pass in decl_stage_function is not Syntax_Token");
        return node.children[offset + 1].right();
    }

    [[nodiscard]] static Syntax_Token const& get_decl_stage_function_stage(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_stage_function, "node is not decl_stage_function");
        ANTON_ASSERT(node.children.size() > (offset + 2), "decl_stage_function has too few children");
        ANTON_ASSERT(node.children[offset + 2].is_right(), "stage in decl_stage_function is not Syntax_Token");
        return node.children[offset + 2].right();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_stage_function_parameter_list(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_stage_function, "node is not decl_stage_function");
        ANTON_ASSERT(node.children.size() > (offset + 3), "decl_stage_function has too few children");
        ANTON_ASSERT(node.children[offset + 3].is_left(), "parameter_list in decl_stage_function is not Syntax_Node");
        return node.children[offset + 3].left();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_stage_function_body(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_stage_function, "node is not decl_stage_function");
        ANTON_ASSERT(node.children.size() > (offset + 4), "decl_stage_function has too few children");
        ANTON_ASSERT(node.children[offset + 4].is_left(), "body in decl_stage_function is not Syntax_Node");
        return node.children[offset + 4].left();
    }

    [[nodiscard]] static Syntax_Node const& get_func_parameter_type(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::func_parameter, "node is not func_parameter");
        ANTON_ASSERT(node.children.size() > (0), "func_parameter has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "type in func_parameter is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_func_parameter_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::func_parameter, "node is not func_parameter");
        ANTON_ASSERT(node.children.size() > (1), "func_parameter has too few children");
        ANTON_ASSERT(node.children[1].is_right(), "identifier in func_parameter is not Syntax_Token");
        return node.children[1].right();
    }

    [[nodiscard]] static anton::Optional<Syntax_Token const&> get_func_parameter_source(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::func_parameter, "node is not func_parameter");
        if(node.children.size() > (3)) {
            ANTON_ASSERT(node.children[3].is_right(), "source in func_parameter is not Syntax_Token");
            return node.children[3].right();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static Syntax_Node const& get_func_parameter_if_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::func_parameter_if, "node is not func_parameter_if");
        ANTON_ASSERT(node.children.size() > (1), "func_parameter_if has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "condition in func_parameter_if is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_func_parameter_if_then_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::func_parameter_if, "node is not func_parameter_if");
        ANTON_ASSERT(node.children.size() > (3), "func_parameter_if has too few children");
        ANTON_ASSERT(node.children[3].is_left(), "then_branch in func_parameter_if is not Syntax_Node");
        return node.children[3].left();
    }

    [[nodiscard]] static Syntax_Node const& get_func_parameter_if_else_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::func_parameter_if, "node is not func_parameter_if");
        ANTON_ASSERT(node.children.size() > (7), "func_parameter_if has too few children");
        ANTON_ASSERT(node.children[7].is_left(), "else_branch in func_parameter_if is not Syntax_Node");
        return node.children[7].left();
    }

    [[nodiscard]] static Syntax_Node const& get_struct_member_type(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::struct_member, "node is not struct_member");
        ANTON_ASSERT(node.children.size() > (offset + 0), "struct_member has too few children");
        ANTON_ASSERT(node.children[offset + 0].is_left(), "type in struct_member is not Syntax_Node");
        return node.children[offset + 0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_struct_member_identifier(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::struct_member, "node is not struct_member");
        ANTON_ASSERT(node.children.size() > (offset + 1), "struct_member has too few children");
        ANTON_ASSERT(node.children[offset + 1].is_right(), "identifier in struct_member is not Syntax_Token");
        return node.children[offset + 1].right();
    }

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_struct_member_initializer(Syntax_Node const& node, i64 const offset) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::struct_member, "node is not struct_member");
        if(node.children.size() > (offset + 3)) {
            ANTON_ASSERT(node.children[offset + 3].is_left(), "initializer in struct_member is not Syntax_Node");
            return node.children[offset + 3].left();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static Syntax_Node const& get_expr_if_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() > (1), "expr_if has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "condition in expr_if is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_if_then_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() > (2), "expr_if has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "then_branch in expr_if is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_if_else_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() > (4), "expr_if has too few children");
        ANTON_ASSERT(node.children[4].is_left(), "else_branch in expr_if is not Syntax_Node");
        return node.children[4].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_binary_lhs(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() > (0), "expr_binary has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "lhs in expr_binary is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_binary_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() > (1), "expr_binary has too few children");
        ANTON_ASSERT(node.children[1].is_right(), "operator in expr_binary is not Syntax_Token");
        return node.children[1].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_binary_rhs(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() > (2), "expr_binary has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "rhs in expr_binary is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_block_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_block, "node is not expr_block");
        ANTON_ASSERT(node.children.size() > (0), "expr_block has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "expression in expr_block is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_identifier_value(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_identifier, "node is not expr_identifier");
        ANTON_ASSERT(node.children.size() > (0), "expr_identifier has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "value in expr_identifier is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_prefix_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_prefix, "node is not expr_prefix");
        ANTON_ASSERT(node.children.size() > (0), "expr_prefix has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "operator in expr_prefix is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_prefix_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_prefix, "node is not expr_prefix");
        ANTON_ASSERT(node.children.size() > (1), "expr_prefix has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in expr_prefix is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_postfix_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_postfix, "node is not expr_postfix");
        ANTON_ASSERT(node.children.size() > (0), "expr_postfix has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "operator in expr_postfix is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_postfix_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_postfix, "node is not expr_postfix");
        ANTON_ASSERT(node.children.size() > (1), "expr_postfix has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in expr_postfix is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_member_access_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_member_access, "node is not expr_member_access");
        ANTON_ASSERT(node.children.size() > (0), "expr_member_access has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "expression in expr_member_access is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_member_access_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_member_access, "node is not expr_member_access");
        ANTON_ASSERT(node.children.size() > (1), "expr_member_access has too few children");
        ANTON_ASSERT(node.children[1].is_right(), "identifier in expr_member_access is not Syntax_Token");
        return node.children[1].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_array_access_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_array_access, "node is not expr_array_access");
        ANTON_ASSERT(node.children.size() > (0), "expr_array_access has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "expression in expr_array_access is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_array_access_index(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_array_access, "node is not expr_array_access");
        ANTON_ASSERT(node.children.size() > (2), "expr_array_access has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "index in expr_array_access is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_parentheses_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_parentheses, "node is not expr_parentheses");
        ANTON_ASSERT(node.children.size() > (1), "expr_parentheses has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in expr_parentheses is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_call_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_call, "node is not expr_call");
        ANTON_ASSERT(node.children.size() > (0), "expr_call has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "identifier in expr_call is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_call_arguments(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_call, "node is not expr_call");
        ANTON_ASSERT(node.children.size() > (1), "expr_call has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "arguments in expr_call is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_lt_bool_value(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_lt_bool, "node is not expr_lt_bool");
        ANTON_ASSERT(node.children.size() > (0), "expr_lt_bool has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "value in expr_lt_bool is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_lt_integer_value(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_lt_integer, "node is not expr_lt_integer");
        ANTON_ASSERT(node.children.size() > (0), "expr_lt_integer has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "value in expr_lt_integer is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static anton::Optional<Syntax_Token const&> get_expr_lt_integer_suffix(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_lt_integer, "node is not expr_lt_integer");
        if(node.children.size() > (1)) {
            ANTON_ASSERT(node.children[1].is_right(), "suffix in expr_lt_integer is not Syntax_Token");
            return node.children[1].right();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static Syntax_Token const& get_expr_lt_float_value(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_lt_float, "node is not expr_lt_float");
        ANTON_ASSERT(node.children.size() > (0), "expr_lt_float has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "value in expr_lt_float is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static anton::Optional<Syntax_Token const&> get_expr_lt_float_suffix(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_lt_float, "node is not expr_lt_float");
        if(node.children.size() > (1)) {
            ANTON_ASSERT(node.children[1].is_right(), "suffix in expr_lt_float is not Syntax_Token");
            return node.children[1].right();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static Syntax_Token const& get_expr_lt_string_value(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_lt_string, "node is not expr_lt_string");
        ANTON_ASSERT(node.children.size() > (0), "expr_lt_string has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "value in expr_lt_string is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_stmt_if_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_if, "node is not stmt_if");
        ANTON_ASSERT(node.children.size() > (1), "stmt_if has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "condition in stmt_if is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_stmt_if_then_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_if, "node is not stmt_if");
        ANTON_ASSERT(node.children.size() > (2), "stmt_if has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "then_branch in stmt_if is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_stmt_if_else_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_if, "node is not stmt_if");
        if(node.children.size() > (4)) {
            ANTON_ASSERT(node.children[4].is_left(), "else_branch in stmt_if is not Syntax_Node");
            return node.children[4].left();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static Syntax_Node const& get_stmt_switch_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_switch, "node is not stmt_switch");
        ANTON_ASSERT(node.children.size() > (1), "stmt_switch has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in stmt_switch is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_stmt_switch_arm_list(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_switch, "node is not stmt_switch");
        ANTON_ASSERT(node.children.size() > (2), "stmt_switch has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "arm_list in stmt_switch is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_switch_arm_body(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::switch_arm, "node is not switch_arm");
        for(SNOT const& snot: node.children) {
            if(snot.is_left() && snot.left().type == Syntax_Node_Type::stmt_block) {
                return snot.left();
            }
        }
        ANTON_ASSERT(false, "stmt_block not present in switch_arm");
        ANTON_UNREACHABLE();
    };

    [[nodiscard]] static Syntax_Node const& get_stmt_while_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_while, "node is not stmt_while");
        ANTON_ASSERT(node.children.size() > (1), "stmt_while has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "condition in stmt_while is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_stmt_while_statements(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_while, "node is not stmt_while");
        ANTON_ASSERT(node.children.size() > (2), "stmt_while has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "statements in stmt_while is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_stmt_for_variable(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_for, "node is not stmt_for");
        for(SNOT const& snot: node.children) {
            if(snot.is_left() && snot.left().type == Syntax_Node_Type::for_variable) {
                return snot.left().children[0].left();
            }
        }
        return anton::null_optional;
    };

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_stmt_for_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_for, "node is not stmt_for");
        for(SNOT const& snot: node.children) {
            if(snot.is_left() && snot.left().type == Syntax_Node_Type::for_condition) {
                return snot.left().children[0].left();
            }
        }
        return anton::null_optional;
    };

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_stmt_for_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_for, "node is not stmt_for");
        for(SNOT const& snot: node.children) {
            if(snot.is_left() && snot.left().type == Syntax_Node_Type::for_expression) {
                return snot.left().children[0].left();
            }
        }
        return anton::null_optional;
    };

    [[nodiscard]] static Syntax_Node const& get_stmt_for_body(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_for, "node is not stmt_for");
        for(SNOT const& snot: node.children) {
            if(snot.is_left() && snot.left().type == Syntax_Node_Type::stmt_block) {
                return snot.left();
            }
        }
        ANTON_ASSERT(false, "stmt_block not present in stmt_for");
        ANTON_UNREACHABLE();
    };

    [[nodiscard]] static Syntax_Node const& get_stmt_do_while_body(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_do_while, "node is not stmt_do_while");
        ANTON_ASSERT(node.children.size() > (1), "stmt_do_while has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "body in stmt_do_while is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_stmt_do_while_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_do_while, "node is not stmt_do_while");
        ANTON_ASSERT(node.children.size() > (3), "stmt_do_while has too few children");
        ANTON_ASSERT(node.children[3].is_left(), "condition in stmt_do_while is not Syntax_Node");
        return node.children[3].left();
    }

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_stmt_return_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_return, "node is not stmt_return");
        for(SNOT const& snot: node.children) {
            if(snot.is_left() && snot.left().type == Syntax_Node_Type::return_expression) {
                return snot.left().children[0].left();
            }
        }
        return anton::null_optional;
    };

    [[nodiscard]] static Syntax_Node const& get_stmt_variable_type(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_variable, "node is not stmt_variable");
        ANTON_ASSERT(node.children.size() > (0), "stmt_variable has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "type in stmt_variable is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_stmt_variable_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_variable, "node is not stmt_variable");
        ANTON_ASSERT(node.children.size() > (1), "stmt_variable has too few children");
        ANTON_ASSERT(node.children[1].is_right(), "identifier in stmt_variable is not Syntax_Token");
        return node.children[1].right();
    }

    [[nodiscard]] static anton::Optional<Syntax_Node const&> get_stmt_variable_initializer(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_variable, "node is not stmt_variable");
        if(node.children.size() > (3)) {
            ANTON_ASSERT(node.children[3].is_left(), "initializer in stmt_variable is not Syntax_Node");
            return node.children[3].left();
        } else {
            return anton::null_optional;
        }
    }

    [[nodiscard]] static Syntax_Node const& get_stmt_expression_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::stmt_expression, "node is not stmt_expression");
        ANTON_ASSERT(node.children.size() > (0), "stmt_expression has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "expression in stmt_expression is not Syntax_Node");
        return node.children[0].left();
    }
} // namespace vush
