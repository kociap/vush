#include <ast.hpp>

namespace vush {
    [[nodiscard]] static Syntax_Node const& get_decl_if_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_if, "node is not decl_if");
        ANTON_ASSERT(node.children.size() >= (1 + 1), "decl_if has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "condition in decl_if is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_if_then_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_if, "node is not decl_if");
        ANTON_ASSERT(node.children.size() >= (3 + 1), "decl_if has too few children");
        ANTON_ASSERT(node.children[3].is_left(), "then_branch in decl_if is not Syntax_Node");
        return node.children[3].left();
    }

    [[nodiscard]] static Syntax_Node const& get_decl_if_else_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::decl_if, "node is not decl_if");
        ANTON_ASSERT(node.children.size() >= (5 + 1), "decl_if has too few children");
        ANTON_ASSERT(node.children[5].is_left(), "else_branch in decl_if is not Syntax_Node");
        return node.children[5].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_if_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() >= (1 + 1), "expr_if has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "condition in expr_if is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_if_then_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() >= (3 + 1), "expr_if has too few children");
        ANTON_ASSERT(node.children[3].is_left(), "then_branch in expr_if is not Syntax_Node");
        return node.children[3].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_if_else_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() >= (5 + 1), "expr_if has too few children");
        ANTON_ASSERT(node.children[5].is_left(), "else_branch in expr_if is not Syntax_Node");
        return node.children[5].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_binary_lhs(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_binary has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "lhs in expr_binary is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_binary_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() >= (1 + 1), "expr_binary has too few children");
        ANTON_ASSERT(node.children[1].is_right(), "operator in expr_binary is not Syntax_Token");
        return node.children[1].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_binary_rhs(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() >= (2 + 1), "expr_binary has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "rhs in expr_binary is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_block_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_block, "node is not expr_block");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_block has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "expression in expr_block is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_identifier_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_identifier, "node is not expr_identifier");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_identifier has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "identifier in expr_identifier is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_prefix_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_prefix, "node is not expr_prefix");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_prefix has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "operator in expr_prefix is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_prefix_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_prefix, "node is not expr_prefix");
        ANTON_ASSERT(node.children.size() >= (1 + 1), "expr_prefix has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in expr_prefix is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_postfix_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_postfix, "node is not expr_postfix");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_postfix has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "operator in expr_postfix is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_postfix_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_postfix, "node is not expr_postfix");
        ANTON_ASSERT(node.children.size() >= (1 + 1), "expr_postfix has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in expr_postfix is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_member_access_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_member_access, "node is not expr_member_access");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_member_access has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "expression in expr_member_access is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_member_access_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_member_access, "node is not expr_member_access");
        ANTON_ASSERT(node.children.size() >= (1 + 1), "expr_member_access has too few children");
        ANTON_ASSERT(node.children[1].is_right(), "identifier in expr_member_access is not Syntax_Token");
        return node.children[1].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_array_access_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_array_access, "node is not expr_array_access");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_array_access has too few children");
        ANTON_ASSERT(node.children[0].is_left(), "expression in expr_array_access is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_array_access_index(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_array_access, "node is not expr_array_access");
        ANTON_ASSERT(node.children.size() >= (2 + 1), "expr_array_access has too few children");
        ANTON_ASSERT(node.children[2].is_left(), "index in expr_array_access is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_parentheses_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_parentheses, "node is not expr_parentheses");
        ANTON_ASSERT(node.children.size() >= (1 + 1), "expr_parentheses has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in expr_parentheses is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_call_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_call, "node is not expr_call");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_call has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "identifier in expr_call is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_call_arguments(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_call, "node is not expr_call");
        ANTON_ASSERT(node.children.size() >= (1 + 1), "expr_call has too few children");
        ANTON_ASSERT(node.children[1].is_left(), "arguments in expr_call is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_literal_value(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_literal, "node is not expr_literal");
        ANTON_ASSERT(node.children.size() >= (0 + 1), "expr_literal has too few children");
        ANTON_ASSERT(node.children[0].is_right(), "value in expr_literal is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static anton::Optional<Syntax_Token const&> get_expr_literal_suffix(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_literal, "node is not expr_literal");
        if(node.children.size() >= (1 + 1)) {
            ANTON_ASSERT(node.children[1].is_right(), "suffix in expr_literal is not Syntax_Token");
            return node.children[1].right();
        } else {
            return anton::null_optional;
        }
    }
} // namespace vush
