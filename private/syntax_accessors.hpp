#include <ast.hpp>

namespace vush {
    [[nodiscard]] static Syntax_Node const& get_expr_if_condition(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() >= 2, "expr_if has fewer than 2 children");
        ANTON_ASSERT(node.children[1].is_left(), "condition in expr_if is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_if_then_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() >= 4, "expr_if has fewer than 4 children");
        ANTON_ASSERT(node.children[3].is_left(), "then_branch in expr_if is not Syntax_Node");
        return node.children[3].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_if_else_branch(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_if, "node is not expr_if");
        ANTON_ASSERT(node.children.size() >= 6, "expr_if has fewer than 6 children");
        ANTON_ASSERT(node.children[5].is_left(), "else_branch in expr_if is not Syntax_Node");
        return node.children[5].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_binary_lhs(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() >= 1, "expr_binary has fewer than 1 children");
        ANTON_ASSERT(node.children[0].is_left(), "lhs in expr_binary is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_binary_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() >= 2, "expr_binary has fewer than 2 children");
        ANTON_ASSERT(node.children[1].is_right(), "operator in expr_binary is not Syntax_Token");
        return node.children[1].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_binary_rhs(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_binary, "node is not expr_binary");
        ANTON_ASSERT(node.children.size() >= 3, "expr_binary has fewer than 3 children");
        ANTON_ASSERT(node.children[2].is_left(), "rhs in expr_binary is not Syntax_Node");
        return node.children[2].left();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_block_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_block, "node is not expr_block");
        ANTON_ASSERT(node.children.size() >= 1, "expr_block has fewer than 1 children");
        ANTON_ASSERT(node.children[0].is_left(), "expression in expr_block is not Syntax_Node");
        return node.children[0].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_identifier_identifier(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_identifier, "node is not expr_identifier");
        ANTON_ASSERT(node.children.size() >= 1, "expr_identifier has fewer than 1 children");
        ANTON_ASSERT(node.children[0].is_right(), "identifier in expr_identifier is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_prefix_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_prefix, "node is not expr_prefix");
        ANTON_ASSERT(node.children.size() >= 1, "expr_prefix has fewer than 1 children");
        ANTON_ASSERT(node.children[0].is_right(), "operator in expr_prefix is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_prefix_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_prefix, "node is not expr_prefix");
        ANTON_ASSERT(node.children.size() >= 2, "expr_prefix has fewer than 2 children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in expr_prefix is not Syntax_Node");
        return node.children[1].left();
    }

    [[nodiscard]] static Syntax_Token const& get_expr_postfix_operator(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_postfix, "node is not expr_postfix");
        ANTON_ASSERT(node.children.size() >= 1, "expr_postfix has fewer than 1 children");
        ANTON_ASSERT(node.children[0].is_right(), "operator in expr_postfix is not Syntax_Token");
        return node.children[0].right();
    }

    [[nodiscard]] static Syntax_Node const& get_expr_postfix_expression(Syntax_Node const& node) {
        ANTON_ASSERT(node.type == Syntax_Node_Type::expr_postfix, "node is not expr_postfix");
        ANTON_ASSERT(node.children.size() >= 2, "expr_postfix has fewer than 2 children");
        ANTON_ASSERT(node.children[1].is_left(), "expression in expr_postfix is not Syntax_Node");
        return node.children[1].left();
    }
} // namespace vush
