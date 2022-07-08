from enum import Enum

class Node_Type(Enum):
    token = 1
    node = 2

syntax_nodes = [
    {"syntax_name": "expr_if", "members": [(Node_Type.node, "condition", 1), (Node_Type.node, "then_branch", 3), (Node_Type.node, "else_branch", 5)]},
    {"syntax_name": "expr_binary", "members": [(Node_Type.node, "lhs", 0), (Node_Type.token, "operator", 1), (Node_Type.node, "rhs", 2)]},
    {"syntax_name": "expr_block", "members": [(Node_Type.node, "expression", 0)]},
    {"syntax_name": "expr_identifier", "members": [(Node_Type.token, "identifier", 0)]},
    {"syntax_name": "expr_prefix", "members": [(Node_Type.token, "operator", 0), (Node_Type.node, "expression", 1)]},
    {"syntax_name": "expr_postfix", "members": [(Node_Type.token, "operator", 0), (Node_Type.node, "expression", 1)]},
]
