from enum import Enum

class Node_Type(Enum):
    token = 1
    node = 2

class Syntax_Member:
    def __init__(self, node_type, name, index, optional = False, indexed = False):
        self.node_type = node_type
        self.name = name
        self.index = index
        self.optional = optional
        self.indexed = indexed

syntax_nodes = [
    {"syntax_name": "decl_if", "members": [Syntax_Member(Node_Type.node, "condition", 1), Syntax_Member(Node_Type.node, "then_branch", 3), Syntax_Member(Node_Type.node, "else_branch", 5)]},
    {"syntax_name": "expr_if", "members": [Syntax_Member(Node_Type.node, "condition", 1), Syntax_Member(Node_Type.node, "then_branch", 3), Syntax_Member(Node_Type.node, "else_branch", 5)]},
    {"syntax_name": "expr_binary", "members": [Syntax_Member(Node_Type.node, "lhs", 0), Syntax_Member(Node_Type.token, "operator", 1), Syntax_Member(Node_Type.node, "rhs", 2)]},
    {"syntax_name": "expr_block", "members": [Syntax_Member(Node_Type.node, "expression", 0)]},
    {"syntax_name": "expr_identifier", "members": [Syntax_Member(Node_Type.token, "identifier", 0)]},
    {"syntax_name": "expr_prefix", "members": [Syntax_Member(Node_Type.token, "operator", 0), Syntax_Member(Node_Type.node, "expression", 1)]},
    {"syntax_name": "expr_postfix", "members": [Syntax_Member(Node_Type.token, "operator", 0), Syntax_Member(Node_Type.node, "expression", 1)]},
    {"syntax_name": "expr_member_access", "members": [Syntax_Member(Node_Type.node, "expression", 0), Syntax_Member(Node_Type.token, "identifier", 1)]},
    {"syntax_name": "expr_array_access", "members": [Syntax_Member(Node_Type.node, "expression", 0), Syntax_Member(Node_Type.node, "index", 2)]},
    {"syntax_name": "expr_parentheses", "members": [Syntax_Member(Node_Type.node, "expression", 1)]},
    {"syntax_name": "expr_call", "members": [Syntax_Member(Node_Type.token, "identifier", 0), Syntax_Member(Node_Type.node, "arguments", 1)]},
    {"syntax_name": "expr_literal", "members": [Syntax_Member(Node_Type.token, "value", 0), Syntax_Member(Node_Type.token, "suffix", 1, optional = True)]},
]
