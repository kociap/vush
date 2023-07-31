from enum import Enum

class Lookup_Kind(Enum):
    index = 1
    search = 2

class Node_Kind(Enum):
    token = 1
    node = 2

class Syntax_Member:
    # unwrap has effect only when node_kind is node and lookup is search.
    def __init__(self, node_kind, name, lookup, index, optional = False, unwrap = False, offset = False):
        self.node_kind = node_kind
        self.name = name
        self.lookup = lookup
        self.index = index
        self.optional = optional
        self.unwrap = unwrap
        self.offset = offset

syntax_nodes = [
    {
        "syntax_name": "type_builtin",
        "members": [
            Syntax_Member(Node_Kind.token, "mut", Lookup_Kind.search, "kw_mut", optional = True),
            Syntax_Member(Node_Kind.token, "value", Lookup_Kind.search, "identifier")
        ]
    },
    {
        "syntax_name": "type_struct",
        "members": [
            Syntax_Member(Node_Kind.token, "mut", Lookup_Kind.search, "kw_mut", optional = True),
            Syntax_Member(Node_Kind.token, "value", Lookup_Kind.search, "identifier")
        ]
    },
    {
        "syntax_name": "type_array",
        "members": [
            Syntax_Member(Node_Kind.token, "mut", Lookup_Kind.search, "kw_mut", optional = True),
            Syntax_Member(Node_Kind.node, "base", Lookup_Kind.search, "type_array_base", unwrap = True),
            Syntax_Member(Node_Kind.node, "size", Lookup_Kind.search, "type_array_size", optional = True, unwrap = True)
        ]
    },
    {
        "syntax_name": "attribute",
        "members": [
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "parameter_list", Lookup_Kind.index, 2, optional = True)
        ]
    },
    {
        "syntax_name": "attribute_parameter_keyed",
        "members": [
            Syntax_Member(Node_Kind.token, "key", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "value", Lookup_Kind.index, 2)
        ]
    },
    {
        "syntax_name": "attribute_parameter_positional",
        "members": [
            Syntax_Member(Node_Kind.node, "value", Lookup_Kind.index, 0),
        ]
    },
    {
        "syntax_name": "variable",
        "members": [
            Syntax_Member(Node_Kind.node, "type", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "initializer", Lookup_Kind.index, 3, optional = True)
        ]
    },
    {
        "syntax_name": "decl_if",
        "members": [
            Syntax_Member(Node_Kind.node, "condition", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "then_branch", Lookup_Kind.index, 2),
            Syntax_Member(Node_Kind.node, "else_branch", Lookup_Kind.index, 4, optional = True)
        ]
    },
    {
        "syntax_name": "decl_import",
        "members": [Syntax_Member(Node_Kind.node, "path", Lookup_Kind.index, 1)]
    },
    {
        "syntax_name": "decl_struct",
        "members": [
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "members", Lookup_Kind.index, 2)
        ]
    },
    {
        "syntax_name": "decl_function",
        "members": [
            Syntax_Member(Node_Kind.node, "attribute_list", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "return_type", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 2),
            Syntax_Member(Node_Kind.node, "parameter_list", Lookup_Kind.index, 3),
            Syntax_Member(Node_Kind.node, "body", Lookup_Kind.index, 4)
        ]
    },
    {
        "syntax_name": "decl_stage_function",
        "members": [
            Syntax_Member(Node_Kind.node, "attribute_list", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "return_type", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.token, "pass", Lookup_Kind.index, 2),
            Syntax_Member(Node_Kind.token, "stage", Lookup_Kind.index, 4),
            Syntax_Member(Node_Kind.node, "parameter_list", Lookup_Kind.index, 5),
            Syntax_Member(Node_Kind.node, "body", Lookup_Kind.index, 6)
        ]
    },
    {
        "syntax_name": "fn_parameter",
        "members": [
            Syntax_Member(Node_Kind.node, "type", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.token, "source", Lookup_Kind.index, 3, optional = True)
        ]
    },
    {
        "syntax_name": "fn_parameter_if",
        "members": [
            Syntax_Member(Node_Kind.node, "condition", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "then_branch", Lookup_Kind.index, 3),
            Syntax_Member(Node_Kind.node, "else_branch", Lookup_Kind.index, 7)
        ]
    },
    {
        "syntax_name": "struct_member",
        "members": [
            Syntax_Member(Node_Kind.node, "attribute_list", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "type", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 2),
            Syntax_Member(Node_Kind.node, "initializer", Lookup_Kind.index, 4, optional = True)
        ]
    },
    {
        "syntax_name": "expr_if",
        "members": [
            Syntax_Member(Node_Kind.node, "condition", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "then_branch", Lookup_Kind.index, 2),
            Syntax_Member(Node_Kind.node, "else_branch", Lookup_Kind.index, 4)
        ]
    },
    {
        "syntax_name": "expr_binary",
        "members": [
            Syntax_Member(Node_Kind.node, "lhs", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.token, "operator", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "rhs", Lookup_Kind.index, 2)
        ]
    },
    {
        "syntax_name": "expr_block",
        "members": [Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 1)]
    },
    {
        "syntax_name": "expr_identifier",
        "members": [Syntax_Member(Node_Kind.token, "value", Lookup_Kind.index, 0)]
    },
    {
        "syntax_name": "expr_prefix",
        "members": [
            Syntax_Member(Node_Kind.token, "operator", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 1)
        ]
    },
    {
        "syntax_name": "expr_field",
        "members": [
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 1)
        ]
    },
    {
        "syntax_name": "expr_index",
        "members": [
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "index", Lookup_Kind.index, 2)
        ]
    },
    {
        "syntax_name": "expr_parentheses",
        "members": [Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 1)]
    },
    {
        "syntax_name": "named_initializer",
        "members": [
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 3)
        ]
    },
    {
        "syntax_name": "indexed_initializer",
        "members": [
            Syntax_Member(Node_Kind.node, "index", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 2)
        ]
    },
    {
        "syntax_name": "basic_initializer",
        "members": [
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 0)
        ]
    },
    {
        "syntax_name": "expr_init",
        "members": [
            Syntax_Member(Node_Kind.node, "type", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "initializers", Lookup_Kind.index, 1)
        ]
    },
    {
        "syntax_name": "expr_call",
        "members": [
            Syntax_Member(Node_Kind.token, "identifier", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.node, "arguments", Lookup_Kind.index, 1)
        ]
    },
    {
        "syntax_name": "expr_lt_bool",
        "members": [Syntax_Member(Node_Kind.token, "value", Lookup_Kind.index, 0)]
    },
    {
        "syntax_name": "expr_lt_integer",
        "members": [
            Syntax_Member(Node_Kind.token, "value", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.token, "suffix", Lookup_Kind.index, 1, optional = True)
        ]
    },
    {
        "syntax_name": "expr_lt_float",
        "members": [
            Syntax_Member(Node_Kind.token, "value", Lookup_Kind.index, 0),
            Syntax_Member(Node_Kind.token, "suffix", Lookup_Kind.index, 1, optional = True)
        ]
    },
    {
        "syntax_name": "expr_lt_string",
        "members": [Syntax_Member(Node_Kind.token, "value", Lookup_Kind.index, 0)]
    },
    {
        "syntax_name": "stmt_if",
        "members": [
            Syntax_Member(Node_Kind.node, "condition", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "then_branch", Lookup_Kind.index, 2),
            Syntax_Member(Node_Kind.node, "else_branch", Lookup_Kind.index, 4, optional = True),
        ]
    },
    {
        "syntax_name": "stmt_switch",
        "members": [
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "arm_list", Lookup_Kind.index, 2),
        ]
    },
    {
        "syntax_name": "switch_arm",
        "members": [
            Syntax_Member(Node_Kind.node, "body", Lookup_Kind.search, "stmt_block"),
        ]
    },
    {
        "syntax_name": "stmt_while",
        "members": [
            Syntax_Member(Node_Kind.node, "condition", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "statements", Lookup_Kind.index, 2),
        ]
    },
    {
        "syntax_name": "stmt_for",
        "members": [
            Syntax_Member(Node_Kind.node, "variable", Lookup_Kind.search, "for_variable", optional = True),
            Syntax_Member(Node_Kind.node, "condition", Lookup_Kind.search, "for_condition", optional = True, unwrap = True),
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.search, "for_expression", optional = True, unwrap = True),
            Syntax_Member(Node_Kind.node, "body", Lookup_Kind.search, "stmt_block"),
        ]
    },
    {
        "syntax_name": "stmt_do_while",
        "members": [
            Syntax_Member(Node_Kind.node, "body", Lookup_Kind.index, 1),
            Syntax_Member(Node_Kind.node, "condition", Lookup_Kind.index, 3),
        ]
    },
    {
        "syntax_name": "stmt_return",
        "members": [
            Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.search, "return_expression", optional = True, unwrap = True)
        ]
    },
    {
        "syntax_name": "stmt_expression",
        "members": [Syntax_Member(Node_Kind.node, "expression", Lookup_Kind.index, 0)]
    },
]
