import subprocess

from node_definitions import syntax_nodes, Node_Type

def format_node_accessor(syntax_name, name, index, optional, indexed):
    index_parameter = ", i64 const index" if indexed else ""
    if optional:
        return f'''\
[[nodiscard]] static anton::Optional<Syntax_Node const&> get_{syntax_name}_{name}(Syntax_Node const& node {index_parameter}) {{
    ANTON_ASSERT(node.type == Syntax_Node_Type::{syntax_name}, "node is not {syntax_name}");
    return node.children[{index}].left();
    if(node.children.size() >= ({index} + 1)) {{
        ANTON_ASSERT(node.children[{index}].is_left(), "{name} in {syntax_name} is not Syntax_Node");
        return node.children[{index}].left();
    }} else {{
        return anton::null_optional;
    }}
}}'''
    else:
        return f'''\
[[nodiscard]] static Syntax_Node const& get_{syntax_name}_{name}(Syntax_Node const& node {index_parameter}) {{
    ANTON_ASSERT(node.type == Syntax_Node_Type::{syntax_name}, "node is not {syntax_name}");
    ANTON_ASSERT(node.children.size() >= ({index} + 1), "{syntax_name} has too few children");
    ANTON_ASSERT(node.children[{index}].is_left(), "{name} in {syntax_name} is not Syntax_Node");
    return node.children[{index}].left();
}}'''

def format_token_accessor(syntax_name, name, index, optional, indexed):
    index_parameter = ", i64 const index" if indexed else ""
    if optional:
        return f'''\
[[nodiscard]] static anton::Optional<Syntax_Token const&> get_{syntax_name}_{name}(Syntax_Node const& node {index_parameter}) {{
    ANTON_ASSERT(node.type == Syntax_Node_Type::{syntax_name}, "node is not {syntax_name}");
    if(node.children.size() >= ({index} + 1)) {{
        ANTON_ASSERT(node.children[{index}].is_right(), "{name} in {syntax_name} is not Syntax_Token");
        return node.children[{index}].right();
    }} else {{
        return anton::null_optional;
    }}
}}'''
    else:
        return f'''\
[[nodiscard]] static Syntax_Token const& get_{syntax_name}_{name}(Syntax_Node const& node {index_parameter}) {{
    ANTON_ASSERT(node.type == Syntax_Node_Type::{syntax_name}, "node is not {syntax_name}");
    ANTON_ASSERT(node.children.size() >= ({index} + 1), "{syntax_name} has too few children");
    ANTON_ASSERT(node.children[{index}].is_right(), "{name} in {syntax_name} is not Syntax_Token");
    return node.children[{index}].right();
}}'''

def generate_accessors(parameters):
    accessors = []
    for member in parameters["members"]:
        if member.node_type == Node_Type.token:
            accessor = format_token_accessor(syntax_name = parameters["syntax_name"], name = member.name, index = member.index, 
                                             optional = member.optional, indexed = member.indexed)
            accessors.append(accessor)
        else:
            accessor = format_node_accessor(syntax_name = parameters["syntax_name"], name = member.name, index = member.index,
                                            optional = member.optional, indexed = member.indexed)
            accessors.append(accessor)
    return accessors

def write_preamble(file):
    preamble = '''\
#include <ast.hpp>

namespace vush {'''
    file.write(preamble)

def write_epilogue(file):
    file.write("}\n")

def main():
    file = open("./private/syntax_accessors.hpp", "w")

    write_preamble(file)
    for parameters in syntax_nodes:
        file.write("\n")
        accessor = generate_accessors(parameters)
        file.write(accessor)
        file.write("\n")
    write_epilogue(file)

    file.close()

    process = subprocess.run(["clang-format", "-i", "./private/syntax_accessors.hpp"])
    process.check_returncode()

main()
