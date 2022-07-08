import subprocess

from node_definitions import syntax_nodes, Node_Type

def format_node_accessor(name, member, index):
    return f'''\
[[nodiscard]] static Syntax_Node const& get_{name}_{member}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.type == Syntax_Node_Type::{name}, "node is not {name}");
    ANTON_ASSERT(node.children.size() >= {index + 1}, "{name} has fewer than {index + 1} children");
    ANTON_ASSERT(node.children[{index}].is_left(), "{member} in {name} is not Syntax_Node");
    return node.children[{index}].left();
}}'''

def format_token_accessor(name, member, index):
    return f'''\
[[nodiscard]] static Syntax_Token const& get_{name}_{member}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.type == Syntax_Node_Type::{name}, "node is not {name}");
    ANTON_ASSERT(node.children.size() >= {index + 1}, "{name} has fewer than {index + 1} children");
    ANTON_ASSERT(node.children[{index}].is_right(), "{member} in {name} is not Syntax_Token");
    return node.children[{index}].right();
}}'''

def generate_accessors(parameters):
    accessors = []
    for node_type, member, index in parameters["members"]:
        if node_type == Node_Type.token:
            accessor = format_token_accessor(name = parameters["syntax_name"], member = member, index = index)
            accessors.append(accessor)
        else:
            accessor = format_node_accessor(name = parameters["syntax_name"], member = member, index = index)
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
    accessor_strings = []
    for parameters in syntax_nodes:
        accessor_strings.extend(generate_accessors(parameters))
    
    file = open("./private/syntax_accessors.hpp", "w")

    write_preamble(file)
    for v in accessor_strings:
        file.write("\n")
        file.write(v)
        file.write("\n")
    write_epilogue(file)

    file.close()

    process = subprocess.run(["clang-format", "-i", "./private/syntax_accessors.hpp"])
    process.check_returncode()

main()
