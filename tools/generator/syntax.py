import subprocess

from node_definitions import syntax_nodes, Node_Kind, Lookup_Kind

def format_node_accessor(syntax_name, member):
    def create_search_accessor(syntax_name, member):
        if member.optional:
            return f'''\
[[nodiscard]] static anton::Optional<Syntax_Node const&> get_{syntax_name}_{member.name}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.type == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    for(SNOT const& snot: node.children) {{
        if(snot.is_left() && snot.left().type == Syntax_Node_Kind::{member.index}) {{
            return snot.left(){".children[0].left()" if member.unwrap else ""};
        }}
    }}
    return anton::null_optional;
}};'''
        else:
            return f'''\
[[nodiscard]] static Syntax_Node const& get_{syntax_name}_{member.name}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.type == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    for(SNOT const& snot: node.children) {{
        if(snot.is_left() && snot.left().type == Syntax_Node_Kind::{member.index}) {{
            return snot.left(){".children[0].left()" if member.unwrap else ""};
        }}
    }}
    ANTON_ASSERT(false, "{member.index} not present in {syntax_name}");
    ANTON_UNREACHABLE();
}};'''

    def create_index_accessor(syntax_name, member):
        offset_parameter = ", i64 const offset" if member.offset else ""
        offset_expression = "offset + " if member.offset else ""
        if member.optional:
            return f'''\
[[nodiscard]] static anton::Optional<Syntax_Node const&> get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter}) {{
    ANTON_ASSERT(node.type == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    if(node.children.size() > ({offset_expression}{member.index})) {{
        ANTON_ASSERT(node.children[{offset_expression}{member.index}].is_left(), "{member.name} in {syntax_name} is not Syntax_Node");
        return node.children[{offset_expression}{member.index}].left();
    }} else {{
        return anton::null_optional;
    }}
}}'''
        else:
            return f'''\
[[nodiscard]] static Syntax_Node const& get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter}) {{
    ANTON_ASSERT(node.type == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    ANTON_ASSERT(node.children.size() > ({offset_expression}{member.index}), "{syntax_name} has too few children");
    ANTON_ASSERT(node.children[{offset_expression}{member.index}].is_left(), "{member.name} in {syntax_name} is not Syntax_Node");
    return node.children[{offset_expression}{member.index}].left();
}}'''

    if member.lookup == Lookup_Kind.search:
        return create_search_accessor(syntax_name, member)
    else:
        return create_index_accessor(syntax_name, member)

def format_token_accessor(syntax_name, member):
    def create_search_accessor(syntax_name, member):
        if member.optional:
            return f'''\
[[nodiscard]] static anton::Optional<Syntax_Token const&> get_{syntax_name}_{member.name}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.type == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    for(SNOT const& snot: node.children) {{
        if(snot.is_right() && snot.right().type == Syntax_Node_Kind::{member.index}) {{
            return snot.right();
        }}
    }}
    return anton::null_optional;
}};'''
        else:
            return f'''\
[[nodiscard]] static Syntax_Token const& get_{syntax_name}_{member.name}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.type == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    for(SNOT const& snot: node.children) {{
        if(snot.is_right() && snot.right().type == Syntax_Node_Kind::{member.index}) {{
            return snot.right();
        }}
    }}
    ANTON_ASSERT(false, "{member.index} not present in {syntax_name}");
    ANTON_UNREACHABLE();
}};'''


    def create_index_accessor(syntax_name, member):
        offset_parameter = ", i64 const offset" if member.offset else ""
        offset_expression = "offset + " if member.offset else ""
        if member.optional:
            return f'''\
[[nodiscard]] static anton::Optional<Syntax_Token const&> get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter}) {{
    ANTON_ASSERT(node.type == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    if(node.children.size() > ({offset_expression}{member.index})) {{
        ANTON_ASSERT(node.children[{offset_expression}{member.index}].is_right(), "{member.name} in {syntax_name} is not Syntax_Token");
        return node.children[{offset_expression}{member.index}].right();
    }} else {{
        return anton::null_optional;
    }}
}}'''
        else:
            return f'''\
[[nodiscard]] static Syntax_Token const& get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter}) {{
    ANTON_ASSERT(node.type == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    ANTON_ASSERT(node.children.size() > ({offset_expression}{member.index}), "{syntax_name} has too few children");
    ANTON_ASSERT(node.children[{offset_expression}{member.index}].is_right(), "{member.name} in {syntax_name} is not Syntax_Token");
    return node.children[{offset_expression}{member.index}].right();
}}'''

    if member.lookup == Lookup_Kind.search:
        return create_search_accessor(syntax_name, member)
    else:
        return create_index_accessor(syntax_name, member)

def generate_accessors(parameters):
    for member in parameters["members"]:
        if member.node_kind == Node_Kind.token:
            accessor = format_token_accessor(parameters["syntax_name"], member)
            yield accessor
        else:
            accessor = format_node_accessor(parameters["syntax_name"], member)
            yield accessor

def write_preamble(file):
    preamble = '''\
#include <ast2.hpp>

namespace vush {'''
    file.write(preamble)

def write_epilogue(file):
    file.write("}\n")

def main():
    file = open("./private/syntax_accessors.hpp", "w")

    write_preamble(file)
    for parameters in syntax_nodes:
        for accessor in generate_accessors(parameters):
            file.write("\n")
            file.write(accessor)
            file.write("\n")
    write_epilogue(file)

    file.close()

    process = subprocess.run(["clang-format", "-i", "./private/syntax_accessors.hpp"])
    process.check_returncode()

main()
