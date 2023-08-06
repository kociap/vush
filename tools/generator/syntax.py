import subprocess

from node_definitions import syntax_nodes, Node_Kind, Lookup_Kind

def format_node_accessor(syntax_name, member):
    def create_search_accessor(syntax_name, member):
        if member.optional:
            return f"[[nodiscard]] anton::Optional<Syntax_Node const&> get_{syntax_name}_{member.name}(Syntax_Node const& node);", f'''\
anton::Optional<Syntax_Node const&> get_{syntax_name}_{member.name}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    for(SNOT const& snot: node.children) {{
        if(snot.is_left() && snot.left().kind == Syntax_Node_Kind::{member.index}) {{
            return snot.left(){".children[0].left()" if member.unwrap else ""};
        }}
    }}
    return anton::null_optional;
}};'''
        else:
            return f"[[nodiscard]] Syntax_Node const& get_{syntax_name}_{member.name}(Syntax_Node const& node);", f'''\
Syntax_Node const& get_{syntax_name}_{member.name}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    for(SNOT const& snot: node.children) {{
        if(snot.is_left() && snot.left().kind == Syntax_Node_Kind::{member.index}) {{
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
            return f"[[nodiscard]] anton::Optional<Syntax_Node const&> get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter});", f'''\
anton::Optional<Syntax_Node const&> get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter}) {{
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    if(node.children.size() > ({offset_expression}{member.index})) {{
        ANTON_ASSERT(node.children[{offset_expression}{member.index}].is_left(), "{member.name} in {syntax_name} is not Syntax_Node");
        return node.children[{offset_expression}{member.index}].left();
    }} else {{
        return anton::null_optional;
    }}
}}'''
        else:
            return f"[[nodiscard]] Syntax_Node const& get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter});", f'''\
Syntax_Node const& get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter}) {{
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
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
            return f"[[nodiscard]] anton::Optional<Syntax_Token const&> get_{syntax_name}_{member.name}(Syntax_Node const& node);", f'''\
anton::Optional<Syntax_Token const&> get_{syntax_name}_{member.name}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    for(SNOT const& snot: node.children) {{
        if(snot.is_right() && snot.right().kind == Syntax_Node_Kind::{member.index}) {{
            return snot.right();
        }}
    }}
    return anton::null_optional;
}};'''
        else:
            return f"[[nodiscard]] Syntax_Token const& get_{syntax_name}_{member.name}(Syntax_Node const& node);", f'''\
Syntax_Token const& get_{syntax_name}_{member.name}(Syntax_Node const& node) {{
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    for(SNOT const& snot: node.children) {{
        if(snot.is_right() && snot.right().kind == Syntax_Node_Kind::{member.index}) {{
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
            return f"[[nodiscard]] anton::Optional<Syntax_Token const&> get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter});", f'''\
anton::Optional<Syntax_Token const&> get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter}) {{
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
    if(node.children.size() > ({offset_expression}{member.index})) {{
        ANTON_ASSERT(node.children[{offset_expression}{member.index}].is_right(), "{member.name} in {syntax_name} is not Syntax_Token");
        return node.children[{offset_expression}{member.index}].right();
    }} else {{
        return anton::null_optional;
    }}
}}'''
        else:
            return f"[[nodiscard]] Syntax_Token const& get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter});", f'''\
Syntax_Token const& get_{syntax_name}_{member.name}(Syntax_Node const& node {offset_parameter}) {{
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::{syntax_name}, "node is not {syntax_name}");
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

def write_header_preamble(file):
    preamble = '''\
// This file has been autogenerated.
// Do not modify manually.
//
#pragma once

#include <anton/optional.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {'''
    file.write(preamble)

def write_header_epilogue(file):
    file.write("}\n")

def write_source_preamble(file):
    preamble = '''\
// This file has been autogenerated.
// Do not modify manually.
//
#include <anton/optional.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {'''
    file.write(preamble)

def write_source_epilogue(file):
    file.write("}\n")

def main():
    header = open("./compiler/vush_autogen/syntax_accessors.hpp", "w")
    source = open("./compiler/vush_autogen/syntax_accessors.cpp", "w")

    write_header_preamble(header)
    write_source_preamble(source)
    for parameters in syntax_nodes:
        for accessor_header, accessor_source in generate_accessors(parameters):
            header.write("\n")
            header.write(accessor_header)

            source.write("\n")
            source.write(accessor_source)
            source.write("\n")
    write_header_epilogue(header)
    write_source_epilogue(source)

    header.close()
    source.close()

    process = subprocess.run(["clang-format", "-i", "./compiler/vush_autogen/syntax_accessors.hpp", "./compiler/vush_autogen/syntax_accessors.cpp"])
    process.check_returncode()

main()
