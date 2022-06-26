#pragma once

#include <anton/expected.hpp>
#include <anton/stream.hpp>
#include <anton/string.hpp>
#include <ast.hpp>
#include <owning_ptr.hpp>

namespace vush {
    // parse_source
    // Builds ast from source code.
    //
    // Parameters:
    // source_name - Name of the source. Must be address-stable and persist for at least as long as the AST.
    // source_code - The source code to be parsed. Must consist of ASCII only. Must be address-stable and
    //               persist for at least as long as the AST.
    //
    anton::Expected<Declaration_List, Error> parse_source(Allocator* allocator, anton::String_View source_name, anton::String_View source_code);

    // parse_builtin_functions
    // Builds builtin functions ast from source code.
    //
    // Parameters:
    // source_name - Name of the source. Must be address-stable and persist for at least as long as the AST.
    // source_code - The source code to be parsed. Must consist of ASCII only. Must be address-stable and
    //               persist for at least as long as the AST.
    //
    anton::Expected<Declaration_List, Error> parse_builtin_functions(Allocator* allocator, anton::String_View source_name, anton::String_View source_code);

    struct Parse_Syntax_Options {
        // include_whitespace_and_comments
        // Whether whitespace and comment tokens should be included
        // in the syntax tree. Those tokens are added to the tree
        // after the entire syntax tree is generated.
        bool include_whitespace_and_comments = false;
    };

    // parse_source_to_syntax_tree
    // Builds syntax tree from source code.
    //
    // Parameters:
    // source_path - Name of the source. Must be address-stable and persist for at least as long as the syntax tree.
    // source_code - The source code to be parsed. Must consist of ASCII only. Must be address-stable and
    //               persist for at least as long as the syntax tree.
    //     options - additional options to use while generating the syntax tree.
    //
    anton::Expected<Array<SNOT>, Error> parse_source_to_syntax_tree(Allocator* allocator, anton::String_View source_path, anton::String_View source_code,
                                                                    Parse_Syntax_Options options);
} // namespace vush
