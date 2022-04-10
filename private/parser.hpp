#pragma once

#include <anton/expected.hpp>
#include <anton/stream.hpp>
#include <anton/string.hpp>
#include <ast.hpp>
#include <owning_ptr.hpp>

namespace vush {
    // parse_source
    // Builds ast from the contents of stream.
    //
    // Parameters:
    // source_name - Name of the source. Must be address-stable and persist for at least as long as the AST.
    // source_code - The source code to be parsed. Must consist of ASCII only. Must be address-stable and
    //               persist for at least as long as the AST.
    //
    anton::Expected<Declaration_List, Error> parse_source(Allocator* allocator, anton::String_View source_name, anton::String_View source_code);

    anton::Expected<Declaration_List, Error> parse_builtin_functions(Allocator* allocator, anton::String_View source_name, anton::String_View source_code);
} // namespace vush
