#pragma once

#include <anton/expected.hpp>
#include <anton/string_view.hpp>

#include <vush_core/types.hpp>
#include <vush_diagnostics/error.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {
  struct Context;

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
  // source_path - Name of the source. Must be address-stable and persist for at least as long as
  //               the syntax tree.
  // source_code - The source code to be parsed. Must consist of ASCII only. Must be address-stable
  //               and persist for at least as long as the syntax tree.
  //     options - additional options to use while generating the syntax tree.
  //
  anton::Expected<Array<SNOT>, Error> parse_source_to_syntax_tree(Context const& ctx,
                                                                  anton::String_View source_path,
                                                                  anton::String_View source_code,
                                                                  Parse_Syntax_Options options);
} // namespace vush
