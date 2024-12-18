#pragma once

#include <anton/expected.hpp>
#include <anton/string_view.hpp>

#include <vush_core/types.hpp>
#include <vush_diagnostics/error.hpp>
#include <vush_lexer/lexer.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {
  struct Context;

  struct Parse_Syntax_Options {
    // include_whitespace_and_comments Whether whitespace and comment tokens
    // should be included in the syntax tree. Those tokens are added to the tree
    // after the entire syntax tree is generated.
    bool include_whitespace_and_comments = false;
  };

  // parse_tokens
  //
  // Builds the syntax tree from a tokenised source code.
  //
  // Parameters:
  //  source - Pointer to the source information of the source being parsed.
  //  tokens - The tokenised source code to be parsed. Must be address-stable
  //           and persist for at least as long as the syntax tree.
  // options - Additional options to use while generating the syntax tree.
  //
  anton::Expected<SNOT*, Error> parse_tokens(Context const& ctx,
                                             Source_Data const* source,
                                             anton::Slice<Token const> tokens,
                                             Parse_Syntax_Options options);
} // namespace vush
