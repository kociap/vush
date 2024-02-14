#pragma once

#include <anton/expected.hpp>
#include <anton/optional.hpp>

#include <vush_ast/ast_fwd.hpp>
#include <vush_diagnostics/error.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {
  struct Context;

  // import_source_code
  //
  // Imports, parses and transforms source code into abstract syntax tree (AST).
  // Resolves declaration ifs. Recursively imports sources required by any
  // imported sources and splices them into the AST.
  //
  // Parameters:
  //         ctx - vush context.
  // source_name - name of the source to be imported.
  // source_info - source information of the import declaration.
  //
  // Returns:
  // ast::Node_List containing ast::Node's of the source or Error. The list will
  // be empty if the source has already been imported before.
  //
  [[nodiscard]] anton::Expected<ast::Node_List, Error> import_source_code(
    Context& ctx, anton::String_View const source_name,
    anton::Optional<Source_Info> source_info = anton::null_optional);

  // lower_syntax_to_ast
  //
  [[nodiscard]] anton::Expected<ast::Node_List, Error>
  lower_syntax_to_ast(Context& ctx, Array<SNOT> const& syntax);
} // namespace vush
