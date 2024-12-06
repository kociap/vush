#pragma once

#include <anton/expected.hpp>
#include <anton/optional.hpp>

#include <vush_ast/ast_fwd.hpp>
#include <vush_diagnostics/error.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {
  struct Context;

  // lower_syntax
  //
  [[nodiscard]] anton::Expected<ast::Node_List, Error>
  lower_syntax(Context& ctx, Array<SNOT> const& syntax);
} // namespace vush
