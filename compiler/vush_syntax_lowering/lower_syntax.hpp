#pragma once

#include <anton/expected.hpp>
#include <anton/optional.hpp>

#include <vush_ast/fwd.hpp>
#include <vush_diagnostics/error.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {
  struct Context;

  // lower_syntax
  //
  [[nodiscard]] anton::Expected<anton::IList<ast::Node>, Error>
  lower_syntax(Context& ctx, SNOT* syntax);
} // namespace vush
