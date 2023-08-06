#pragma once

#include <anton/expected.hpp>

#include <vush_ast/ast_fwd.hpp>
#include <vush_core/context.hpp>
#include <vush_diagnostics/error.hpp>

namespace vush {
  struct Context;

  [[nodiscard]] anton::Expected<ast::Node_List, Error>
  run_ast_construction_pass(Context& ctx, ast::Node_List ast);
} // namespace vush
