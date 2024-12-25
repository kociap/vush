#pragma once

#include <anton/expected.hpp>
#include <anton/flat_hash_map.hpp>

#include <vush_ast/fwd.hpp>
#include <vush_core/types.hpp>
#include <vush_diagnostics/error.hpp>

namespace vush {
  struct Context;

  [[nodiscard]] anton::Expected<void, Error> run_sema(Context& ctx,
                                                      ast::Node_List& ast);
} // namespace vush
