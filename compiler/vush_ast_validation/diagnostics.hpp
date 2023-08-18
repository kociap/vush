#pragma once

#include <vush_ast/ast_fwd.hpp>
#include <vush_core/source_info.hpp>
#include <vush_diagnostics/error.hpp>

namespace vush {
  struct Context;

  [[nodiscard]] Error err_init_invalid_matrix_initializer_kind(Context const& ctx,
                                                               ast::Initializer const* initializer);

  [[nodiscard]] Error err_init_invalid_struct_initializer_kind(Context const& ctx,
                                                               ast::Initializer const* initializer);
} // namespace vush
