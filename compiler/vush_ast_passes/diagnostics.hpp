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

  [[nodiscard]] inline Error
  err_identifier_names_a_function_but_is_not_called([[maybe_unused]] Context const& ctx,
                                                    [[maybe_unused]] Source_Info const& identifier)
  {
    return Error{.diagnostic = anton::String("err_identifier_names_a_function_but_is_not_called")};
  }
} // namespace vush
