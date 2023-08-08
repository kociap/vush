#pragma once

#include <vush_ast/ast_fwd.hpp>
#include <vush_core/source_info.hpp>
#include <vush_diagnostics/error.hpp>

namespace vush {
  struct Context;

  [[nodiscard]] Error
  err_no_matching_overload(Context const& ctx, ast::Expr_Call const* call,
                           anton::Slice<ast::Decl_Function const* const> overloads);

  [[nodiscard]] Error
  err_ambiguous_overload(Context const& ctx, ast::Expr_Call const* call,
                         anton::Slice<ast::Decl_Function const* const> candidates);

  [[nodiscard]] Error err_cannot_convert_type(Context const& ctx, Source_Info const& where,
                                              ast::Type const* to, ast::Type const* from);
} // namespace vush
