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

  [[nodiscard]] Error err_no_assignment_operator(Context const& ctx, ast::Type const* from_type,
                                                 ast::Type const* to_type,
                                                 ast::Expr_Assignment const* expr);

  [[nodiscard]] Error err_matrix_field_invalid(Context const& ctx, ast::Identifier const* field);
} // namespace vush
