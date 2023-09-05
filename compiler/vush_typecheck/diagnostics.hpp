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

  [[nodiscard]] Error err_vector_swizzle_invalid(Context const& ctx, ast::Identifier const& field);

  [[nodiscard]] Error err_vector_swizzle_overlong(Context const& ctx, ast::Type_Builtin const* type,
                                                  ast::Identifier const& field);

  [[nodiscard]] Error err_unknown_vector_type(Context const& ctx, ast::Type const* type);

  [[nodiscard]] Error err_matrix_field_invalid(Context const& ctx, ast::Identifier const* field);

  [[nodiscard]] inline Error
  err_builtin_type_has_no_member_named([[maybe_unused]] Context const& ctx,
                                       [[maybe_unused]] ast::Type const* type,
                                       [[maybe_unused]] ast::Identifier const& member_identifier)
  {
    return Error{.diagnostic = anton::String("err_builtin_type_has_no_member_named")};
  }

  [[nodiscard]] Error err_type_has_no_field_named(Context const& ctx, ast::Type const* type,
                                                  ast::Identifier const& field_identifier);
} // namespace vush
