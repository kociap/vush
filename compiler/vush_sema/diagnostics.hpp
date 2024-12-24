#pragma once

#include <vush_ast/ast.hpp>
#include <vush_ast/ast_fwd.hpp>
#include <vush_core/source_info.hpp>
#include <vush_diagnostics/error.hpp>

namespace vush {
  struct Context;

  [[nodiscard]] Error err_undefined_symbol(Context const& ctx,
                                           Source_Info const& symbol);
  [[nodiscard]] Error err_symbol_redefinition(Context const& ctx,
                                              Source_Info const& old_symbol,
                                              Source_Info const& new_symbol);

  [[nodiscard]] Error
  err_init_invalid_vector_initializer_kind(Context const& ctx,
                                           ast::Initializer const* initializer);

  [[nodiscard]] Error
  err_init_invalid_matrix_initializer_kind(Context const& ctx,
                                           ast::Initializer const* initializer);

  [[nodiscard]] Error
  err_init_invalid_struct_initializer_kind(Context const& ctx,
                                           ast::Initializer const* initializer);

  [[nodiscard]] inline Error
  err_discard_outside_fragment([[maybe_unused]] Context const& ctx,
                               [[maybe_unused]] Source_Info const& discard)
  {
    return Error{.diagnostic = anton::String("err_discard_outside_fragment")};
  }

  [[nodiscard]] Error
  err_variable_type_unsized_array(Context const& ctx,
                                  ast::Variable const* variable);

  [[nodiscard]] Error err_variable_type_opaque(Context const& ctx,
                                               ast::Variable const* variable);

  [[nodiscard]] Error
  err_opaque_type_non_assignable(Context const& ctx,
                                 ast::Stmt_Assignment const* assignment);

  [[nodiscard]] Error
  err_assignment_to_immutable(Context const& ctx,
                              ast::Stmt_Assignment const* assignment);

  [[nodiscard]] Error err_arithmetic_assignment_to_non_arithmetic_type(
    Context const& ctx, ast::Stmt_Assignment const* assignment);

  // TODO: Implement.
  [[nodiscard]] inline Error
  err_unsized_array_not_allowed([[maybe_unused]] Context const& ctx,
                                [[maybe_unused]] Source_Info const& array)
  {
    return Error{.diagnostic = anton::String("err_unsized_array_not_allowed")};
  }

  [[nodiscard]] inline Error err_identifier_names_a_function_but_is_not_called(
    [[maybe_unused]] Context const& ctx,
    [[maybe_unused]] Source_Info const& identifier)
  {
    return Error{.diagnostic = anton::String(
                   "err_identifier_names_a_function_but_is_not_called")};
  }

  [[nodiscard]] Error err_no_matching_overload(
    Context const& ctx, ast::Expr_Call const* call,
    anton::Slice<ast::Decl_Function const* const> overloads);

  [[nodiscard]] Error err_ambiguous_overload(
    Context const& ctx, ast::Expr_Call const* call,
    anton::Slice<ast::Decl_Function const* const> candidates);

  [[nodiscard]] Error err_cannot_convert_type(Context const& ctx,
                                              Source_Info const& where,
                                              ast::Type const* to,
                                              ast::Type const* from);

  [[nodiscard]] Error
  err_no_assignment_operator(Context const& ctx, ast::Type const* from_type,
                             ast::Type const* to_type,
                             ast::Stmt_Assignment const* assignment);

  [[nodiscard]] Error err_vector_swizzle_invalid(Context const& ctx,
                                                 ast::Identifier const& field);

  [[nodiscard]] Error err_vector_swizzle_overlong(Context const& ctx,
                                                  ast::Type_Builtin const* type,
                                                  ast::Identifier const& field);

  [[nodiscard]] Error
  err_vector_lvalue_swizzle_overlong(Context const& ctx,
                                     ast::Expr_Field const* expr);

  [[nodiscard]] Error
  err_vector_lvalue_swizzle_duplicate_components(Context const& ctx,
                                                 ast::Expr_Field const* expr);

  [[nodiscard]] Error
  err_expr_default_not_lvalue(Context const& ctx,
                              ast::Expr_Default const* expr);

  [[nodiscard]] Error err_lt_bool_not_lvalue(Context const& ctx,
                                             ast::Lt_Bool const* expr);

  [[nodiscard]] Error err_lt_integer_not_lvalue(Context const& ctx,
                                                ast::Lt_Integer const* expr);

  [[nodiscard]] Error err_lt_float_not_lvalue(Context const& ctx,
                                              ast::Lt_Float const* expr);

  [[nodiscard]] Error err_expr_call_not_lvalue(Context const& ctx,
                                               ast::Expr_Call const* expr);

  [[nodiscard]] Error err_expr_init_not_lvalue(Context const& ctx,
                                               ast::Expr_Init const* expr);

  [[nodiscard]] Error err_expr_if_not_lvalue(Context const& ctx,
                                             ast::Expr_If const* expr);

  [[nodiscard]] Error err_unknown_vector_type(Context const& ctx,
                                              ast::Type const* type);

  [[nodiscard]] inline Error err_builtin_type_has_no_member_named(
    [[maybe_unused]] Context const& ctx, [[maybe_unused]] ast::Type const* type,
    [[maybe_unused]] ast::Identifier const& member_identifier)
  {
    return Error{.diagnostic =
                   anton::String("err_builtin_type_has_no_member_named")};
  }

  [[nodiscard]] Error
  err_type_has_no_field_named(Context const& ctx, ast::Type const* type,
                              ast::Identifier const& field_identifier);

  [[nodiscard]] inline Error
  err_image_parameter_not_image(Context const& ctx,
                                ast::Fn_Parameter const& parameter)
  {
    return Error{.diagnostic = anton::String("err_image_parameter_not_image")};
  }

} // namespace vush
