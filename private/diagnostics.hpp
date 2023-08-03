#pragma once

#include <anton/string.hpp>
#include <anton/string_view.hpp>

#include <context.hpp>
#include <vush/vush.hpp>

namespace vush {
  [[nodiscard]] inline anton::String build_error_message(anton::String_View const message)
  {
    return anton::String{u8"error: "} + message;
  }

  [[nodiscard]] inline anton::String build_error_message(anton::String_View const& path,
                                                         i64 const line, i64 const column,
                                                         anton::String_View const message)
  {
    return anton::String{path} + u8":" + anton::to_string(line) + u8":" + anton::to_string(column) +
           u8": error: " + message;
  }

  [[deprecated("use err_undefined_symbol instead")]] [[nodiscard]] anton::String
  format_undefined_symbol(Context const& ctx, Source_Info const& symbol);
  [[nodiscard]] Error err_undefined_symbol(Context const& ctx, Source_Info const& symbol);
  [[nodiscard]] Error err_symbol_redefinition(Context const& ctx, Source_Info const& old_symbol,
                                              Source_Info const& new_symbol);
  // TODO: Implement.
  [[nodiscard]] inline Error
  err_called_symbol_does_not_name_function([[maybe_unused]] Context const& ctx,
                                           [[maybe_unused]] Source_Info const& symbol)
  {
    return Error{.diagnostic = anton::String("err_called_symbol_does_not_name_function")};
  }
  [[nodiscard]] Error err_invalid_integer_suffix(Context const& ctx, Source_Info const& source);
  [[nodiscard]] Error err_invalid_float_suffix(Context const& ctx, Source_Info const& source);
  [[nodiscard]] anton::String format_integer_literal_overflow(Context const& ctx,
                                                              Source_Info const& integer);
  [[nodiscard]] anton::String format_integer_literal_leading_zeros(Context const& ctx,
                                                                   Source_Info const& integer);
  [[nodiscard]] Error err_overload_on_return_type(Context const& ctx,
                                                  Source_Info const& identifier1,
                                                  Source_Info const& return1,
                                                  Source_Info const& identifier2,
                                                  Source_Info const& return2);
  [[nodiscard]] anton::String
  format_variable_declaration_in_global_scope(Context const& ctx, Source_Info const& declaration);
  // TODO: Implement.
  [[nodiscard]] inline Error
  err_immutable_variable_missing_initializer([[maybe_unused]] Context const& ctx,
                                             [[maybe_unused]] Source_Info const& constant)
  {
    return Error{.diagnostic = anton::String("err_immutable_variable_missing_initializer")};
  }
  [[nodiscard]] anton::String
  format_expression_not_implicitly_convertible_to_bool(Context const& ctx,
                                                       Source_Info const& expression);
  [[nodiscard]] anton::String format_ordinary_parameter_not_allowed_on_stage(Context const& ctx,
                                                                             Source_Info const& src,
                                                                             Stage_Kind stage);
  [[nodiscard]] anton::String format_vertex_input_not_allowed_on_stage(Context const& ctx,
                                                                       Source_Info const& src,
                                                                       Stage_Kind stage);
  [[nodiscard]] anton::String format_illegal_image_layout_qualifier_on_non_sourced_parameter(
    Context const& ctx, Source_Info const& qualifier, Source_Info const& parameter_identifier);
  [[nodiscard]] anton::String format_illegal_image_layout_qualifier_on_non_image_type(
    Context const& ctx, Source_Info const& qualifier, Source_Info const& type);
  [[nodiscard]] anton::String format_duplicate_pass_stage_error(Context const& ctx,
                                                                Source_Info const& first,
                                                                Source_Info const& second,
                                                                anton::String const& pass_name,
                                                                Stage_Kind const& stage);
  [[nodiscard]] anton::String format_missing_vertex_stage_error(Context const& ctx,
                                                                anton::String const& pass_name);
  [[nodiscard]] anton::String format_graphics_and_compute_stages(Context const& ctx,
                                                                 anton::String const& pass_name);
  [[nodiscard]] Error err_stage_return_must_be_builtin_or_struct(Context const& ctx,
                                                                 anton::String_View pass_name,
                                                                 Source_Info const& stage,
                                                                 Source_Info const& return_type);
  [[nodiscard]] Error err_compute_return_must_be_void(Context const& ctx,
                                                      anton::String_View pass_name,
                                                      Source_Info const& return_type);
  [[nodiscard]] Error err_duplicate_attribute(Context const& ctx, Source_Info const& old_attr,
                                              Source_Info const& new_attr);
  [[nodiscard]] Error err_illegal_attribute(Context const& ctx, Source_Info const& attr);
  [[nodiscard]] Error err_empty_struct(Context const& ctx, Source_Info const& struct_name);
  [[nodiscard]] Error err_duplicate_struct_member(Context const& ctx,
                                                  Source_Info const& first_member_name,
                                                  Source_Info const& second_member_name);
  [[nodiscard]] Error err_opaque_type_in_struct(Context const& ctx, Source_Info const& type);
  [[nodiscard]] Error err_recursive_type_definition(Context const& ctx,
                                                    Source_Info const& struct_name,
                                                    Source_Info const& type);
  [[nodiscard]] Error err_source_import_failed(Context const& ctx, Source_Info const& import_info,
                                               anton::String_View source_callback_message);
  [[nodiscard]] Error
  err_source_import_failed_no_location(Context const& ctx,
                                       anton::String_View source_callback_message);
  [[nodiscard]] anton::String format_duplicate_sourced_parameter(Context const& ctx,
                                                                 Source_Info const& first,
                                                                 Source_Info const& first_type,
                                                                 Source_Info const& second,
                                                                 Source_Info const& second_type);
  // TODO: Implement.
  [[nodiscard]] inline Error err_duplicate_default_label([[maybe_unused]] Context const& ctx,
                                                         [[maybe_unused]] Source_Info const& first,
                                                         [[maybe_unused]] Source_Info const& second)
  {
    return Error{.diagnostic = anton::String("err_duplicate_default_label")};
  }

  [[nodiscard]] Error err_duplicate_label(Context const& ctx, Source_Info const& first,
                                          Source_Info const& second);

  // TODO: Implement.
  [[nodiscard]] inline Error
  err_invalid_switch_arm_expression([[maybe_unused]] Context const& ctx,
                                    [[maybe_unused]] Source_Info const& expression)
  {
    return Error{.diagnostic = anton::String("err_invalid_switch_arm_expression")};
  }

  [[nodiscard]] inline Error
  err_break_used_outside_loop([[maybe_unused]] Context const& ctx,
                              [[maybe_unused]] Source_Info const& statement)
  {
    return Error{.diagnostic = anton::String("err_break_used_outside_loop")};
  }

  [[nodiscard]] inline Error
  err_continue_used_outside_loop([[maybe_unused]] Context const& ctx,
                                 [[maybe_unused]] Source_Info const& statement)
  {
    return Error{.diagnostic = anton::String("err_continue_used_outside_loop")};
  }

  [[nodiscard]] Error err_identifier_is_not_a_constant(Context const& ctx,
                                                       Source_Info const& identifier_source);
  [[nodiscard]] Error err_expression_is_not_constant_evaluable(Context const& ctx,
                                                               Source_Info const& expression);

  [[nodiscard]] inline Error
  err_identifier_names_a_function_but_is_not_called([[maybe_unused]] Context const& ctx,
                                                    [[maybe_unused]] Source_Info const& identifier)
  {
    return Error{.diagnostic = anton::String("err_identifier_names_a_function_but_is_not_called")};
  }

  // TODO: Implement.
  [[nodiscard]] inline Error
  err_unsized_array_not_allowed([[maybe_unused]] Context const& ctx,
                                [[maybe_unused]] Source_Info const& array)
  {
    return Error{.diagnostic = anton::String("err_unsized_array_not_allowed")};
  }
  [[nodiscard]] inline Error
  err_fn_sourced_parameter_not_allowed([[maybe_unused]] Context const& ctx,
                                       [[maybe_unused]] Source_Info const& parameter)
  {
    return Error{.diagnostic = anton::String("err_fn_sourced_parameter_not_allowed")};
  }
  [[nodiscard]] inline Error
  err_stage_parameter_must_be_builtin_or_struct([[maybe_unused]] Context const& ctx,
                                                [[maybe_unused]] Source_Info const& parameter_type)
  {
    return Error{.diagnostic = anton::String("err_stage_parameter_must_be_builtin_or_struct")};
  }
  [[nodiscard]] inline Error
  err_vertex_ordinary_parameter_not_allowed([[maybe_unused]] Context const& ctx,
                                            [[maybe_unused]] Source_Info const& parameter)
  {
    return Error{.diagnostic = anton::String("err_vertex_ordinary_parameter_not_allowed")};
  }
  [[nodiscard]] inline Error
  err_vertex_vin_must_not_be_opaque([[maybe_unused]] Context const& ctx,
                                    [[maybe_unused]] Source_Info const& parameter_type)
  {
    return Error{.diagnostic = anton::String("err_vertex_vin_must_not_be_opaque")};
  }
  [[nodiscard]] inline Error
  err_vertex_vin_must_not_be_array([[maybe_unused]] Context const& ctx,
                                   [[maybe_unused]] Source_Info const& parameter_type)
  {
    return Error{.diagnostic = anton::String("err_vertex_vin_must_not_be_array")};
  }
  [[nodiscard]] inline Error
  err_fragment_ordinary_parameter_not_allowed([[maybe_unused]] Context const& ctx,
                                              [[maybe_unused]] Source_Info const& parameter)
  {
    return Error{.diagnostic = anton::String("err_fragment_ordinary_parameter_not_allowed")};
  }
  [[nodiscard]] inline Error
  err_fragment_vin_not_allowed([[maybe_unused]] Context const& ctx,
                               [[maybe_unused]] Source_Info const& parameter_source)
  {
    return Error{.diagnostic = anton::String("err_fragment_vin_not_allowed")};
  }
  [[nodiscard]] inline Error
  err_compute_ordinary_parameter_not_allowed([[maybe_unused]] Context const& ctx,
                                             [[maybe_unused]] Source_Info const& parameter)
  {
    return Error{.diagnostic = anton::String("err_compute_ordinary_parameter_not_allowed")};
  }
  [[nodiscard]] inline Error
  err_compute_vin_not_allowed_on_stage([[maybe_unused]] Context const& ctx,
                                       [[maybe_unused]] Source_Info const& parameter)
  {
    return Error{.diagnostic = anton::String("err_compute_vin_not_allowed_on_stage")};
  }

  [[nodiscard]] inline Error
  err_ambiguous_overload([[maybe_unused]] Context const& ctx,
                         [[maybe_unused]] ast::Expr_Call const* call,
                         [[maybe_unused]] anton::Slice<ast::Decl_Function const* const> overloads)
  {
    return Error{.diagnostic = anton::String("err_ambiguous_overload")};
  }

  [[nodiscard]] inline Error
  err_no_matching_overload([[maybe_unused]] Context const& ctx,
                           [[maybe_unused]] ast::Expr_Call const* call,
                           [[maybe_unused]] anton::Slice<ast::Decl_Function const* const> overloads)
  {
    return Error{.diagnostic = anton::String("err_no_matching_overload")};
  }

  [[nodiscard]] inline Error err_no_assignment_operator(
    [[maybe_unused]] Context const& ctx, [[maybe_unused]] ast::Type const* from_type,
    [[maybe_unused]] ast::Type const* to_type, [[maybe_unused]] ast::Expr_Assignment const* expr)
  {
    return Error{.diagnostic = anton::String("err_no_assignment_operator")};
  }

  [[nodiscard]] inline Error
  err_condition_not_of_bool_type([[maybe_unused]] Context const& ctx,
                                 [[maybe_unused]] ast::Expr const* condition,
                                 [[maybe_unused]] ast::Type const* condition_type)
  {
    return Error{.diagnostic = anton::String("err_condition_not_of_bool_type")};
  }

  [[nodiscard]] inline Error
  err_switch_expression_not_of_integer_type([[maybe_unused]] Context const& ctx,
                                            [[maybe_unused]] ast::Expr const* expression,
                                            [[maybe_unused]] ast::Type const* expression_type)
  {
    return Error{.diagnostic = anton::String("err_switch_expression_not_of_integer_type")};
  }

  [[nodiscard]] inline Error err_incompatible_if_expression_types(
    [[maybe_unused]] Context const& ctx, [[maybe_unused]] ast::Type const* then_type,
    [[maybe_unused]] ast::Expr const* then_expr, [[maybe_unused]] ast::Type const* else_type,
    [[maybe_unused]] ast::Expr const* else_expr)
  {
    return Error{.diagnostic = anton::String("err_incompatible_if_expression_types")};
  }

  [[nodiscard]] inline Error
  err_expression_is_not_indexable([[maybe_unused]] Context const& ctx,
                                  [[maybe_unused]] ast::Type const* expr_type,
                                  [[maybe_unused]] ast::Expr const* expr)
  {
    return Error{.diagnostic = anton::String("err_expression_is_not_indexable")};
  }

  [[nodiscard]] inline Error
  err_array_index_is_not_integer([[maybe_unused]] Context const& ctx,
                                 [[maybe_unused]] ast::Type const* index_type,
                                 [[maybe_unused]] ast::Expr const* index_expr)
  {
    return Error{.diagnostic = anton::String("err_array_index_is_not_integer")};
  }

  [[nodiscard]] Error err_type_has_no_field_named(Context const& ctx, ast::Type const* type,
                                                  ast::Identifier const* field_identifier);

  [[nodiscard]] inline Error
  err_builtin_type_has_no_member_named([[maybe_unused]] Context const& ctx,
                                       [[maybe_unused]] ast::Type const* type,
                                       [[maybe_unused]] ast::Identifier const* member_identifier)
  {
    return Error{.diagnostic = anton::String("err_builtin_type_has_no_member_named")};
  }

  [[nodiscard]] Error err_vector_swizzle_invalid(Context const& ctx, ast::Identifier const* field);
  [[nodiscard]] Error err_vector_swizzle_overlong(Context const& ctx, ast::Type_Builtin const* type,
                                                  ast::Identifier const* field);
  [[nodiscard]] Error err_unknown_vector_type(Context const& ctx, ast::Type const* type);

  [[nodiscard]] inline Error err_init_type_is_builtin([[maybe_unused]] Context const& ctx,
                                                      [[maybe_unused]] ast::Type const* type)
  {
    return Error{.diagnostic = anton::String("err_init_type_is_builtin")};
  }

  [[nodiscard]] Error err_init_invalid_struct_initializer_kind(Context const& ctx,
                                                               ast::Initializer const* initializer);

  [[nodiscard]] inline Error
  err_init_invalid_array_initializer_kind([[maybe_unused]] Context const& ctx,
                                          [[maybe_unused]] ast::Initializer const* initializer)
  {
    return Error{.diagnostic = anton::String("err_init_invalid_array_initializer_kind")};
  }

  [[nodiscard]] inline Error err_init_array_initialization_must_not_have_both_initializer_kinds(
    [[maybe_unused]] Context const& ctx, [[maybe_unused]] ast::Initializer const* first_initializer,
    [[maybe_unused]] ast::Initializer const* second_initializer)
  {
    return Error{.diagnostic = anton::String(
                   "err_init_array_initialization_must_not_have_both_initializer_kinds")};
  }

  [[nodiscard]] Error err_unimplemented(Context const& ctx, Source_Info const& source);

  [[nodiscard]] inline Error
  err_named_initializer_no_field_named([[maybe_unused]] Context const& ctx,
                                       [[maybe_unused]] ast::Decl_Struct const* decl,
                                       [[maybe_unused]] ast::Named_Initializer const* initializer)
  {
    return Error{.diagnostic = anton::String("err_named_initializer_no_field_named")};
  }

  [[nodiscard]] inline Error err_cannot_convert_type([[maybe_unused]] Context const& ctx,
                                                     [[maybe_unused]] ast::Type const* to,
                                                     [[maybe_unused]] ast::Type const* from)
  {
    return Error{.diagnostic = anton::String("err_cannot_convert_type")};
  }
} // namespace vush
