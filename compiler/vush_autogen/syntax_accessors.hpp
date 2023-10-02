// This file has been autogenerated.
// Do not modify manually.
//
#pragma once

#include <anton/optional.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {
  [[nodiscard]] anton::Optional<Syntax_Token const&> get_type_named_mut(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_type_named_value(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Token const&> get_type_array_mut(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_type_array_base(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&> get_type_array_size(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_attribute_identifier(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&>
  get_attribute_parameter_list(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_attribute_parameter_keyed_key(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_attribute_parameter_keyed_value(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const&
  get_attribute_parameter_positional_value(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_variable_type(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_variable_identifier(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&>
  get_variable_initializer(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_if_condition(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_if_then_branch(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&>
  get_decl_if_else_branch(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_import_path(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_decl_struct_identifier(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_struct_members(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_function_attribute_list(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_function_return_type(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_decl_function_identifier(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_function_parameter_list(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_function_body(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_stage_function_attribute_list(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_stage_function_return_type(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_decl_stage_function_pass(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_decl_stage_function_stage(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_stage_function_parameter_list(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_decl_stage_function_body(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_fn_parameter_type(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_fn_parameter_identifier(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Token const&>
  get_fn_parameter_source(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_fn_parameter_if_condition(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_fn_parameter_if_then_branch(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_fn_parameter_if_else_branch(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_struct_member_attribute_list(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_struct_member_type(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_struct_member_identifier(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&>
  get_struct_member_initializer(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_if_condition(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_if_then_branch(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_if_else_branch(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_binary_lhs(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_binary_operator(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_binary_rhs(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_block_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_identifier_value(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_prefix_operator(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_prefix_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_field_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_field_identifier(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_index_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_index_index(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_parentheses_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_field_initializer_identifier(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_field_initializer_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_index_initializer_index(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_index_initializer_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_basic_initializer_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_init_type(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_init_initializers(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_call_identifier(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_expr_call_arguments(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_lt_bool_value(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_lt_integer_value(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Token const&>
  get_expr_lt_integer_suffix(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_lt_float_value(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Token const&>
  get_expr_lt_float_suffix(Syntax_Node const& node);
  [[nodiscard]] Syntax_Token const& get_expr_lt_string_value(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_if_condition(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_if_then_branch(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&>
  get_stmt_if_else_branch(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_switch_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_switch_arm_list(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_switch_arm_body(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_while_condition(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_while_statements(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&> get_stmt_for_variable(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&> get_stmt_for_condition(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&>
  get_stmt_for_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_for_body(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_do_while_body(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_do_while_condition(Syntax_Node const& node);
  [[nodiscard]] anton::Optional<Syntax_Node const&>
  get_stmt_return_expression(Syntax_Node const& node);
  [[nodiscard]] Syntax_Node const& get_stmt_expression_expression(Syntax_Node const& node);
} // namespace vush
