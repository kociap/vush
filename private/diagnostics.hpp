#pragma once

#include <anton/string.hpp>
#include <anton/string_view.hpp>

#include <context.hpp>
#include <vush/vush.hpp>

namespace vush {
    [[nodiscard]] inline anton::String build_error_message(anton::String_View const message) {
        return anton::String{u8"error: "} + message;
    }

    [[nodiscard]] inline anton::String build_error_message(anton::String_View const& path, i64 const line, i64 const column, anton::String_View const message) {
        return anton::String{path} + u8":" + anton::to_string(line) + u8":" + anton::to_string(column) + u8": error: " + message;
    }

    [[deprecated("use err_undefined_symbol instead")]] [[nodiscard]] anton::String format_undefined_symbol(Context const& ctx, Source_Info const& symbol);
    [[nodiscard]] Error err_undefined_symbol(Context const& ctx, Source_Info const& symbol);
    [[nodiscard]] Error err_symbol_redefinition(Context const& ctx, Source_Info const& old_symbol, Source_Info const& new_symbol);
    [[nodiscard]] Error err_called_symbol_does_not_name_function(Context const& ctx, Source_Info const& symbol);
    [[nodiscard]] Error err_invalid_integer_suffix(Context const& ctx, Source_Info const& source);
    [[nodiscard]] Error err_invalid_float_suffix(Context const& ctx, Source_Info const& source);
    [[nodiscard]] anton::String format_integer_literal_overflow(Context const& ctx, Source_Info const& integer);
    [[nodiscard]] anton::String format_integer_literal_leading_zeros(Context const& ctx, Source_Info const& integer);
    [[nodiscard]] Error err_overload_on_return_type(Context const& ctx, Source_Info const& identifier1, Source_Info const& return1,
                                                    Source_Info const& identifier2, Source_Info const& return2);
    [[nodiscard]] anton::String format_variable_declaration_in_global_scope(Context const& ctx, Source_Info const& declaration);
    [[nodiscard]] Error err_immutable_variable_missing_initializer(Context const& ctx, Source_Info const& constant);
    [[nodiscard]] anton::String format_expression_not_implicitly_convertible_to_bool(Context const& ctx, Source_Info const& expression);
    [[nodiscard]] anton::String format_ordinary_parameter_not_allowed_on_stage(Context const& ctx, Source_Info const& src, Stage_Kind stage);
    [[nodiscard]] anton::String format_vertex_input_not_allowed_on_stage(Context const& ctx, Source_Info const& src, Stage_Kind stage);
    [[nodiscard]] anton::String format_illegal_image_layout_qualifier_on_non_sourced_parameter(Context const& ctx, Source_Info const& qualifier,
                                                                                               Source_Info const& parameter_identifier);
    [[nodiscard]] anton::String format_illegal_image_layout_qualifier_on_non_image_type(Context const& ctx, Source_Info const& qualifier,
                                                                                        Source_Info const& type);
    [[nodiscard]] anton::String format_duplicate_pass_stage_error(Context const& ctx, Source_Info const& first, Source_Info const& second,
                                                                  anton::String const& pass_name, Stage_Kind const& stage);
    [[nodiscard]] anton::String format_missing_vertex_stage_error(Context const& ctx, anton::String const& pass_name);
    [[nodiscard]] anton::String format_graphics_and_compute_stages(Context const& ctx, anton::String const& pass_name);
    [[nodiscard]] Error err_stage_return_must_be_builtin_or_udt(Context const& ctx, anton::String_View pass_name, Source_Info const& stage,
                                                                Source_Info const& return_type);
    [[nodiscard]] Error err_compute_return_must_be_void(Context const& ctx, anton::String_View pass_name, Source_Info const& return_type);
    [[nodiscard]] Error err_duplicate_attribute(Context const& ctx, Source_Info const& old_attr, Source_Info const& new_attr);
    [[nodiscard]] Error err_illegal_attribute(Context const& ctx, Source_Info const& attr);
    [[nodiscard]] Error err_empty_struct(Context const& ctx, Source_Info const& struct_name);
    [[nodiscard]] Error err_duplicate_struct_member(Context const& ctx, Source_Info const& first_member_name, Source_Info const& second_member_name);
    [[nodiscard]] Error err_opaque_type_in_struct(Context const& ctx, Source_Info const& type);
    [[nodiscard]] Error err_recursive_type_definition(Context const& ctx, Source_Info const& struct_name, Source_Info const& type);
    [[nodiscard]] Error err_source_import_failed(Context const& ctx, Source_Info const& import_info, anton::String_View source_callback_message);
    [[nodiscard]] Error err_source_import_failed_no_location(Context const& ctx, anton::String_View source_callback_message);
    [[nodiscard]] anton::String format_duplicate_sourced_parameter(Context const& ctx, Source_Info const& first, Source_Info const& first_type,
                                                                   Source_Info const& second, Source_Info const& second_type);
    [[nodiscard]] Error err_duplicate_default_label(Context const& ctx, Source_Info const& first, Source_Info const& second);
    [[nodiscard]] Error err_duplicate_label(Context const& ctx, Source_Info const& first, Source_Info const& second);
    [[nodiscard]] Error err_invalid_switch_arm_expression(Context const& ctx, Source_Info const& expression);

    [[nodiscard]] Error err_identifier_is_not_a_constant(Context const& ctx, Source_Info const& identifier_source);
    [[nodiscard]] Error err_expression_is_not_constant_evaluable(Context const& ctx, Source_Info const& expression);
    // TODO: Implement.
    [[nodiscard]] Error err_unsized_array_not_allowed(Context const& ctx, Source_Info const& array);
    [[nodiscard]] Error err_fn_sourced_parameter_not_allowed(Context const& ctx, Source_Info const& parameter);
    [[nodiscard]] Error err_stage_parameter_must_be_builtin_or_udt(Context const& ctx, Source_Info const& parameter_type);
    [[nodiscard]] Error err_vertex_ordinary_parameter_not_allowed(Context const& ctx, Source_Info const& parameter);
    [[nodiscard]] Error err_vertex_vin_must_not_be_opaque(Context const& ctx, Source_Info const& parameter_type);
    [[nodiscard]] Error err_vertex_vin_must_not_be_array(Context const& ctx, Source_Info const& parameter_type);
    [[nodiscard]] Error err_fragment_ordinary_parameter_not_allowed(Context const& ctx, Source_Info const& parameter);
    [[nodiscard]] Error err_fragment_vin_not_allowed(Context const& ctx, Source_Info const& parameter_source);
    [[nodiscard]] Error err_compute_ordinary_parameter_not_allowed(Context const& ctx, Source_Info const& parameter);
    [[nodiscard]] Error err_compute_vin_not_allowed_on_stage(Context const& ctx, Source_Info const& parameter);
} // namespace vush
