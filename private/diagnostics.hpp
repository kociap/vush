#pragma once

#include <anton/string.hpp>
#include <anton/string_view.hpp>
#include <ast_fwd.hpp>
#include <context.hpp>
#include <vush/vush.hpp>

namespace vush {
    [[nodiscard]] inline anton::String build_error_message(anton::String_View const message) {
        return anton::String{u8"error: "} + message;
    }

    [[nodiscard]] inline anton::String build_error_message(anton::String_View const& path, i64 const line, i64 const column, anton::String_View const message) {
        return anton::String{path} + u8":" + anton::to_string(line) + u8":" + anton::to_string(column) + u8": error: " + message;
    }

    [[nodiscard]] anton::String format_undefined_symbol(Context const& ctx, Source_Info const& symbol);
    [[nodiscard]] anton::String format_called_symbol_does_not_name_function(Context const& ctx, Source_Info const& symbol);
    [[nodiscard]] anton::String format_symbol_redefinition(Context const& ctx, Source_Info const& first, Source_Info const& second);

    [[nodiscard]] Error format_invalid_float_suffix(Context const& ctx, Source_Info const& source);
    [[nodiscard]] anton::String format_integer_literal_overflow(Context const& ctx, Source_Info const& integer);
    [[nodiscard]] anton::String format_integer_literal_leading_zeros(Context const& ctx, Source_Info const& integer);
    [[nodiscard]] anton::String format_overload_identical_parameters_different_return_types(Context const& ctx, Function_Declaration const& overload1,
                                                                                            Function_Declaration const& overload2);
    [[nodiscard]] anton::String format_overload_identical_parameters(Context const& ctx, Function_Declaration const& overload1,
                                                                     Function_Declaration const& overload2);
    [[nodiscard]] anton::String format_variable_declaration_in_global_scope(Context const& ctx, Source_Info const& declaration);
    [[nodiscard]] anton::String format_constant_missing_initializer(Context const& ctx, Source_Info const& constant);
    [[nodiscard]] anton::String format_expression_not_implicitly_convertible_to_bool(Context const& ctx, Source_Info const& expression);
    [[nodiscard]] anton::String format_ordinary_parameter_not_allowed_on_stage(Context const& ctx, Source_Info const& src, Stage_Type stage);
    [[nodiscard]] anton::String format_vertex_input_not_allowed_on_stage(Context const& ctx, Source_Info const& src, Stage_Type stage);
    [[nodiscard]] anton::String format_illegal_image_layout_qualifier_on_non_sourced_parameter(Context const& ctx, Source_Info const& qualifier,
                                                                                               Source_Info const& parameter_identifier);
    [[nodiscard]] anton::String format_illegal_image_layout_qualifier_on_non_image_type(Context const& ctx, Source_Info const& qualifier,
                                                                                        Source_Info const& type);
    [[nodiscard]] anton::String format_duplicate_pass_stage_error(Context const& ctx, Source_Info const& first, Source_Info const& second,
                                                                  anton::String const& pass_name, Stage_Type const& stage);
    [[nodiscard]] anton::String format_missing_vertex_stage_error(Context const& ctx, anton::String const& pass_name);
    [[nodiscard]] anton::String format_graphics_and_compute_stages(Context const& ctx, anton::String const& pass_name);
    [[nodiscard]] anton::String format_stage_return_type_must_be_void_or_udt(Context const& ctx, anton::String_View pass_name, Stage_Type stage,
                                                                             Type const& return_type);
    [[nodiscard]] anton::String format_stage_input_parameter_must_be_udt(Context const& ctx, anton::String_View pass_name, Stage_Type stage,
                                                                         Function_Parameter const& parameter);

    // format_empty_struct
    //
    [[nodiscard]] anton::String format_empty_struct(Context const& ctx, Source_Info const& struct_name);

    [[nodiscard]] anton::String format_compute_return_type_must_be_void(Context const& ctx, Source_Info const& return_type);
    [[nodiscard]] Error format_import_source_failed(Context const& ctx, Source_Info const& import_info, anton::String_View source_callback_message);
    [[nodiscard]] Error format_import_source_failed_no_location(Context const& ctx, anton::String_View source_callback_message);
    [[nodiscard]] anton::String format_duplicate_sourced_parameter(Context const& ctx, Source_Info const& first, Source_Info const& first_type,
                                                                   Source_Info const& second, Source_Info const& second_type);
    [[nodiscard]] anton::String format_duplicate_default_label(Context const& ctx, Source_Info const& first, Source_Info const& second);
    [[nodiscard]] anton::String format_duplicate_label(Context const& ctx, Source_Info const& first, Source_Info const& second);

    [[nodiscard]] Error err_identifier_does_not_name_constant(Context const& ctx, Source_Info const& identifier_source);
    [[nodiscard]] Error err_expression_is_not_constant_evaluable(Context const& ctx, Source_Info const& expression);
} // namespace vush
