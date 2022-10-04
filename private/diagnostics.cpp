#include <diagnostics.hpp>

#include <anton/format.hpp>

#include <ast2.hpp>

namespace vush {
    using namespace anton::literals;

    [[nodiscard]] static anton::String format_diagnostic_location(Allocator* const allocator, anton::String_View const source, i64 const line,
                                                                  i64 const column) {
        return anton::concat(allocator, source, u8":"_sv, anton::to_string(allocator, line), u8":"_sv, anton::to_string(allocator, column), u8": "_sv);
    }

    [[nodiscard]] static anton::String format_diagnostic_location(Allocator* const allocator, Source_Info const& info) {
        return format_diagnostic_location(allocator, info.source_path, info.line, info.column);
    }

    anton::String Error::format(Allocator* const allocator, bool const include_extended_diagnostic) const {
        // Add 32 bytes for line and column numbers, colons, spaces and newlines.
        i64 const size = source.size_bytes() + diagnostic.size_bytes() + extended_diagnostic.size_bytes() + 32;
        anton::String error_message{anton::reserve, size, allocator};
        error_message += format_diagnostic_location(allocator, source, line, column);
        error_message += diagnostic;
        if(include_extended_diagnostic && extended_diagnostic.size_bytes() > 0) {
            error_message += extended_diagnostic;
        }
        return error_message;
    }

    [[nodiscard]] static Error error_from_source(Allocator* const allocator, Source_Info const& source) {
        return Error{.line = source.line,
                     .column = source.column,
                     .end_line = source.end_line,
                     .end_column = source.end_column,
                     .source = anton::String(source.source_path, allocator),
                     .diagnostic = anton::String(allocator),
                     .extended_diagnostic = anton::String(allocator)};
    }

    static void print_underline(anton::String& out, i64 const padding, i64 const underline_length) {
        for(i64 i = 0; i < padding; ++i) {
            out += U' ';
        }

        for(i64 i = 0; i < underline_length; ++i) {
            out += U'~';
        }
    }

    struct Line_Limits {
        i64 start;
        i64 end;
    };

    [[nodiscard]] static Line_Limits find_line_limits(anton::String_View const source, i64 const start_pos) {
        Line_Limits limits{start_pos, start_pos};
        char8 const* const begin = source.bytes_begin() - 1;
        char8 const* const end = source.bytes_end();
        // Search backwards
        for(char8 const* data = source.data() + start_pos - 1; data != begin; --data) {
            if(*data == '\n') {
                break;
            }

            limits.start -= 1;
        }
        // Search forward
        for(char8 const* data = source.data() + start_pos + 1; data != end; ++data) {
            if(*data == '\n') {
                break;
            }

            limits.end += 1;
        }

        return limits;
    }

    [[nodiscard]] static i64 calculate_integer_length(i64 integer) {
        i64 length = 0;
        // Account for '-' sign
        if(integer < 0) {
            length = 1;
            integer = -integer;
        }

        do {
            length += 1;
            integer /= 10;
        } while(integer != 0);
        return length;
    }

    static void print_left_margin(Allocator* const allocator, anton::String& out, i64 const width, anton::Optional<i64> const number = anton::null_optional) {
        if(number) {
            out += ' ';
            out += anton::to_string(allocator, number.value());
            out += u8" | "_sv;
        } else {
            for(i64 i = 0; i < width + 1; ++i) {
                out += ' ';
            }
            out += u8" | "_sv;
        }
    }

    static void print_source_snippet(Context const& ctx, anton::String& out, anton::String_View const source, Source_Info const& src_info) {
        Line_Limits const line = find_line_limits(source, src_info.offset);
        anton::String_View const source_bit{source.data() + line.start, source.data() + line.end};
        if(ctx.diagnostics.display_line_numbers) {
            i64 const line_number = src_info.line;
            i64 const line_number_width = calculate_integer_length(line_number);
            print_left_margin(ctx.allocator, out, line_number_width);
            out += '\n';
            print_left_margin(ctx.allocator, out, line_number_width, line_number);
            out += source_bit;
            out += '\n';
            print_left_margin(ctx.allocator, out, line_number_width);
            i64 const padding = src_info.offset - line.start;
            i64 const underline = src_info.end_offset - src_info.offset;
            print_underline(out, padding, underline);
        } else {
            out += source_bit;
            out += U'\n';
            i64 const padding = src_info.offset - line.start;
            i64 const underline = src_info.end_offset - src_info.offset;
            print_underline(out, padding, underline);
        }
    }

    [[nodiscard]] static anton::String_View get_source_bit(anton::String_View const source, Source_Info const& src_info) {
        anton::String_View const source_bit{source.data() + src_info.offset, source.data() + src_info.end_offset};
        return source_bit;
    }

    anton::String format_undefined_symbol(Context const& ctx, Source_Info const& symbol) {
        anton::String_View const source = ctx.find_source(symbol.source_path)->data;
        anton::String message = format_diagnostic_location(ctx.allocator, symbol);
        message += u8"error: undefined symbol '"_sv;
        message += get_source_bit(source, symbol);
        message += u8"'\n"_sv;
        if(ctx.diagnostics.extended) {
            print_source_snippet(ctx, message, source, symbol);
            message += '\n';
        }
        return message;
    }

    Error err_undefined_symbol(Context const& ctx, Source_Info const& symbol) {
        Error error = error_from_source(ctx.allocator, symbol);
        anton::String_View const source = ctx.find_source(symbol.source_path)->data;
        anton::String_View const name = get_source_bit(source, symbol);
        error.diagnostic = anton::format(ctx.allocator, u8"error: undefined symbol '{}'"_sv, name);
        print_source_snippet(ctx, error.extended_diagnostic, source, symbol);
        error.extended_diagnostic += anton::format(ctx.allocator, u8" '{}' used, but not defined"_sv, name);
        return error;
    }

    Error err_symbol_redefinition(Context const& ctx, Source_Info const& old_symbol, Source_Info const& new_symbol) {
        Error error = error_from_source(ctx.allocator, new_symbol);
        anton::String_View const new_source = ctx.find_source(new_symbol.source_path)->data;
        anton::String_View const old_source = ctx.find_source(old_symbol.source_path)->data;
        anton::String_View const name = get_source_bit(new_source, new_symbol);
        error.diagnostic = anton::format(ctx.allocator, u8"error: symbol '{}' is defined multiple times"_sv, name);
        error.extended_diagnostic = format_diagnostic_location(ctx.allocator, old_symbol);
        error.extended_diagnostic += '\n';
        print_source_snippet(ctx, error.extended_diagnostic, old_source, old_symbol);
        error.extended_diagnostic += anton::format(ctx.allocator, u8" definition of '{}' here\n"_sv, name);
        error.extended_diagnostic += format_diagnostic_location(ctx.allocator, new_symbol);
        error.extended_diagnostic += '\n';
        print_source_snippet(ctx, error.extended_diagnostic, new_source, new_symbol);
        error.extended_diagnostic += u8" redefined here"_sv;
        return error;
    }

    // anton::String format_called_symbol_does_not_name_function(Context const& ctx, Source_Info const& symbol) {
    //     anton::String_View const source = ctx.find_source(symbol.source_path)->data;
    //     anton::String message = format_diagnostic_location(ctx.allocator, symbol);
    //     message += u8"error: called symbol '"_sv;
    //     message += get_source_bit(source, symbol);
    //     message += u8"' does not name a function\n"_sv;
    //     if(ctx.diagnostics.extended) {
    //         print_source_snippet(ctx, message, source, symbol);
    //         message += '\n';
    //     }
    //     return message;
    // }

    Error err_invalid_integer_suffix(Context const& ctx, Source_Info const& suffix) {
        Error error = error_from_source(ctx.allocator, suffix);
        anton::String_View const source = ctx.find_source(suffix.source_path)->data;
        error.diagnostic = anton::format(ctx.allocator, "error: invalid integer suffix '{}'"_sv, get_source_bit(source, suffix));
        print_source_snippet(ctx, error.extended_diagnostic, source, suffix);
        error.extended_diagnostic += " valid suffixes are 'u' and 'U'"_sv;
        return error;
    }

    Error err_invalid_float_suffix(Context const& ctx, Source_Info const& suffix) {
        Error error = error_from_source(ctx.allocator, suffix);
        anton::String_View const source = ctx.find_source(suffix.source_path)->data;
        error.diagnostic = anton::format(ctx.allocator, "error: invalid float suffix '{}'"_sv, get_source_bit(source, suffix));
        print_source_snippet(ctx, error.extended_diagnostic, source, suffix);
        error.extended_diagnostic += " valid suffixes are 'd' and 'D'"_sv;
        return error;
    }

    anton::String format_integer_literal_overflow(Context const& ctx, Source_Info const& integer) {
        anton::String message = format_diagnostic_location(ctx.allocator, integer);
        message += u8"error: integer literal requires more than 32 bits\n"_sv;
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.find_source(integer.source_path)->data;
            print_source_snippet(ctx, message, source, integer);
            message += '\n';
        }
        return message;
    }

    anton::String format_integer_literal_leading_zeros(Context const& ctx, Source_Info const& integer) {
        anton::String message = format_diagnostic_location(ctx.allocator, integer);
        message += u8"error: leading zeros in decimal integer literals are not allowed\n"_sv;
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.find_source(integer.source_path)->data;
            print_source_snippet(ctx, message, source, integer);
            message += '\n';
        }
        return message;
    }

    Error err_overload_on_return_type(Context const& ctx, Source_Info const& identifier1, Source_Info const& return1, Source_Info const& identifier2,
                                      Source_Info const& return2) {
        // TODO: Separate error function for user-builtin functions.
        Error error = error_from_source(ctx.allocator, identifier2);
        anton::String_View const source1 = ctx.find_source(identifier1.source_path)->data;
        error.diagnostic = anton::String("error: functions may not be overloaded on their return type alone"_sv, ctx.allocator);
        error.extended_diagnostic = format_diagnostic_location(ctx.allocator, identifier1);
        error.extended_diagnostic += '\n';
        print_source_snippet(ctx, error.extended_diagnostic, source1, identifier1);
        error.extended_diagnostic += anton::format(ctx.allocator, u8"overload with return type '{}' defined here\n"_sv, get_source_bit(source1, return1));
        anton::String_View const source2 = ctx.find_source(identifier2.source_path)->data;
        error.extended_diagnostic += format_diagnostic_location(ctx.allocator, identifier2);
        error.extended_diagnostic += '\n';
        print_source_snippet(ctx, error.extended_diagnostic, source2, identifier2);
        error.extended_diagnostic += anton::format(ctx.allocator, u8"overload with a different return type '{}', but identical parameters defined here\n"_sv,
                                                   get_source_bit(source2, return2));
        return error;
    }

    anton::String format_variable_declaration_in_global_scope(Context const& ctx, Source_Info const& declaration) {
        anton::String message = format_diagnostic_location(ctx.allocator, declaration);
        message += u8"error: illegal declaration of a variable in global scope\n"_sv;
        if(ctx.diagnostics.extended) {
            anton::String_View const& source = ctx.find_source(declaration.source_path)->data;
            print_source_snippet(ctx, message, source, declaration);
            message += '\n';
        }
        return message;
    }

    // Error err_immutable_variable_missing_initializer(Context const& ctx, Source_Info const& constant) {
    //     anton::String message = format_diagnostic_location(ctx.allocator, constant);
    //     message += u8"error: missing constant initializer\n"_sv;
    //     if(ctx.diagnostics.extended) {
    //         anton::String_View const source = ctx.find_source(constant.source_path)->data;
    //         print_source_snippet(ctx, message, source, constant);
    //         message += '\n';
    //     }
    //     return message;
    // }

    anton::String format_expression_not_implicitly_convertible_to_bool(Context const& ctx, Source_Info const& expression) {
        anton::String message = format_diagnostic_location(ctx.allocator, expression);
        message += u8"error: expression is not implicitly convertible to bool\n"_sv;
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.find_source(expression.source_path)->data;
            print_source_snippet(ctx, message, source, expression);
            message += '\n';
        }
        return message;
    }

    anton::String format_illegal_image_layout_qualifier_on_non_sourced_parameter(Context const& ctx, Source_Info const& qualifier,
                                                                                 Source_Info const& parameter_identifier) {
        anton::String message = format_diagnostic_location(ctx.allocator, qualifier);
        anton::String_View const source = ctx.find_source(qualifier.source_path)->data;
        message += u8"illegal image layout qualifier '"_sv;
        message += get_source_bit(source, qualifier);
        message += u8"' on non-sourced parameter '"_sv;
        message += get_source_bit(source, parameter_identifier);
        message += u8"'\n"_sv;
        if(ctx.diagnostics.extended) {
            // TODO: Underline the qualifier and the parameter?
            print_source_snippet(ctx, message, source, qualifier);
            message += '\n';
        }
        return message;
    }

    anton::String format_illegal_image_layout_qualifier_on_non_image_type(Context const& ctx, Source_Info const& qualifier, Source_Info const& type) {
        anton::String message = format_diagnostic_location(ctx.allocator, qualifier);
        anton::String_View const source = ctx.find_source(qualifier.source_path)->data;
        message += u8"illegal image layout qualifier '"_sv;
        message += get_source_bit(source, qualifier);
        message += u8"' on non-image type '"_sv;
        message += get_source_bit(source, type);
        message += u8"'\n"_sv;
        if(ctx.diagnostics.extended) {
            // TODO: Underline the qualifier and the type?
            print_source_snippet(ctx, message, source, qualifier);
            message += '\n';
        }
        return message;
    }

    anton::String format_missing_vertex_stage_error([[maybe_unused]] Context const& ctx, anton::String const& pass_name) {
        return anton::format(ctx.allocator, u8"error: missing vertex stage in pass '{}'\n"_sv, pass_name);
    }

    anton::String format_graphics_and_compute_stages([[maybe_unused]] Context const& ctx, anton::String const& pass_name) {
        return anton::format(ctx.allocator, u8"error: pass must have either compute or graphics (vertex, fragment) stages. '{}' has both\n"_sv, pass_name);
    }

    Error err_stage_return_must_be_builtin_or_udt(Context const& ctx, anton::String_View const pass_name, Source_Info const& stage,
                                                  Source_Info const& return_type) {
        Error error = error_from_source(ctx.allocator, return_type);
        anton::String_View const source = ctx.find_source(return_type.source_path)->data;
        anton::String_View const stage_str = get_source_bit(source, stage);
        error.diagnostic =
            anton::format(ctx.allocator, "error: the return type of the {} stage of '{}' is not a builtin or user defined type"_sv, stage_str, pass_name);
        print_source_snippet(ctx, error.extended_diagnostic, source, return_type);
        error.extended_diagnostic += " return type must be a builtin or user defined type"_sv;
        return error;
    }

    Error err_compute_return_must_be_void(Context const& ctx, anton::String_View const pass_name, Source_Info const& return_type) {
        Error error = error_from_source(ctx.allocator, return_type);
        error.diagnostic += anton::format(ctx.allocator, "error: the return type of the compute stage of '{}' must be void\n"_sv, pass_name);
        anton::String_View const source = ctx.find_source(return_type.source_path)->data;
        print_source_snippet(ctx, error.extended_diagnostic, source, return_type);
        error.extended_diagnostic += " return type must be void"_sv;
        return error;
    }

    Error err_duplicate_attribute(Context const& ctx, Source_Info const& old_attr, Source_Info const& new_attr) {
        Error error = error_from_source(ctx.allocator, new_attr);
        anton::String_View const source = ctx.find_source(new_attr.source_path)->data;
        error.diagnostic = anton::format(ctx.allocator, "error: duplicate attribute '{}'"_sv, get_source_bit(source, new_attr));
        print_source_snippet(ctx, error.extended_diagnostic, source, old_attr);
        error.extended_diagnostic += " attribute first appeared here\n"_sv;
        print_source_snippet(ctx, error.extended_diagnostic, source, new_attr);
        error.extended_diagnostic += " duplicated here"_sv;
        return error;
    }

    Error err_illegal_attribute(Context const& ctx, Source_Info const& attr) {
        Error error = error_from_source(ctx.allocator, attr);
        anton::String_View const source = ctx.find_source(attr.source_path)->data;
        error.diagnostic = anton::format(ctx.allocator, "error: illegal attribute '{}'"_sv, get_source_bit(source, attr));
        print_source_snippet(ctx, error.extended_diagnostic, source, attr);
        error.extended_diagnostic += " attribute not allowed"_sv;
        return error;
    }

    Error err_empty_struct(Context const& ctx, Source_Info const& struct_name) {
        Error error = error_from_source(ctx.allocator, struct_name);
        anton::String_View const source = ctx.find_source(struct_name.source_path)->data;
        anton::String_View const name = get_source_bit(source, struct_name);
        error.diagnostic = anton::format(ctx.allocator, "error: structs must have at least one member, but '{}' is empty"_sv, name);
        print_source_snippet(ctx, error.extended_diagnostic, source, struct_name);
        error.extended_diagnostic += " defined here with an empty body"_sv;
        return error;
    }

    Error err_duplicate_struct_member(Context const& ctx, Source_Info const& first_member_name, Source_Info const& second_member_name) {
        Error error = error_from_source(ctx.allocator, second_member_name);
        anton::String_View const source = ctx.find_source(second_member_name.source_path)->data;
        anton::String_View const name = get_source_bit(source, second_member_name);
        error.diagnostic = anton::format(ctx.allocator, "error: duplicate member '{}'"_sv, name);
        print_source_snippet(ctx, error.extended_diagnostic, source, first_member_name);
        error.extended_diagnostic += " defined here\n"_sv;
        print_source_snippet(ctx, error.extended_diagnostic, source, second_member_name);
        error.extended_diagnostic += " duplicated here"_sv;
        return error;
    }

    Error err_opaque_type_in_struct(Context const& ctx, Source_Info const& type) {
        Error error = error_from_source(ctx.allocator, type);
        anton::String_View const source = ctx.find_source(type.source_path)->data;
        anton::String_View const name = get_source_bit(source, type);
        error.diagnostic = anton::format(ctx.allocator, "error: opaque type '{}' may not be used inside struct"_sv, name);
        print_source_snippet(ctx, error.extended_diagnostic, source, type);
        error.extended_diagnostic += " opaque type used\n"_sv;
        return error;
    }

    Error err_recursive_type_definition(Context const& ctx, Source_Info const& struct_name, Source_Info const& type) {
        Error error = error_from_source(ctx.allocator, type);
        anton::String_View const source = ctx.find_source(struct_name.source_path)->data;
        anton::String_View const name = get_source_bit(source, struct_name);
        error.diagnostic = anton::format(ctx.allocator, "error: recursively defined type '{}'"_sv, name);
        print_source_snippet(ctx, error.extended_diagnostic, source, type);
        error.extended_diagnostic += anton::format(ctx.allocator, " '{}' used within its own definition\n"_sv, name);
        return error;
    }

    Error err_source_import_failed(Context const& ctx, Source_Info const& import_info, anton::String_View const source_callback_message) {
        Error error = error_from_source(ctx.allocator, import_info);
        error.diagnostic = anton::String("error: source import failed with the following error: "_sv, ctx.allocator);
        error.diagnostic += source_callback_message;
        anton::String_View const source = ctx.find_source(import_info.source_path)->data;
        print_source_snippet(ctx, error.extended_diagnostic, source, import_info);
        return error;
    }

    Error err_source_import_failed_no_location(Context const& ctx, anton::String_View const source_callback_message) {
        Error error;
        error.line = 1;
        error.column = 1;
        error.end_line = 1;
        error.end_column = 1;
        error.source = anton::String("<vush>"_sv, ctx.allocator);
        error.diagnostic = anton::String("error: source import failed with the following error: "_sv, ctx.allocator);
        error.diagnostic += source_callback_message;
        error.extended_diagnostic = anton::String(ctx.allocator);
        return error;
    }

    anton::String format_duplicate_sourced_parameter(Context const& ctx, Source_Info const& first, Source_Info const& first_type, Source_Info const& second,
                                                     Source_Info const& second_type) {
        anton::String message = format_diagnostic_location(ctx.allocator, second);
        message += u8"error: duplicate sourced parameter name with a different type\n"_sv;
        if(ctx.diagnostics.extended) {
            anton::String_View const first_source = ctx.find_source(first.source_path)->data;
            message += format_diagnostic_location(ctx.allocator, first);
            message += u8"first definition with type '"_sv;
            message += get_source_bit(first_source, first_type);
            message += u8"' found here\n"_sv;
            print_source_snippet(ctx, message, first_source, first);
            message += '\n';
            anton::String_View const second_source = ctx.find_source(second.source_path)->data;
            message += format_diagnostic_location(ctx.allocator, second);
            message += u8"second definition with type '"_sv;
            message += get_source_bit(second_source, second_type);
            message += u8"' found here\n"_sv;
            print_source_snippet(ctx, message, second_source, second);
            message += '\n';
        }
        return message;
    }

    // Error err_duplicate_default_label(Context const& ctx, Source_Info const& first, Source_Info const& second) {
    //     anton::String message = format_diagnostic_location(ctx.allocator, second);
    //     message += u8"error: duplicate 'default' label in switch statement\n"_sv;
    //     if(ctx.diagnostics.extended) {
    //         anton::String_View const first_source = ctx.find_source(first.source_path)->data;
    //         message += format_diagnostic_location(ctx.allocator, first);
    //         message += u8"first occurence of 'default' found here\n"_sv;
    //         print_source_snippet(ctx, message, first_source, first);
    //         message += '\n';
    //         anton::String_View const second_source = ctx.find_source(second.source_path)->data;
    //         message += format_diagnostic_location(ctx.allocator, second);
    //         message += u8"second occurence of 'default' found here\n"_sv;
    //         print_source_snippet(ctx, message, second_source, second);
    //         message += '\n';
    //     }
    //     return message;
    // }

    // Error err_duplicate_label(Context const& ctx, Source_Info const& first, Source_Info const& second) {
    //     anton::String_View const second_source = ctx.find_source(second.source_path)->data;
    //     anton::String message = format_diagnostic_location(ctx.allocator, second);
    //     message += u8"error: duplicate '"_sv;
    //     message += get_source_bit(second_source, second);
    //     message += u8"' label in switch statement\n"_sv;
    //     if(ctx.diagnostics.extended) {
    //         anton::String_View const first_source = ctx.find_source(first.source_path)->data;
    //         message += format_diagnostic_location(ctx.allocator, first);
    //         message += u8"first occurence of '"_sv;
    //         message += get_source_bit(first_source, first);
    //         message += u8"' found here\n"_sv;
    //         print_source_snippet(ctx, message, first_source, first);
    //         message += '\n';
    //         message += format_diagnostic_location(ctx.allocator, second);
    //         message += u8"second occurence of '"_sv;
    //         message += get_source_bit(second_source, second);
    //         message += u8"' found here\n"_sv;
    //         print_source_snippet(ctx, message, second_source, second);
    //         message += '\n';
    //     }
    //     return message;
    // }

    // Error err_invalid_switch_arm_expression(Context const& ctx, Source_Info const& expression) {}

    Error err_identifier_is_not_a_constant(Context const& ctx, Source_Info const& identifier) {
        Error error = error_from_source(ctx.allocator, identifier);
        anton::String_View const source = ctx.find_source(identifier.source_path)->data;
        anton::String_View const name = get_source_bit(source, identifier);
        error.diagnostic = anton::format(ctx.allocator, "error: '{}' is not a constant"_sv, name);
        print_source_snippet(ctx, error.extended_diagnostic, source, identifier);
        error.extended_diagnostic += anton::format(ctx.allocator, " '{}' is not a constant"_sv, name);
        return error;
    }

    Error err_expression_is_not_constant_evaluable(Context const& ctx, Source_Info const& expression) {
        Error error = error_from_source(ctx.allocator, expression);
        anton::String_View const source = ctx.find_source(expression.source_path)->data;
        error.diagnostic = "error: expression is not constant evaluable'"_sv;
        print_source_snippet(ctx, error.extended_diagnostic, source, expression);
        error.extended_diagnostic += " not constant evaluable"_sv;
        return error;
    }
} // namespace vush
