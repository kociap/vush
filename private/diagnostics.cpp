#include <diagnostics.hpp>

#include <ast.hpp>

namespace vush {
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

    static void print_source_snippet(anton::String& out, anton::String const& source, Source_Info const& src_info) {
        Line_Limits const line = find_line_limits(source, src_info.start_offset);
        anton::String_View const source_bit{source.data() + line.start, source.data() + line.end};
        out += source_bit;
        out += U'\n';
        i64 const padding = src_info.start_offset - line.start;
        i64 const underline = src_info.end_offset - src_info.start_offset;
        print_underline(out, padding, underline);
    }

    static anton::String_View get_source_bit(anton::String const& source, Source_Info const& src_info) {
        anton::String_View const source_bit{source.data() + src_info.start_offset, source.data() + src_info.end_offset};
        return source_bit;
    }

    [[nodiscard]] static anton::String format_diagnostic_location(Source_Info const& info) {
        return anton::String{info.file_path} + u8":" + anton::to_string(info.line + 1) + u8":" + anton::to_string(info.column + 1) + u8": ";
    }

    anton::String format_diagnostic_location(anton::String_View const path, i64 const line, i64 const column) {
        return anton::String{path} + u8":" + anton::to_string(line + 1) + u8":" + anton::to_string(column + 1) + u8": ";
    }

    anton::String format_integer_literal_overflow(Context const& ctx, Source_Info const& integer) {
        anton::String message = format_diagnostic_location(integer);
        message += u8"error: integer literal requires more than 32 bits\n";
        if(ctx.extended_diagnostics) {
            anton::String const& source = ctx.source_registry.find(integer.file_path)->value;
            print_source_snippet(message, source, integer);
        }
        return message;
    }

    anton::String format_undefined_symbol(Context const& ctx, Source_Info const& symbol) {
        anton::String const& source = ctx.source_registry.find(symbol.file_path)->value;
        anton::String message = format_diagnostic_location(symbol);
        message += u8"error: undefined symbol '";
        message += get_source_bit(source, symbol);
        message += u8"'\n";
        if(ctx.extended_diagnostics) {
            print_source_snippet(message, source, symbol);
        }
        return message;
    }

    anton::String format_called_symbol_does_not_name_function(Context const& ctx, Source_Info const& symbol) {
        anton::String const& source = ctx.source_registry.find(symbol.file_path)->value;
        anton::String message = format_diagnostic_location(symbol);
        message += u8"error: called symbol '";
        message += get_source_bit(source, symbol);
        message += u8"' does not name a function\n";
        if(ctx.extended_diagnostics) {
            print_source_snippet(message, source, symbol);
        }
        return message;
    }

    anton::String format_variable_declaration_in_global_scope(Context const& ctx, Source_Info const& declaration) {
        anton::String message = format_diagnostic_location(declaration);
        message += u8"error: illegal declaration of a variable in global scope\n";
        if(ctx.extended_diagnostics) {
            anton::String const& source = ctx.source_registry.find(declaration.file_path)->value;
            print_source_snippet(message, source, declaration);
        }
        return message;
    }

    anton::String format_constant_missing_initializer(Context const& ctx, Source_Info const& constant) {
        anton::String message = format_diagnostic_location(constant);
        message += u8"error: missing constant initializer\n";
        if(ctx.extended_diagnostics) {
            anton::String const& source = ctx.source_registry.find(constant.file_path)->value;
            print_source_snippet(message, source, constant);
        }
        return message;
    }

    anton::String format_expression_not_implicitly_convertible_to_bool(Context const& ctx, Source_Info const& expression) {
        anton::String message = format_diagnostic_location(expression);
        message += u8"error: expression is not implicitly convertible to bool\n";
        if(ctx.extended_diagnostics) {
            anton::String const& source = ctx.source_registry.find(expression.file_path)->value;
            print_source_snippet(message, source, expression);
        }
        return message;
    }

    anton::String format_ordinary_parameter_not_allowed_on_stage(Context const& ctx, Source_Info const& src, Stage_Type const stage) {
        anton::String message = format_diagnostic_location(src);
        message += u8"ordinary parameters are not allowed on ";
        message += stringify(stage);
        message += u8" stage\n";
        if(ctx.extended_diagnostics) {
            anton::String const& source = ctx.source_registry.find(src.file_path)->value;
            print_source_snippet(message, source, src);
        }
        return message;
    }

    anton::String format_vertex_input_not_allowed_on_stage(Context const& ctx, Source_Info const& src, Stage_Type const stage) {
        anton::String message = format_diagnostic_location(src);
        message += u8"vertex input parameters are not allowed on ";
        message += stringify(stage);
        message += u8" stage\n";
        if(ctx.extended_diagnostics) {
            anton::String const& source = ctx.source_registry.find(src.file_path)->value;
            print_source_snippet(message, source, src);
        }
        return message;
    }

    anton::String format_illegal_image_layout_qualifier_on_non_sourced_parameter(Context const& ctx, Source_Info const& qualifier,
                                                                                 Source_Info const& parameter_identifier) {
        anton::String message = format_diagnostic_location(qualifier);
        anton::String const& source = ctx.source_registry.find(qualifier.file_path)->value;
        message += u8"illegal image layout qualifier '";
        message += get_source_bit(source, qualifier);
        message += u8"' on non-sourced parameter '";
        message += get_source_bit(source, parameter_identifier);
        message += u8"'\n";
        if(ctx.extended_diagnostics) {
            // TODO: Underline the qualifier and the parameter?
            print_source_snippet(message, source, qualifier);
        }
        return message;
    }

    anton::String format_illegal_image_layout_qualifier_on_non_image_type(Context const& ctx, Source_Info const& qualifier, Source_Info const& type) {
        anton::String message = format_diagnostic_location(qualifier);
        anton::String const& source = ctx.source_registry.find(qualifier.file_path)->value;
        message += u8"illegal image layout qualifier '";
        message += get_source_bit(source, qualifier);
        message += u8"' on non-image type '";
        message += get_source_bit(source, type);
        message += u8"'\n";
        if(ctx.extended_diagnostics) {
            // TODO: Underline the qualifier and the type?
            print_source_snippet(message, source, qualifier);
        }
        return message;
    }

    anton::String format_duplicate_pass_stage_error(Context const& ctx, Source_Info const& first, Source_Info const& second, anton::String const& pass_name,
                                                    Stage_Type const& stage) {
        anton::String message = format_diagnostic_location(second) + u8"error: duplicate " + stringify(stage) + u8" stage in pass '" + pass_name + "'\n";
        if(ctx.extended_diagnostics) {
            message += format_diagnostic_location(first);
            message += u8"first definition found here:\n";
            message += format_diagnostic_location(second);
            message += u8"second definition found here:\n";
        }
        return message;
    }

    anton::String format_missing_vertex_stage_error([[maybe_unused]] Context const& ctx, anton::String const& pass_name) {
        return u8"error: missing vertex stage in pass '" + pass_name + u8"'";
    }

    anton::String format_vertex_and_compute_stages_error([[maybe_unused]] Context const& ctx, anton::String const& pass_name) {
        return u8"error: pass must have either compute or graphics (vertex, fragment) stages. '" + pass_name + u8"' has both";
    }

    anton::String format_empty_struct([[maybe_unused]] Context const& ctx, Source_Info const& struct_info) {
        anton::String message = format_diagnostic_location(struct_info);
        message += u8"error: empty structs are not allowed";
        return message;
    }

    anton::String format_compute_return_type_must_be_void(Context const& ctx, Source_Info const& return_type) {
        anton::String message = format_diagnostic_location(return_type);
        message += u8"error: the return type of the compute stage must be void\n";
        if(ctx.extended_diagnostics) {
            anton::String const& source = ctx.source_registry.find(return_type.file_path)->value;
            print_source_snippet(message, source, return_type);
        }
        return message;
    }

    anton::String format_source_import_failed(Context const& ctx, Source_Info const& import_info, anton::String_View const source_callback_message) {
        anton::String message = format_diagnostic_location(import_info);
        message += u8"error: source import failed with the following error: ";
        message += source_callback_message;
        if(ctx.extended_diagnostics) {
            anton::String const& source = ctx.source_registry.find(import_info.file_path)->value;
            print_source_snippet(message, source, import_info);
        }
        return message;
    }

    anton::String format_duplicate_sourced_parameter(Context const& ctx, Source_Info const& first, Source_Info const& first_type, Source_Info const& second,
                                                     Source_Info const& second_type) {
        anton::String message = format_diagnostic_location(second);
        message += u8"error: duplicate sourced parameter name with a different type\n";
        if(ctx.extended_diagnostics) {
            anton::String const& first_source = ctx.source_registry.find(first.file_path)->value;
            message += format_diagnostic_location(first);
            message += u8"first definition with type '";
            message += get_source_bit(first_source, first_type);
            message += u8"' found here\n";
            print_source_snippet(message, first_source, first);
            message += '\n';
            anton::String const& second_source = ctx.source_registry.find(second.file_path)->value;
            message += format_diagnostic_location(second);
            message += u8"second definition with type '";
            message += get_source_bit(second_source, second_type);
            message += u8"' found here\n";
            print_source_snippet(message, second_source, second);
        }
        return message;
    }
} // namespace vush
