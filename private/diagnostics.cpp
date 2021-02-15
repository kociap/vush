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

    [[nodiscard]] static anton::String format_diagnostic_location(Source_Info const& info) {
        return anton::String{info.file_path} + u8":" + anton::to_string(info.line + 1) + u8":" + anton::to_string(info.column + 1) + u8": ";
    }

    anton::String format_diagnostic_location(anton::String_View const path, i64 const line, i64 const column) {
        return anton::String{path} + u8":" + anton::to_string(line + 1) + u8":" + anton::to_string(column + 1) + u8": ";
    }

    anton::String format_integer_literal_overflow(Context const& ctx, Source_Info const& integer) {
        anton::String message = format_diagnostic_location(integer);
        message += u8"error: integer literal requires more than 32 bits\n";

        auto iter = ctx.source_registry.find(integer.file_path);
        anton::String const& source = iter->value;
        Line_Limits const line = find_line_limits(source, integer.start_offset);
        anton::String_View const source_bit{source.data() + line.start, source.data() + line.end};
        message += source_bit;
        message += U'\n';
        i64 const padding = integer.start_offset - line.start;
        i64 const underline = integer.end_offset - integer.start_offset;
        print_underline(message, padding, underline);
        return message;
    }

    anton::String format_undefined_symbol(Context const& ctx, Source_Info const& symbol) {
        auto iter = ctx.source_registry.find(symbol.file_path);
        anton::String const& source = iter->value;
        anton::String_View const symbol_name{source.data() + symbol.start_offset, source.data() + symbol.end_offset};

        anton::String message = format_diagnostic_location(symbol);
        message += u8"error: undefined symbol '";
        message += symbol_name;
        message += u8"'\n";

        Line_Limits const line = find_line_limits(source, symbol.start_offset);
        anton::String_View const source_bit{source.data() + line.start, source.data() + line.end};
        message += source_bit;
        message += U'\n';
        i64 const padding = symbol.start_offset - line.start;
        i64 const underline = symbol.end_offset - symbol.start_offset;
        print_underline(message, padding, underline);
        return message;
    }

    anton::String format_called_symbol_does_not_name_function(Context const& ctx, Source_Info const& symbol) {
        auto iter = ctx.source_registry.find(symbol.file_path);
        anton::String const& source = iter->value;
        anton::String_View const symbol_name{source.data() + symbol.start_offset, source.data() + symbol.end_offset};

        anton::String message = format_diagnostic_location(symbol);
        message += u8"error: called symbol '";
        message += symbol_name;
        message += u8"' does not name a function\n";

        Line_Limits const line = find_line_limits(source, symbol.start_offset);
        anton::String_View const source_bit{source.data() + line.start, source.data() + line.end};
        message += source_bit;
        message += U'\n';
        i64 const padding = symbol.start_offset - line.start;
        i64 const underline = symbol.end_offset - symbol.start_offset;
        print_underline(message, padding, underline);
        return message;
    }

    anton::String format_duplicate_pass_stage_error(Source_Info const& duplicate, Source_Info const& previous, anton::String const& pass_name,
                                                    Stage_Type const& stage) {
        anton::String message = format_diagnostic_location(duplicate) + u8"error: duplicate " + stringify(stage) + u8" stage in pass '" + pass_name + "'\n";
        message += format_diagnostic_location(previous) + u8" note: previous definition is here";
        return message;
    }

    anton::String format_missing_vertex_stage_error(anton::String const& pass_name) {
        return u8"error: missing vertex stage in pass '" + pass_name + u8"'";
    }

    anton::String format_vertex_and_compute_stages_error(anton::String const& pass_name) {
        return u8"error: pass must have either compute or graphics (vertex, fragment) stages. '" + pass_name + u8"' has both";
    }

    anton::String format_empty_struct(Source_Info const& struct_info) {
        anton::String message = format_diagnostic_location(struct_info);
        message += u8"error: empty structs are not allowed";
        return message;
    }

    anton::String format_compute_return_type_must_be_void(Context const& ctx, Source_Info const& return_type) {
        anton::String message = format_diagnostic_location(return_type);
        message += u8"error: the return type of compute stage must be void\n";

        auto iter = ctx.source_registry.find(return_type.file_path);
        anton::String const& source = iter->value;
        Line_Limits const line = find_line_limits(source, return_type.start_offset);
        anton::String_View const source_bit{source.data() + line.start, source.data() + line.end};
        message += source_bit;
        message += U'\n';
        i64 const padding = return_type.start_offset - line.start;
        i64 const underline = return_type.end_offset - return_type.start_offset;
        print_underline(message, padding, underline);
        return message;
    }

    anton::String format_sourced_global_pass_does_not_exist(Sourced_Global_Decl const& global) {
        anton::String message = format_diagnostic_location(global.source_info);
        message += u8"error: global sourced from pass named '" + global.pass_name->value + "' that does not exist";
        return message;
    }

    anton::String format_source_import_failed(Context const& ctx, Import_Decl const& import_decl, anton::String_View const source_callback_message) {
        Source_Info const& string_src_info = import_decl.path->source_info;
        anton::String message = format_diagnostic_location(string_src_info);
        message += u8"error: source import failed with the following error: ";
        message += source_callback_message;

        auto iter = ctx.source_registry.find(string_src_info.file_path);
        anton::String const& source = iter->value;
        Line_Limits const line = find_line_limits(source, string_src_info.start_offset);
        anton::String_View const source_bit{source.data() + line.start, source.data() + line.end};
        message += U'\n';
        message += source_bit;
        message += U'\n';
        i64 const padding = string_src_info.start_offset - line.start;
        i64 const underline = string_src_info.end_offset - string_src_info.start_offset;
        print_underline(message, padding, underline);
        return message;
    }
} // namespace vush
