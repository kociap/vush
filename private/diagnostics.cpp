#include <diagnostics.hpp>

#include <ast.hpp>

namespace vush {
    [[nodiscard]] static anton::String format_diagnostic_location(Source_Info const& info) {
        return anton::String{info.file_path} + u8":" + anton::to_string(info.line + 1) + u8":" + anton::to_string(info.column + 1) + u8": ";
    }

    anton::String format_diagnostic_location(anton::String_View const path, i64 const line, i64 const column) {
        return anton::String{path} + u8":" + anton::to_string(line + 1) + u8":" + anton::to_string(column + 1) + u8": ";
    }

    anton::String format_integer_literal_overflow(Source_Info const& integer) {
        anton::String message = format_diagnostic_location(integer) + u8"error: integer literal requires more than 32 bits";
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
} // namespace vush
