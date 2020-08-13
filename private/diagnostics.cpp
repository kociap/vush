#include <diagnostics.hpp>

#include <ast.hpp>

namespace vush {
    anton::String format_diagnostic_location(anton::String_View const path, i64 const line, i64 const column) {
        return anton::String{path} + u8":" + anton::to_string(line + 1) + u8":" + anton::to_string(column + 1) + u8": ";
    }

    anton::String format_duplicate_pass_stage_error(Source_Info const& duplicate, Source_Info const& previous, anton::String const& pass_name,
                                                    Stage_Type const& stage) {
        anton::String message = format_diagnostic_location(duplicate.file_path, duplicate.line, duplicate.column) + u8"error: duplicate " + stringify(stage) +
                                u8" stage in pass '" + pass_name + "'\n";
        message += format_diagnostic_location(previous.file_path, previous.line, previous.column) + u8" note: previous definition is here";
        return message;
    }

    anton::String format_missing_vertex_stage_error(anton::String const& pass_name) {
        return u8"error:  missing vertex stage in pass '" + pass_name + u8"'";
    }

    anton::String format_vertex_and_compute_stages_error(anton::String const& pass_name) {
        return u8"error: pass must have either compute or graphics (vertex, fragment) stages. '" + pass_name + u8"' has both";
    }
} // namespace vush
