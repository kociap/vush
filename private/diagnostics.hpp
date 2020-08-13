#pragma once

#include <anton/string.hpp>
#include <anton/string_view.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Source_Info;
    enum struct Stage_Type;

    [[nodiscard]] inline anton::String build_error_message(anton::String_View const message) {
        return anton::String{u8"error: "} + message;
    }

    [[nodiscard]] inline anton::String build_error_message(anton::String_View const& path, i64 const line, i64 const column, anton::String_View const message) {
        return anton::String{path} + u8":" + anton::to_string(line + 1) + u8":" + anton::to_string(column + 1) + u8": error: " + message;
    }

    [[nodiscard]] anton::String format_diagnostic_location(anton::String_View path, i64 line, i64 column);

    [[nodiscard]] anton::String format_duplicate_pass_stage_error(Source_Info const& duplicate, Source_Info const& previous, anton::String const& pass_name,
                                                                  Stage_Type const& stage);
    [[nodiscard]] anton::String format_missing_vertex_stage_error(anton::String const& pass_name);
    [[nodiscard]] anton::String format_vertex_and_compute_stages_error(anton::String const& pass_name);
} // namespace vush
