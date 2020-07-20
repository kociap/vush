#pragma once

#include <anton/string.hpp>
#include <anton/string_view.hpp>

namespace vush {
    [[nodiscard]] inline anton::String build_error_message(anton::String_View const message) {
        return anton::String{u8"error: "} + message;
    }

    [[nodiscard]] inline anton::String build_error_message(anton::String_View const& path, i64 const line, i64 const column, anton::String_View const message) {
        return anton::String{path} + u8":" + anton::to_string(line + 1) + u8":" + anton::to_string(column + 1) + u8": error: " + message;
    }
} // namespace vush
