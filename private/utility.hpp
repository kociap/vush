#pragma once

#include <anton/string.hpp>
#include <anton/string_view.hpp>

namespace vush {
    [[nodiscard]] inline anton::String build_error_message(anton::String const& message) {
        return u8"error: " + message;
    }

    [[nodiscard]] inline anton::String build_error_message(anton::String const& path, i64 const line, i64 const column, anton::String const& message) {
        return path + u8":" + anton::to_string(line + 1) + u8":" + anton::to_string(column + 1) + u8": error: " + message;
    }
} // namespace vush
