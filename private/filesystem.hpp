#pragma once

#include <string>
#include <string_view>

namespace vush::fs {
    // concat_paths
    // Concatenate paths with separator.
    //
    std::string concat_paths(std::string_view path1, std::string_view path2);

    bool exists(std::string_view path);
} // namespace vush::fs
