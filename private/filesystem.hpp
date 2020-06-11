#pragma once

#include <anton/string.hpp>
#include <anton/string_view.hpp>

namespace vush::fs {
    // concat_paths
    // Concatenate paths with separator.
    //
    anton::String concat_paths(anton::String_View path1, anton::String_View path2);

    bool exists(anton::String_View path);
} // namespace vush::fs
