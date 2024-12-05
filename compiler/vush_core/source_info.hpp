#pragma once

#include <anton/string_view.hpp>

#include <vush_core/types.hpp>

namespace vush {
  struct Source_Info {
    anton::String_View source_path;
    i64 line = 0;
    i64 column = 0;
    // The offset into the source at which the matched node starts.
    i64 offset = 0;
    // The offset into the source at which the matched node ends.
    i64 end_offset = 0;
  };
} // namespace vush
