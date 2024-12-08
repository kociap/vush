#pragma once

#include <anton/string.hpp>
#include <anton/string_view.hpp>

#include <vush_core/types.hpp>

namespace vush {
  struct Source_Data {
    anton::String path;
    anton::String data;
  };

  struct Source_Info {
    Source_Data const* source;
    i32 line = 0;
    i32 column = 0;
    // The offset into the source at which the matched node starts.
    i32 offset = 0;
    // The offset into the source at which the matched node ends.
    i32 end_offset = 0;
  };
} // namespace vush
