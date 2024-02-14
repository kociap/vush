#pragma once

#include <anton/string.hpp>

#include <vush_core/types.hpp>

namespace vush {
  struct Error {
    // Name of the source where the error was generated.
    anton::String source;
    // The diagnostic message.
    anton::String diagnostic;
    // A more thorough explanation of the error that contains source code
    // snippets with the exact locations highlighted.
    anton::String extended_diagnostic;
    i64 line;
    i64 column;
    i64 end_line;
    i64 end_column;

    // format
    // Format diagnostic message with source, line and column.
    //   <source>:<line>:<column>: error: <diagnostic>
    // The extended diagnostic will be included if include_extended_diagnostic
    // is true.
    //   <source>:<line>:<column>: error: <diagnostic>
    //   <extended_diagnostic>
    //
    // Parameters:
    //                   allocator - allocator to use for allocating the string.
    // include_extended_diagnostic - whether to include the extended diagnostic
    //                               message.
    //
    // Returns:
    // Formatted diagnostic message.
    //
    [[nodiscard]] anton::String format(Allocator* allocator,
                                       bool include_extended_diagnostic) const;
  };
} // namespace vush
