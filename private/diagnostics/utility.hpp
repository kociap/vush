#pragma once

#include <ast_fwd.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Context;

    // error_from_source
    // Create empty Error with source information filled out.
    //
    [[nodiscard]] Error error_from_source(Allocator* allocator, Source_Info const& info);
    [[nodiscard]] Error error_from_source(Allocator* allocator, anton::String_View source_path, i64 line, i64 column);

    // format_diagnostic_location
    // Format the location of the diagnostic into a standardised format.
    //
    [[nodiscard]] anton::String format_diagnostic_location(Allocator* allocator, anton::String_View source_path, i64 line, i64 column);
    [[nodiscard]] anton::String format_diagnostic_location(Allocator* allocator, Source_Info const& info);

    // get_source_bit
    // Extract part of the source.
    //
    [[nodiscard]] anton::String_View get_source_bit(anton::String_View source, i64 offset, i64 end_offset);
    [[nodiscard]] anton::String_View get_source_bit(anton::String_View source, Source_Info const& src_info);

    void print_source_snippet(Context const& ctx, anton::String& out, anton::String_View source, i64 offset, i64 end_offset, i64 line);
    void print_source_snippet(Context const& ctx, anton::String& out, anton::String_View source, Source_Info const& src_info);
} // namespace vush