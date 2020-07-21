#pragma once

#include <anton/expected.hpp>
#include <anton/stream.hpp>
#include <anton/string.hpp>
#include <ast.hpp>
#include <owning_ptr.hpp>

namespace vush {
    struct Parse_Error {
        anton::String message;
        i64 line = 0;
        i64 column = 0;
        i64 stream_offset = 0;
    };

    // parse_source
    // Builds ast from the contents of stream.
    // path must be address-stable
    //
    anton::Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_source(anton::Input_Stream& stream, anton::String_View path);
} // namespace vush
