#pragma once

#include <anton/expected.hpp>
#include <anton/string.hpp>
#include <ast.hpp>
#include <owning_ptr.hpp>

namespace vush {
    struct Parse_Error {
        anton::String message;
        i64 line = 0;
        i64 column = 0;
        i64 file_offset = 0;
    };

    // parse_file
    // Opens the file unser path for reading and builds ast from its contents.
    // path must be address-stable
    //
    anton::Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_file(anton::String const& path);
} // namespace vush
