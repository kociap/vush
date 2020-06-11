#pragma once

#include <anton/string.hpp>
#include <ast.hpp>
#include <owning_ptr.hpp>
#include <vush/expected.hpp>

namespace vush {
    struct Parse_Error {
        anton::String message;
        i64 line = 0;
        i64 column = 0;
        i64 file_offset = 0;
    };

    Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_file(anton::String const& path);
} // namespace vush
