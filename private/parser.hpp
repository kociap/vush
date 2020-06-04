#pragma once

#include <ast.hpp>
#include <owning_ptr.hpp>
#include <string>
#include <vush/expected.hpp>

namespace vush {
    struct Parse_Error {
        std::string message;
        i64 line = 0;
        i64 column = 0;
        i64 file_offset = 0;
    };

    Expected<Owning_Ptr<Declaration_List>, Parse_Error> parse_file(std::string const& path);
} // namespace vush
