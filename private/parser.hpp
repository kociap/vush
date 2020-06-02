#pragma once

#include <ast.hpp>
#include <owning_ptr.hpp>
#include <string>
#include <vush/expected.hpp>

namespace vush {
    struct Parse_Error {
        std::string message;
        i64 line;
        i64 column;
        i64 file_offset;
    };

    Expected<Owning_Ptr<Syntax_Tree_Node>, Parse_Error> parse_file(std::string const& path);
} // namespace vush
