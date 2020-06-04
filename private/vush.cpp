#include <vush/vush.hpp>

#include <hierarchy_printer.hpp>
#include <parser.hpp>

#include <iostream>

namespace vush {
    Expected<Compiled_File, String> compile_to_glsl(char const* source_path, char const* const* include_paths, i64 include_paths_count) {
        std::string path = source_path;
        Expected<Owning_Ptr<Syntax_Tree_Node>, Parse_Error> result = parse_file(path);
        if(!result) {
            Parse_Error error = std::move(result.error());
            std::string msg = std::move(path);
            msg += ":";
            msg += std::to_string(error.line + 1);
            msg += ":";
            msg += std::to_string(error.column + 1);
            msg += ": error: ";
            msg += error.message;
            char* str_data = (char*)::operator new(msg.size() + 1);
            memcpy(str_data, msg.data(), msg.size() + 1);
            return {expected_error, str_data, (i64)msg.size()};
        }

        Hierarchy_Printer printer(std::cout);
        result.value()->visit(printer);

        return {expected_value};
    }
} // namespace vush
