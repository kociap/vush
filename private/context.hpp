#pragma once

#include <symbol.hpp>

#include <string>
#include <unordered_map>

namespace vush {
    struct Context {
        char const* const* import_paths;
        char const* const* import_paths_end;
        std::string const* current_file;
        std::unordered_map<std::string, Symbol> global_symbols;
    };
} // namespace vush