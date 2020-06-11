#pragma once

#include <anton/flat_hash_map.hpp>
#include <anton/string.hpp>
#include <symbol.hpp>

namespace vush {
    struct Context {
        char const* const* import_paths;
        char const* const* import_paths_end;
        anton::String const* current_file;
        anton::Flat_Hash_Map<anton::String, Symbol> global_symbols;
    };
} // namespace vush