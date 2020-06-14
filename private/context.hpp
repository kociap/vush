#pragma once

#include <anton/array.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>
#include <symbol.hpp>

namespace vush {
    struct Context {
        char const* const* import_paths;
        char const* const* import_paths_end;
        anton::String const* current_file;
        anton::Array<anton::Flat_Hash_Map<anton::String, Symbol>> symbols;
    };

    // find_symbol
    // Looks up a symbol with the given name in scopes starting from the innermost and
    // progressing towards the outermost.
    //
    // Returns:
    // Pointer to the symbol or nullptr if not found.
    // Adding a new symbol to scope might invalidate the pointer.
    //
    Symbol* find_symbol(Context& ctx, anton::String_View name);
} // namespace vush