#pragma once

#include <anton/array.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>
#include <owning_ptr.hpp>
#include <symbol.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Context {
        anton::Array<Owning_Ptr<anton::String>> imported_sources;
        anton::Array<anton::Flat_Hash_Map<anton::String, Symbol>> symbols;
        source_request_callback source_request_cb;
        void* source_request_user_data;
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
    Symbol const* find_symbol(Context const& ctx, anton::String_View name);
} // namespace vush