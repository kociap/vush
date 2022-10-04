#pragma once

#include <anton/array.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>

#include <ast_fwd.hpp>
#include <vush/vush.hpp>

namespace vush {
    using Symbol_Kind = ast::Node_Kind;

    struct Symbol {
        Symbol(anton::String_View name, ast::Node const* node);

        anton::String_View get_name() const;
        ast::Node const* get_node() const;
        Symbol_Kind get_kind() const;

    private:
        anton::String_View name;
        ast::Node const* node;
    };

    struct Source_Data {
        anton::String name;
        anton::String data;
    };

    struct Context {
        source_definition_callback source_definition_cb = nullptr;
        void* source_definition_user_data = nullptr;
        source_import_callback source_import_cb = nullptr;
        void* source_import_user_data = nullptr;
        Diagnostics_Options diagnostics = {};
        Allocator* allocator = nullptr;

        Context();

        // find_symbol
        // Looks up a symbol with the given name in scopes starting from the
        // innermost and progressing towards the outermost.
        //
        // Returns:
        // Pointer to the symbol or nullptr if not found.
        // Adding a new symbol to the scope might invalidate the pointer.
        //
        Symbol const* find_symbol(anton::String_View name) const;

        // add_symbol
        // Adds a symbol to the current scope.
        // Adding a symbol might invalidate pointers previously returned by and find_symbol.
        //
        void add_symbol(Symbol symbol);

        // push_scope
        // Add a new scope.
        //
        void push_scope();

        // pop_scope
        // Pops the current scope. Does not pop the global scope.
        //
        void pop_scope();

        // find_source
        //
        Source_Data const* find_source(anton::String_View name) const;

        // add_source
        //
        void add_source(Source_Data source);

        // find_definition
        //
        ast::Node const* find_definition(ast::Node const* node) const;

        // add_definition
        //
        void add_definition(ast::Node const* node, ast::Node const* definition);

    private:
        // Maps source's name to source information.
        anton::Flat_Hash_Map<anton::String_View, Source_Data> source_registry;
        anton::Array<anton::Flat_Hash_Map<anton::String, Symbol>> symbols;
        // Maps ast::Node* to its definition node.
        anton::Flat_Hash_Map<ast::Node const*, ast::Node const*> definitions;
    };
} // namespace vush
