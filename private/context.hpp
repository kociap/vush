#pragma once

#include <anton/array.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>

#include <ast_fwd.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Source_Data {
        anton::String name;
        anton::String data;
    };

    struct Context {
    public:
        source_definition_callback source_definition_cb = nullptr;
        void* source_definition_user_data = nullptr;
        source_import_callback source_import_cb = nullptr;
        void* source_import_user_data = nullptr;
        Diagnostics_Options diagnostics = {};
        Allocator* allocator = nullptr;

    private:
        // Maps source's name to source information.
        anton::Flat_Hash_Map<anton::String_View, Source_Data> source_registry;
        // Maps ast::Node* to its definition node.
        anton::Flat_Hash_Map<ast::Node const*, ast::Node const*> definitions;

    public:
        Context(Allocator* allocator);

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

    };
} // namespace vush
