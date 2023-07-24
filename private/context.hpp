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
        // Maps ast::Node* to evaluated expression type.
        anton::Flat_Hash_Map<ast::Node const*, ast::Type const*> types;
        // Maps ast::Node* to evaluated expression type.
        anton::Flat_Hash_Map<ast::Node const*, ast::Decl_Function const*> overloads;
        // Maps type identifiers (string views) to their definitions.
        anton::Flat_Hash_Map<anton::String_View, ast::Node const*> type_definitions;

    public:
        Context(Allocator* allocator);

        // find_source
        // Find the source data corresponding to given name.
        //
        Source_Data const* find_source(anton::String_View name) const;

        // add_source
        //
        void add_source(Source_Data source);

        // find_type_definition
        //
        ast::Node const* find_type_definition(anton::String_View identifier) const;

        // add_type_definition
        //
        void add_type_definition(anton::String_View identifier, ast::Node const* node);

        // find_node_definition
        // Find the definition of a node. The definitions are populated by the
        // defcheck pass and may be one of the following: variable, parameter,
        // overloaded function, struct declaration.
        //
        ast::Node const* find_node_definition(ast::Node const* node) const;

        // add_node_definition
        //
        void add_node_definition(ast::Node const* node, ast::Node const* definition);

        // find_node_type
        //
        ast::Type const* find_node_type(ast::Node const* node) const;

        // add_node_type
        //
        void add_node_type(ast::Node const* node, ast::Type const* type);

        // find_overload
        //
        ast::Decl_Function const* find_overload(ast::Node const* node);

        // add_overload
        //
        void add_overload(ast::Node const* node, ast::Decl_Function const* fn);
    };
} // namespace vush
