#pragma once

#include <anton/array.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>

#include <vush.hpp>
#include <vush_ast/ast_fwd.hpp>

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
    anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*> overload_groups;

  private:
    // Maps source's name to source information.
    anton::Flat_Hash_Map<anton::String_View, Source_Data> source_registry;
    // Maps ast::Node* to evaluated expression type.
    anton::Flat_Hash_Map<ast::Node const*, ast::Type const*> types;

  public:
    Context(Allocator* allocator);

    // find_source
    // Find the source data corresponding to given name.
    //
    Source_Data const* find_source(anton::String_View name) const;

    // add_source
    //
    void add_source(Source_Data source);

    // find_node_type
    //
    ast::Type const* find_node_type(ast::Node const* node) const;

    // add_node_type
    //
    void add_node_type(ast::Node const* node, ast::Type const* type);
  };
} // namespace vush
