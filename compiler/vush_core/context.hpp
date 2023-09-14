#pragma once

#include <anton/array.hpp>
#include <anton/flat_hash_map.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>

#include <vush.hpp>
#include <vush_ast/ast_fwd.hpp>
#include <vush_core/source_registry.hpp>

namespace vush {
  struct Context {
    source_definition_callback source_definition_cb = nullptr;
    void* source_definition_user_data = nullptr;
    source_import_callback source_import_cb = nullptr;
    void* source_import_user_data = nullptr;
    Allocator* allocator = nullptr;
    Source_Registry* source_registry = nullptr;
    // We do not initialize overload_groups with allocator in the constructor since this will be
    // assigned a proper object at a later time.
    anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*> overload_groups;
    Diagnostics_Options diagnostics = {};
  };
} // namespace vush
