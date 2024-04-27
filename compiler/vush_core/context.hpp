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
    buffer_definition_callback buffer_definition_cb = nullptr;
    void* buffer_definition_user_data = nullptr;
    source_import_callback source_import_cb = nullptr;
    void* source_import_user_data = nullptr;
    Allocator* allocator = nullptr;
    Source_Registry* source_registry = nullptr;
    Diagnostics_Options diagnostics = {};
  };
} // namespace vush
