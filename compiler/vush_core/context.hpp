#pragma once

#include <vush.hpp>
#include <vush_core/source_info.hpp>
#include <vush_core/source_registry.hpp>

namespace vush {
  struct Context {
    Allocator* allocator = nullptr;
    Source_Registry* source_registry = nullptr;
    Diagnostics_Options diagnostics = {};
    buffer_definition_callback buffer_definition_cb = nullptr;
    void* buffer_definition_user_data = nullptr;
    source_query_callback query_source_cb;
    void* query_main_source_user_data;
    void* query_source_user_data;
    source_import_callback import_source_cb;
    void* import_main_source_user_data;
    void* import_source_user_data;
  };

  [[nodiscard]] anton::Expected<anton::String_View, Error>
  import_main_source(Context& ctx, anton::String_View const source_name);

  [[nodiscard]] anton::Expected<anton::String_View, Error>
  import_source(Context& ctx, anton::String_View const source_name,
                Source_Info const& source_info);
} // namespace vush
