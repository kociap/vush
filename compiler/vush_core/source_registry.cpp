#include <vush_core/source_registry.hpp>

namespace vush {
  Source_Registry::Source_Registry(Allocator* allocator): sources(allocator) {}

  Source_Data const*
  Source_Registry::find_source(anton::String_View const name) const
  {
    auto result = sources.find(name);
    if(result != sources.end()) {
      return &result->value;
    } else {
      return nullptr;
    }
  }

  Source_Data const* Source_Registry::add_source(Source_Data source)
  {
    auto result = sources.emplace(source.name, ANTON_MOV(source));
    return &result->value;
  }
} // namespace vush
