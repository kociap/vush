#include <vush_core/source_registry.hpp>

namespace vush {
  Source_Registry::Source_Registry(Allocator* allocator): sources(allocator) {}

  Source_Data const* Source_Registry::find_source(anton::String_View const name) const
  {
    auto result = sources.find(name);
    if(result != sources.end()) {
      return &result->value;
    } else {
      return nullptr;
    }
  }

  void Source_Registry::add_source(Source_Data source)
  {
    sources.emplace(source.name, ANTON_MOV(source));
  }
} // namespace vush
