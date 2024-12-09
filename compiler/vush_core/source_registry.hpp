#pragma once

#include <anton/flat_hash_map.hpp>
#include <anton/string.hpp>

#include <vush_core/source_info.hpp>
#include <vush_core/types.hpp>

namespace vush {
  struct Source_Registry {
  private:
    // Maps source's name to source information.
    anton::Flat_Hash_Map<anton::String_View, Source_Data*> sources;

  public:
    Source_Registry(Allocator* allocator);

    // find_source
    //
    // Find the source data corresponding to the given name.
    //
    [[nodiscard]] Source_Data const* find_source(anton::String_View name) const;

    // add_source
    //
    Source_Data const* add_source(Source_Data* source);
  };
} // namespace vush
