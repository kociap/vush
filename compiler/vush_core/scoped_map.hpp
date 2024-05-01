#pragma once

#include <anton/flat_hash_map.hpp>

#include <vush_core/types.hpp>

namespace vush {
  template<typename Key, typename Value>
  struct Scoped_Map {
  private:
    using Entry_Map = anton::Flat_Hash_Map<Key, Value>;

    Array<Entry_Map> scopes;
    Allocator* allocator;

  public:
    Scoped_Map(Allocator* allocator): scopes(allocator), allocator(allocator)
    {
      // Push global scope.
      push_scope();
    }

    // find_entry
    // Looks up an entry with the given key in the scopes starting from the
    // innermost and progressing towards the outermost.
    //
    // Returns:
    // Pointer to the value of the entry or nullptr if not found.
    //
    [[nodiscard]] Value const* find_entry(Key const& name) const
    {
      for(i64 i = scopes.size() - 1; i >= 0; --i) {
        Entry_Map const& map = scopes[i];
        auto result = map.find(name);
        if(result != map.end()) {
          return &result->value;
        }
      }
      return nullptr;
    }

    // add_entry
    // Adds an entry to the current scope. Adding an entry might invalidate
    // pointers previously returned by find_entry.
    //
    Value const* add_entry(Key const& key, Value const& value)
    {
      Entry_Map& map = scopes.back();
      auto iterator = map.emplace(key, value);
      return &iterator->value;
    }

    // push_scope
    // Add a new scope.
    //
    void push_scope()
    {
      scopes.emplace_back(allocator);
    }

    // pop_scope
    // Pops the current scope. Does not pop the global scope.
    //
    void pop_scope()
    {
      if(scopes.size() > 1) {
        scopes.pop_back();
      }
    }
  };
} // namespace vush
