#pragma once

#include <anton/memory/core.hpp>

#include <vush_core/types.hpp>

namespace vush {
#define VUSH_ALLOCATE(T, allocator, ...) \
  (::new(allocator->allocate(sizeof(T), alignof(T))) T{__VA_ARGS__})

  template<typename T>
  void deallocate(Allocator* allocator, T* v)
  {
    allocator->deallocate(v, sizeof(T), alignof(T));
  }
} // namespace vush
