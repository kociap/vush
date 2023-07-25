#pragma once

#include <anton/memory/core.hpp>
#include <owning_ptr.hpp>
#include <vush/allocator.hpp>

namespace vush {
  template<typename T, typename... Args>
  T* allocate(Allocator* allocator, Args&&... args)
  {
    T* const p = static_cast<T*>(allocator->allocate(sizeof(T), alignof(T)));
    anton::construct(p, ANTON_FWD(args)...);
    return p;
  }

  template<typename T, typename... Args>
  Owning_Ptr<T> allocate_owning(Allocator* allocator, Args&&... args)
  {
    T* const p = static_cast<T*>(allocator->allocate(sizeof(T), alignof(T)));
    anton::construct(p, ANTON_FWD(args)...);
    return Owning_Ptr{p, allocator};
  }

  template<typename T>
  void deallocate(Allocator* allocator, T* v)
  {
    allocator->deallocate(v, sizeof(T), alignof(T));
  }
} // namespace vush
