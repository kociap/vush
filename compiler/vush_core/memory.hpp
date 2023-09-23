#pragma once

#include <anton/memory/core.hpp>

#include <vush_core/types.hpp>

namespace vush {
  template<typename T, typename... Args>
  T* allocate(Allocator* allocator, Args&&... args)
  {
    T* const p = static_cast<T*>(allocator->allocate(sizeof(T), alignof(T)));
    anton::construct(p, ANTON_FWD(args)...);
    return p;
  }

  template<typename T>
  void deallocate(Allocator* allocator, T* v)
  {
    allocator->deallocate(v, sizeof(T), alignof(T));
  }

  // align_address
  // Round address up to alignment. If address is already aligned, does not change address.
  //
  // Parameters:
  //   address - address to be aligned.
  // alignment - target alignment. Must be a power of 2.
  //
  // Returns:
  // Address aligned to alignment.
  //
  [[nodiscard]] constexpr u64 align_address(u64 const address, u64 const alignment)
  {
    return (address + (alignment - 1)) & ~(alignment - 1);
  }
} // namespace vush
