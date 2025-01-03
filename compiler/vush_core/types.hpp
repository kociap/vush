#pragma once

#include <anton/allocator.hpp>
#include <anton/array.hpp>
#include <anton/types.hpp>

namespace vush {
  using i8 = anton::i8;
  using i16 = anton::i16;
  using i32 = anton::i32;
  using i64 = anton::i64;

  using u8 = anton::u8;
  using u16 = anton::u16;
  using u32 = anton::u32;
  using u64 = anton::u64;

  using f32 = anton::f32;
  using f64 = anton::f64;

  using isize = anton::isize;
  using usize = anton::usize;

  using char8 = anton::char8;
  using char16 = anton::char16;
  using char32 = anton::char32;

  using nullptr_t = anton::nullptr_t;

  using anton::Array;

  using Allocator = anton::Memory_Allocator;

  enum struct Stage_Kind : u8 {
    vertex,
    fragment,
    compute,
  };
} // namespace vush
