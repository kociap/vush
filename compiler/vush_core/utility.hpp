#pragma once

#include <anton/assert.hpp>
#include <anton/type_traits/transformations.hpp>

namespace vush {
  template<typename T, typename V>
  T safe_cast(V&& value)
  {
    ANTON_ASSERT(
      instanceof<
        anton::remove_const<anton::remove_reference<anton::remove_pointer<T>>>>(
        value),
      "invalid cast");
    return static_cast<T>(ANTON_FWD(value));
  }
} // namespace vush
