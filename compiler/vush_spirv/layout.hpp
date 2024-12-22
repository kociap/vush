#pragma once

#include <vush_core/types.hpp>
#include <vush_ir/fwd.hpp>

namespace vush {
  struct Interface_Type_Layout {
    i32 alignment;
    i32 size;
  };

  // calculate_base_layout
  //
  // Calculate the base layout of a type as specified in the Vulkan
  // specification 15.8.4. This is equivalent to OpenGL's std430.
  //
  // Parameters:
  // type - must not be void, pointer, sampler, texture or image.
  //
  [[nodiscard]] Interface_Type_Layout
  calculate_base_layout(ir::Type const* type);
} // namespace vush
