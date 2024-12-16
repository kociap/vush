#include <vush_spirv/layout.hpp>

#include <vush_ir/ir.hpp>

namespace vush {
  Interface_Type_Layout calculate_base_layout(ir::Type const* const type)
  {
    switch(type->kind) {
      // TODO: Stride of pointer, sampler types.
    case ir::Type_Kind::e_void:
    case ir::Type_Kind::e_ptr:
    case ir::Type_Kind::e_sampler:
    case ir::Type_Kind::e_image:
    case ir::Type_Kind::e_texture:
      return {0, 0};

    case ir::Type_Kind::e_int8:
    case ir::Type_Kind::e_uint8:
      return {1, 1};

    case ir::Type_Kind::e_int16:
    case ir::Type_Kind::e_uint16:
      return {2, 2};

    // bool is the same size as uint32.
    case ir::Type_Kind::e_bool:
    case ir::Type_Kind::e_int32:
    case ir::Type_Kind::e_uint32:
      return {4, 4};

    case ir::Type_Kind::e_fp16:
      return {2, 2};

    case ir::Type_Kind::e_fp32:
      return {4, 4};

    case ir::Type_Kind::e_fp64:
      return {8, 8};

    case ir::Type_Kind::e_vec: {
      auto const vec = static_cast<ir::Type_Vec const*>(type);
      auto const element = calculate_base_layout(vec->element_type);
      ANTON_ASSERT(vec->rows > 1, "vector must have at least 2 components");
      i32 alignment = element.alignment;
      if(vec->rows == 2) {
        alignment = 2 * alignment;
      } else {
        alignment = 4 * alignment;
      }
      return {
        .alignment = alignment,
        .size = element.size * vec->rows,
      };
    }

    case ir::Type_Kind::e_mat: {
      auto const mat = static_cast<ir::Type_Mat const*>(type);
      auto const element = calculate_base_layout(mat->column_type);
      return {
        .alignment = element.alignment,
        .size = element.size * mat->columns,
      };
    }

    case ir::Type_Kind::e_array: {
      auto const array = static_cast<ir::Type_Array const*>(type);
      auto const element = calculate_base_layout(array->element_type);
      return {
        .alignment = element.alignment,
        .size = element.size * array->size,
      };
    }

    case ir::Type_Kind::e_composite: {
      auto const composite = static_cast<ir::Type_Composite const*>(type);
      // Currently we do not implement types smaller than 4 bytes, hence
      // the least possible alignment of a composite type is 4 bytes.
      i32 largest_alignment = 4;
      i32 offset = 0;
      for(auto const element: composite->elements) {
        auto const layout = calculate_base_layout(element);
        offset = anton::align_address(offset, layout.alignment);
        offset += layout.size;
        largest_alignment =
          anton::math::max(largest_alignment, layout.alignment);
      }
      // Empty composites must occupy at least 1 byte.
      if(offset == 0) {
        offset = 1;
      }
      offset = anton::align_address(offset, largest_alignment);
      return {
        .alignment = largest_alignment,
        .size = offset,
      };
    }
    }
  }
} // namespace vush
