#pragma once

#include <vush_ast/ast_fwd.hpp>
#include <vush_core/types.hpp>

namespace vush {
  struct Type_Layout {
    // The stride of the base type. If the type is a vector, matrix or array,
    // it's the stride of the elements. Otherwise, 0.
    i32 base_stride;
    // The stride of the type in bytes.
    // 0 if the type has no stride.
    i32 stride;
    // The alignment of the type in bytes.
    // 0 if the type has no alignment.
    i32 alignment;

    //  The number of elements in a vector. 1, 2, 3 or 4. Scalars will always
    //  have 1.
    i8 vector_elements;
    //  The number of columns in a matrix. 1, 2, 3 or 4. Scalars will always
    //  have 1.
    i8 matrix_columns;
  };

  // get_builtin_type_layout
  // Get the type layout of a builtin type. Builtin opaque types (including
  // void) have no type information, hence their properties are reported as 0.
  //
  [[nodiscard]] Type_Layout
  get_builtin_type_layout(ast::Type_Builtin_Kind kind);
} // namespace vush
