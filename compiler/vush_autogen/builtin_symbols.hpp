#pragma once

#include <anton/slice.hpp>

#include <vush_ast/ast_fwd.hpp>

namespace vush {
  [[nodiscard]] Array<ast::Decl_Overloaded_Function*>
  get_builtin_functions_declarations(Allocator* allocator);

  // get_builtin_type
  // Get an immutable builtin type.
  //
  [[nodiscard]] ast::Type_Builtin const* get_builtin_type(ast::Type_Builtin_Kind type);
} // namespace vush
