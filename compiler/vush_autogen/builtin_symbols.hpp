#pragma once

#include <anton/flat_hash_map.hpp>

#include <vush_ast/ast_fwd.hpp>

namespace vush {
  // get_builtin_functions_declarations
  // Allocate ASTs of builtin functions. The functions are organised into
  // overload groups. Includes both normal functions and operators.
  //
  // IMPORTANT!
  // This function is compiled without optimisations, hence calling it is quite
  // expensive.
  //
  // Returns:
  // A map of names to overload groups.
  //
  [[nodiscard]] anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*>
  get_builtin_functions_declarations(Allocator* allocator);

  // get_builtin_type
  // Get an immutable builtin type.
  //
  [[nodiscard]] ast::Type_Builtin const*
  get_builtin_type(ast::Type_Builtin_Kind type);
} // namespace vush
