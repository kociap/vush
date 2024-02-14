#pragma once

#include <anton/expected.hpp>
#include <anton/flat_hash_map.hpp>

#include <vush_ast/ast_fwd.hpp>
#include <vush_diagnostics/error.hpp>

namespace vush {
  struct Context;

  [[nodiscard]] anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*>
  run_overload_group_pass(Allocator* allocator, ast::Node_List ast);

  [[nodiscard]] anton::Expected<void, Error>
  run_namebind_pass(Context& ctx, ast::Node_List ast);

  [[nodiscard]] anton::Expected<void, Error>
  run_ast_validation_pass(Context& ctx, ast::Node_List ast);
} // namespace vush
