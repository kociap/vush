#pragma once

#include <vush_ast/ast.hpp>

namespace vush {
  bool run_opt_ast_fold_swizzles(Allocator* allocator, ast::Node_List nodes);
}
