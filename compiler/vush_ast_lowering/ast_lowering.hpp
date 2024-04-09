#pragma once

#include <vush_ast/ast_fwd.hpp>
#include <vush_ir/ir_fwd.hpp>

namespace vush {
  [[nodiscard]] Array<ir::Module> lower_ast_to_ir(Allocator* allocator,
                                                  ast::Node_List ast);
}
