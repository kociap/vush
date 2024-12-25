#pragma once

#include <vush_ast/fwd.hpp>
#include <vush_ir/fwd.hpp>

namespace vush {
  [[nodiscard]] Array<ir::Module> lower_ast_to_ir(Allocator* allocator,
                                                  ast::Node_List const& ast);
}
