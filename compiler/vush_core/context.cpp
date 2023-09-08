#include <vush_core/context.hpp>

#include <vush_ast/ast.hpp>

namespace vush {
  Context::Context(Allocator* const allocator): allocator(allocator), types(allocator) {}

  ast::Type const* Context::find_node_type(ast::Node const* node) const
  {
    auto result = types.find(node);
    if(result != types.end()) {
      return result->value;
    } else {
      return nullptr;
    }
  }

  void Context::add_node_type(ast::Node const* node, ast::Type const* type)
  {
    types.emplace(node, type);
  }
} // namespace vush
