#include <vush_core/context.hpp>

#include <vush_ast/ast.hpp>

namespace vush {
  Context::Context(Allocator* const allocator)
    : allocator(allocator), source_registry(allocator), types(allocator)
  {
  }

  Source_Data const* Context::find_source(anton::String_View const name) const
  {
    auto result = source_registry.find(name);
    if(result != source_registry.end()) {
      return &result->value;
    } else {
      return nullptr;
    }
  }

  void Context::add_source(Source_Data source)
  {
    source_registry.emplace(source.name, ANTON_MOV(source));
  }

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
