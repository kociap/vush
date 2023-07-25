#include <context.hpp>

#include <ast.hpp>
#include <vush/types.hpp>

namespace vush {
  Context::Context(Allocator* const allocator)
    : allocator(allocator), source_registry(allocator), definitions(allocator)
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

  ast::Node const* Context::find_type_definition(anton::String_View const identifier) const
  {
    auto result = type_definitions.find(identifier);
    if(result != type_definitions.end()) {
      return result->value;
    } else {
      return nullptr;
    }
  }

  void Context::add_type_definition(anton::String_View identifier, ast::Node const* node)
  {
    type_definitions.emplace(identifier, node);
  }

  ast::Node const* Context::find_node_definition(ast::Node const* const node) const
  {
    auto result = definitions.find(node);
    if(result != definitions.end()) {
      return result->value;
    } else {
      return nullptr;
    }
  }

  void Context::add_node_definition(ast::Node const* const node, ast::Node const* const definition)
  {
    definitions.emplace(node, definition);
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

  ast::Decl_Function const* Context::find_overload(ast::Node const* const node)
  {
    auto result = overloads.find(node);
    if(result != overloads.end()) {
      return result->value;
    } else {
      return nullptr;
    }
  }

  void Context::add_overload(ast::Node const* node, ast::Decl_Function const* fn)
  {
    overloads.emplace(node, fn);
  }
} // namespace vush
