#include <vush_ast_passes/passes.hpp>

#include <vush_ast/ast.hpp>
#include <vush_autogen/builtin_symbols.hpp>
#include <vush_core/memory.hpp>

namespace vush {
  anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*>
  run_overload_group_pass(Allocator* allocator, ast::Node_List const ast)
  {
    anton::Flat_Hash_Map<anton::String_View, ast::Overload_Group*> groups =
      get_builtin_functions_declarations(allocator);

    for(ast::Node* const node: ast) {
      if(node->node_kind != ast::Node_Kind::decl_function) {
        continue;
      }

      auto const fn = static_cast<ast::Decl_Function*>(node);
      auto i = groups.find(fn->identifier.value);
      if(i == groups.end()) {
        auto const group = allocate<ast::Overload_Group>(allocator, allocator,
                                                         fn->identifier.value);
        i = groups.emplace(fn->identifier.value, group);
      }

      i->value->overloads.push_back(fn);
    }
    return groups;
  }
} // namespace vush
