#include <vush_ast_transform/transforms.hpp>

#include <anton/flat_hash_map.hpp>

#include <vush_ast/ast.hpp>
#include <vush_autogen/builtin_symbols.hpp>
#include <vush_core/memory.hpp>

namespace vush {
  static void add_overload(
    Allocator* const allocator,
    anton::Flat_Hash_Map<anton::String_View, Array<ast::Decl_Function const*>*>& dictionary,
    ast::Decl_Function const* const fn)
  {
    auto iter = dictionary.find(fn->identifier.value);
    if(iter == dictionary.end()) {
      auto const functions = allocate<Array<ast::Decl_Function const*>>(
        allocator, allocator, anton::variadic_construct, fn);
      dictionary.emplace(fn->identifier.value, functions);
    } else {
      iter->value->push_back(fn);
    }
  }

  [[nodiscard]] static Array<ast::Decl_Overloaded_Function const*>
  construct_overloaded_fns(Context& ctx, ast::Node_List const ast)
  {
    anton::Flat_Hash_Map<anton::String_View, Array<ast::Decl_Function const*>*>
      overloads_dictionary(ctx.allocator);
    // First add builtin functions so that they are reported in diagnostics as the functions defined
    // first.
    for(ast::Decl_Overloaded_Function const* const ofn:
        get_builtin_functions_declarations(ctx.allocator)) {
      for(ast::Decl_Function const* const fn: ofn->overloads) {
        add_overload(ctx.allocator, overloads_dictionary, fn);
      }
    }

    for(ast::Node const* const decl: ast) {
      if(decl->node_kind != ast::Node_Kind::decl_function) {
        continue;
      }

      ast::Decl_Function const* const fn = static_cast<ast::Decl_Function const*>(decl);
      add_overload(ctx.allocator, overloads_dictionary, fn);
    }

    Array<ast::Decl_Overloaded_Function const*> overloaded_fns;
    for(auto& [key, functions]: overloads_dictionary) {
      overloaded_fns.push_back(
        allocate<ast::Decl_Overloaded_Function>(ctx.allocator, key, *functions));
    }
    return overloaded_fns;
  }

  anton::Expected<ast::Node_List, Error> run_ast_construction_pass(Context& ctx,
                                                                   ast::Node_List const ast)
  {
    Array<ast::Decl_Overloaded_Function const*> const overloaded_fns =
      construct_overloaded_fns(ctx, ast);
    // Merge ast and overloaded functions while removing function declarations.
    Array<ast::Node const*>* const result_ast =
      allocate<Array<ast::Node const*>>(ctx.allocator, ctx.allocator);
    for(ast::Node const* const node: overloaded_fns) {
      result_ast->push_back(node);
    }
    for(ast::Node const* const node: ast) {
      if(node->node_kind != ast::Node_Kind::decl_function) {
        result_ast->push_back(node);
      }
    }
    return {anton::expected_value, *result_ast};
  }
} // namespace vush
