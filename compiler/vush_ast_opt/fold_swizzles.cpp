// This simplification pass simplifies nested field expressions on vector types.
//
//   value.zxy.xzy -> value.zyx
//

#include <anton/string7_view.hpp>

#include <vush_ast/ast.hpp>
#include <vush_ast/visitor.hpp>
#include <vush_core/memory.hpp>

namespace vush {
  struct Fold_Swizzles_Visitor: public ast::Visitor {
  private:
    Allocator* allocator;

  public:
    bool changed = false;

  public:
    Fold_Swizzles_Visitor(Allocator* allocator): allocator(allocator) {}

    [[nodiscard]] virtual ast::Visitor_Status
    visit(ast::Expr_Field* expr) override
    {
      while(true) {
        bool const result = do_fold(expr);
        if(!result) {
          break;
        }

        changed = true;
      }

      return ast::Visitor_Status::e_continue;
    }

  private:
    [[nodiscard]] bool do_fold(ast::Expr_Field* const expr)
    {
      bool const base_is_vector = ast::is_vector(*expr->base->evaluated_type);
      bool const base_is_field_expr =
        expr->base->node_kind == ast::Node_Kind::expr_field;
      if(!base_is_vector || !base_is_field_expr) {
        return false;
      }

      // Remap swizzles.
      auto const base = static_cast<ast::Expr_Field*>(expr->base);
      bool const base_of_base_is_vector =
        ast::is_vector(*base->base->evaluated_type);
      if(!base_of_base_is_vector) {
        return false;
      }
      anton::String* const swizzle =
        VUSH_ALLOCATE(anton::String, allocator, allocator);
      anton::String_View const expr_swizzle = expr->field.value;
      anton::String7_View const base_swizzle{base->field.value.bytes_begin(),
                                             base->field.value.bytes_end()};
      for(char8 const c: expr_swizzle.bytes()) {
        i32 const index = ast::vector_swizzle_char_to_index(c);
        swizzle->append(base_swizzle[index]);
      }

      // Unlink the base expression.
      expr->base = base->base;
      expr->field.value = *swizzle;

      return true;
    }
  };

  bool run_opt_ast_fold_swizzles(Allocator* allocator, ast::Node_List nodes)
  {
    Fold_Swizzles_Visitor visitor(allocator);
    visitor.run(nodes);
    return visitor.changed;
  }
} // namespace vush
