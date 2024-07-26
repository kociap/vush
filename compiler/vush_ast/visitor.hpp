#pragma once

#include <vush_ast/ast_fwd.hpp>

namespace vush::ast {
  enum struct Visitor_Status {
    // Continue normally.
    e_continue,
    // Continue from parent, skip siblings.
    e_continue_parent,
    // Immediately stop traversal.
    e_stop,
  };

  struct Visitor {
  public:
    virtual ~Visitor() = default;

    void run(Node_List const& list);

    [[nodiscard]] virtual Visitor_Status visit(Type*);

    [[nodiscard]] virtual Visitor_Status visit(Expr_Init*);
    [[nodiscard]] virtual Visitor_Status visit(Expr_Call*);
    [[nodiscard]] virtual Visitor_Status visit(Expr_If*);
    [[nodiscard]] virtual Visitor_Status visit(Expr_Index*);
    [[nodiscard]] virtual Visitor_Status visit(Expr_Field*);
    [[nodiscard]] virtual Visitor_Status visit(Expr_Identifier*);
    [[nodiscard]] virtual Visitor_Status visit(Lt_Bool*);
    [[nodiscard]] virtual Visitor_Status visit(Lt_Integer*);
    [[nodiscard]] virtual Visitor_Status visit(Lt_Float*);

    [[nodiscard]] virtual Visitor_Status visit(Variable*);

    [[nodiscard]] virtual Visitor_Status visit(Decl_Function*);
    [[nodiscard]] virtual Visitor_Status visit(Decl_Stage_Function*);
    [[nodiscard]] virtual Visitor_Status visit(Decl_Struct*);

  private:
    [[nodiscard]] Visitor_Status traverse_decl(ast::Node*);
    [[nodiscard]] Visitor_Status traverse_stmt(ast::Node*);
    [[nodiscard]] Visitor_Status traverse_expr(ast::Expr*);
  };
} // namespace vush::ast
