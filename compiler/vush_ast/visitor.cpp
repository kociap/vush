#include <vush_ast/visitor.hpp>

#include <anton/intrinsics.hpp>

#include <vush_ast/ast.hpp>

namespace vush::ast {
#define CONT_PARENT_TO_CONT(status)                                        \
  status == Visitor_Status::e_continue_parent ? Visitor_Status::e_continue \
                                              : status
#define TRAVERSE_STMT_LIST(list)                      \
  for(auto const node: list) {                        \
    Visitor_Status status = traverse_stmt(node);      \
    if(status == Visitor_Status::e_stop) {            \
      return status;                                  \
    }                                                 \
                                                      \
    if(status == Visitor_Status::e_continue_parent) { \
      break;                                          \
    }                                                 \
  }

#define TRAVERSE_EXPR_LIST(list)                      \
  for(auto const node: list) {                        \
    Visitor_Status status = traverse_expr(node);      \
    if(status == Visitor_Status::e_stop) {            \
      return status;                                  \
    }                                                 \
                                                      \
    if(status == Visitor_Status::e_continue_parent) { \
      break;                                          \
    }                                                 \
  }

#define VISIT_LIST(list)                              \
  for(auto const node: list) {                        \
    Visitor_Status status = visit(node);              \
    if(status == Visitor_Status::e_stop) {            \
      return status;                                  \
    }                                                 \
                                                      \
    if(status == Visitor_Status::e_continue_parent) { \
      break;                                          \
    }                                                 \
  }

  void Visitor::run(Node_List const& list)
  {
    for(Node* const generic_node: list) {
      Visitor_Status const status = traverse_decl(generic_node);
      if(status == Visitor_Status::e_stop) {
        return;
      }
    }
  }

  Visitor_Status Visitor::traverse_decl(ast::Node* const generic_decl)
  {
    switch(generic_decl->node_kind) {
    case Node_Kind::decl_function: {
      auto const node = static_cast<Decl_Function*>(generic_decl);

      Visitor_Status const node_status = visit(node);
      if(node_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(node_status);
      }

      Visitor_Status const return_status = visit(node->return_type);
      if(return_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(return_status);
      }

      for(Fn_Parameter* const parameter: node->parameters) {
        Visitor_Status const status = visit(parameter->type);
        if(status == Visitor_Status::e_stop) {
          return status;
        }

        if(status == Visitor_Status::e_continue_parent) {
          break;
        }
      }

      for(Node* const generic_stmt: node->body) {
        Visitor_Status const status = traverse_stmt(generic_stmt);
        if(status == Visitor_Status::e_stop) {
          return status;
        }

        if(status == Visitor_Status::e_continue_parent) {
          break;
        }
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::decl_stage_function: {
      auto const node = static_cast<Decl_Stage_Function*>(generic_decl);

      Visitor_Status const node_status = visit(node);
      if(node_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(node_status);
      }

      for(Fn_Parameter* const parameter: node->parameters) {
        Visitor_Status const status = visit(parameter->type);
        if(status == Visitor_Status::e_stop) {
          return status;
        }

        if(status == Visitor_Status::e_continue_parent) {
          break;
        }
      }

      TRAVERSE_STMT_LIST(node->body);

      return Visitor_Status::e_continue;
    }

    case Node_Kind::decl_struct: {
      auto const node = static_cast<Decl_Struct*>(generic_decl);
      Visitor_Status const node_status = visit(node);
      if(node_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(node_status);
      }

      for(Struct_Field* const field: node->fields) {
        Visitor_Status const status = visit(field->type);
        if(status == Visitor_Status::e_stop) {
          return status;
        }

        if(status == Visitor_Status::e_continue_parent) {
          break;
        }
      }

      return Visitor_Status::e_continue;
    }

    default:
      return Visitor_Status::e_continue;
    }
  }

  Visitor_Status Visitor::traverse_stmt(ast::Node* const generic_stmt)
  {
    switch(generic_stmt->node_kind) {
    case Node_Kind::variable: {
      auto const stmt = static_cast<Variable*>(generic_stmt);
      Visitor_Status const stmt_status = visit(stmt);
      if(stmt_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(stmt_status);
      }

      Visitor_Status const type_status = visit(stmt->type);
      if(type_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(type_status);
      }

      if(stmt->initializer) {
        Visitor_Status const initializer_status =
          traverse_expr(stmt->initializer);
        return CONT_PARENT_TO_CONT(initializer_status);
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_block: {
      auto const stmt = static_cast<Stmt_Block*>(generic_stmt);
      TRAVERSE_STMT_LIST(stmt->statements);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_assignment: {
      auto const stmt = static_cast<Stmt_Assignment*>(generic_stmt);
      Visitor_Status const lhs_status = traverse_expr(stmt->lhs);
      if(lhs_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(lhs_status);
      }

      Visitor_Status const rhs_status = traverse_expr(stmt->rhs);
      if(rhs_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(rhs_status);
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_if: {
      auto const stmt = static_cast<Stmt_If*>(generic_stmt);
      Visitor_Status const cond_status = traverse_expr(stmt->condition);
      if(cond_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(cond_status);
      }

      TRAVERSE_STMT_LIST(stmt->then_branch);
      TRAVERSE_STMT_LIST(stmt->else_branch);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_switch: {
      auto const stmt = static_cast<Stmt_Switch*>(generic_stmt);
      Visitor_Status const expression_status = traverse_expr(stmt->expression);
      if(expression_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(expression_status);
      }

      for(Switch_Arm* const arm: stmt->arms) {
        TRAVERSE_EXPR_LIST(arm->labels);
        TRAVERSE_STMT_LIST(arm->statements);
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_for: {
      auto const stmt = static_cast<Stmt_For*>(generic_stmt);
      VISIT_LIST(stmt->declarations);

      Visitor_Status const expression_status = traverse_expr(stmt->condition);
      if(expression_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(expression_status);
      }

      TRAVERSE_EXPR_LIST(stmt->actions);
      TRAVERSE_STMT_LIST(stmt->statements);

      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_while: {
      auto const stmt = static_cast<Stmt_While*>(generic_stmt);
      Visitor_Status const expression_status = traverse_expr(stmt->condition);
      if(expression_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(expression_status);
      }

      TRAVERSE_STMT_LIST(stmt->statements);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_do_while: {
      auto const stmt = static_cast<Stmt_Do_While*>(generic_stmt);
      TRAVERSE_STMT_LIST(stmt->statements);
      Visitor_Status const expression_status = traverse_expr(stmt->condition);
      if(expression_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(expression_status);
      }
      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_return: {
      auto const stmt = static_cast<Stmt_Return*>(generic_stmt);
      if(stmt->expression) {
        Visitor_Status const status = traverse_expr(stmt->expression);
        return CONT_PARENT_TO_CONT(status);
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_break: {
      auto const stmt = static_cast<Stmt_Break*>(generic_stmt);
      ANTON_UNUSED(stmt);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_continue: {
      auto const stmt = static_cast<Stmt_Continue*>(generic_stmt);
      ANTON_UNUSED(stmt);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_discard: {
      auto const stmt = static_cast<Stmt_Discard*>(generic_stmt);
      ANTON_UNUSED(stmt);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::stmt_expression: {
      auto const stmt = static_cast<Stmt_Expression*>(generic_stmt);
      Visitor_Status const status = traverse_expr(stmt->expression);
      return CONT_PARENT_TO_CONT(status);
    }

    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  Visitor_Status Visitor::traverse_expr(ast::Expr* const generic_expr)
  {
    switch(generic_expr->node_kind) {
    case Node_Kind::expr_if: {
      auto const expr = static_cast<Expr_If*>(generic_expr);
      Visitor_Status const expr_status = visit(expr);
      if(expr_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(expr_status);
      }

      Visitor_Status const cond_status = traverse_expr(expr->condition);
      if(cond_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(cond_status);
      }

      Visitor_Status const then_status = traverse_expr(expr->then_branch);
      if(then_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(then_status);
      }

      Visitor_Status const else_status = traverse_expr(expr->else_branch);
      if(else_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(else_status);
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::expr_identifier: {
      auto const expr = static_cast<Expr_Identifier*>(generic_expr);
      Visitor_Status const status = visit(expr);
      return CONT_PARENT_TO_CONT(status);
    }

    case Node_Kind::expr_init: {
      auto const expr = static_cast<Expr_Init*>(generic_expr);
      Visitor_Status const type_status = visit(expr->type);
      if(type_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(type_status);
      }

      for(Initializer* const node: expr->initializers) {
        if(node->node_kind == Node_Kind::basic_initializer) {
          auto const initializer = static_cast<Basic_Initializer*>(node);
          Visitor_Status const status = traverse_expr(initializer->expression);
          if(status == Visitor_Status::e_stop) {
            return status;
          }

          if(status == Visitor_Status::e_continue_parent) {
            break;
          }
        } else if(node->node_kind == Node_Kind::field_initializer) {
          auto const initializer = static_cast<Field_Initializer*>(node);
          Visitor_Status const status = traverse_expr(initializer->expression);
          if(status == Visitor_Status::e_stop) {
            return status;
          }

          if(status == Visitor_Status::e_continue_parent) {
            break;
          }
        } else { // Node_Kind::index_initializer
          auto const initializer = static_cast<Index_Initializer*>(node);
          Visitor_Status const status = traverse_expr(initializer->expression);
          if(status == Visitor_Status::e_stop) {
            return status;
          }

          if(status == Visitor_Status::e_continue_parent) {
            break;
          }

          Visitor_Status const index_status = visit(initializer->index);
          if(index_status == Visitor_Status::e_stop) {
            return index_status;
          }

          if(index_status == Visitor_Status::e_continue_parent) {
            break;
          }
        }
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::expr_call: {
      auto const expr = static_cast<Expr_Call*>(generic_expr);
      Visitor_Status const expr_status = visit(expr);
      if(expr_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(expr_status);
      }

      TRAVERSE_EXPR_LIST(expr->arguments);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::expr_field: {
      auto const expr = static_cast<Expr_Field*>(generic_expr);
      Visitor_Status const expr_status = visit(expr);
      if(expr_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(expr_status);
      }

      Visitor_Status const base_status = traverse_expr(expr->base);
      if(base_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(base_status);
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::expr_index: {
      auto const expr = static_cast<Expr_Index*>(generic_expr);
      Visitor_Status const expr_status = visit(expr);
      if(expr_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(expr_status);
      }

      Visitor_Status const base_status = traverse_expr(expr->base);
      if(base_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(base_status);
      }

      Visitor_Status const index_status = traverse_expr(expr->index);
      if(index_status != Visitor_Status::e_continue) {
        return CONT_PARENT_TO_CONT(index_status);
      }

      return Visitor_Status::e_continue;
    }

    case Node_Kind::expr_reinterpret: {
      auto const expr = static_cast<Expr_Reinterpret*>(generic_expr);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::expr_default: {
      auto const expr = static_cast<Expr_Default*>(generic_expr);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::lt_bool: {
      auto const expr = static_cast<Lt_Bool*>(generic_expr);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::lt_integer: {
      auto const expr = static_cast<Lt_Integer*>(generic_expr);
      return Visitor_Status::e_continue;
    }

    case Node_Kind::lt_float: {
      auto const expr = static_cast<Lt_Float*>(generic_expr);
      return Visitor_Status::e_continue;
    }

    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  Visitor_Status Visitor::visit(Type*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Expr_Init*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Expr_Call*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Expr_If*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Expr_Index*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Expr_Field*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Expr_Identifier*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Lt_Bool*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Lt_Integer*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Lt_Float*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Variable*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Decl_Function*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Decl_Stage_Function*)
  {
    return Visitor_Status::e_continue;
  }

  Visitor_Status Visitor::visit(Decl_Struct*)
  {
    return Visitor_Status::e_continue;
  }
} // namespace vush::ast
