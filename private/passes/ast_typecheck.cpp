#include "anton/expected.hpp"
#include "builtin_symbols.hpp"
#include <passes.hpp>

#include <anton/algorithm.hpp>
#include <ast.hpp>
#include <context.hpp>
#include <diagnostics.hpp>

namespace vush {
  // select_overload
  //
  [[nodiscard]] static anton::Expected<ast::Decl_Function const*, Error>
  select_overload(Context& ctx, ast::Expr_Call const* const call,
                  ast::Decl_Overloaded_Function const* const overloads);

  // evaluate_expression_type
  // Evaluate the type of an expression node. Recursively descend the hierarchy evaluating types of
  // all expressions in the tree. To prevent costly reevaluation, evaluated types are maintained in
  // a cache within Context.
  //
  [[nodiscard]] static anton::Expected<ast::Type const*, Error>
  evaluate_expression_type(Context& ctx, ast::Node const* const node);

  anton::Expected<ast::Decl_Function const*, Error>
  select_overload(Context& ctx, ast::Expr_Call const* const call,
                  ast::Decl_Overloaded_Function const* const overloads)
  {
    Array<ast::Decl_Function const*> candidates(ctx.allocator);
    for(ast::Decl_Function const* const fn: overloads->overloads) {
      if(fn->parameters.size() != call->arguments.size()) {
        continue;
      }

      bool equal = true;
      anton::Zip_Iterator begin{call->arguments.begin(), fn->parameters.begin()};
      anton::Zip_Iterator end{call->arguments.end(), fn->parameters.end()};
      for(auto const [argument, parameter]: anton::Range(ANTON_MOV(begin), ANTON_MOV(end))) {
        anton::Expected<ast::Type const*, Error> eval_result =
          evaluate_expression_type(ctx, argument);
        if(!eval_result) {
          return {anton::expected_error, ANTON_MOV(eval_result.error())};
        }

        ast::Type const* const type = eval_result.value();
        if(!ast::compare_types_equal(*type, *parameter->type)) {
          equal = false;
          break;
        }
      }

      if(equal) {
        candidates.push_back(fn);
      }
    }

    if(candidates.size() == 0) {
      return {anton::expected_error, err_no_matching_overload(ctx, call, overloads->overloads)};
    }

    if(candidates.size() > 1) {
      return {anton::expected_error, err_ambiguous_overload(ctx, call, candidates)};
    }

    return {anton::expected_value, candidates[0]};
  }

  anton::Expected<ast::Type const*, Error> evaluate_expression_type(Context& ctx,
                                                                    ast::Node const* const node)
  {
    ast::Type const* const cached_type = ctx.find_node_type(node);
    if(cached_type) {
      return {anton::expected_value, cached_type};
    }

    switch(node->node_kind) {
      case ast::Node_Kind::lt_bool: {
        ast::Type const* const type = get_builtin_type(ast::GLSL_Type::glsl_bool);
        ctx.add_node_type(node, type);
        return {anton::expected_value, type};
      } break;

      case ast::Node_Kind::lt_integer: {
        ast::Lt_Integer const* const lt = static_cast<ast::Lt_Integer const*>(node);
        switch(lt->kind) {
          case ast::Lt_Integer_Kind::i32: {
            ast::Type const* const type = get_builtin_type(ast::GLSL_Type::glsl_int);
            ctx.add_node_type(node, type);
            return {anton::expected_value, type};
          }

          case ast::Lt_Integer_Kind::u32: {
            ast::Type const* const type = get_builtin_type(ast::GLSL_Type::glsl_uint);
            ctx.add_node_type(node, type);
            return {anton::expected_value, type};
          }
        }
      } break;

      case ast::Node_Kind::lt_float: {
        ast::Lt_Float const* const lt = static_cast<ast::Lt_Float const*>(node);
        switch(lt->kind) {
          case ast::Lt_Float_Kind::f32: {
            ast::Type const* const type = get_builtin_type(ast::GLSL_Type::glsl_float);
            ctx.add_node_type(node, type);
            return {anton::expected_value, type};
          }

          case ast::Lt_Float_Kind::f64: {
            ast::Type const* const type = get_builtin_type(ast::GLSL_Type::glsl_double);
            ctx.add_node_type(node, type);
            return {anton::expected_value, type};
          }
        }
      } break;

      case ast::Node_Kind::expr_identifier: {
        ast::Node const* const definition = ctx.find_node_definition(node);
        switch(definition->node_kind) {
          case ast::Node_Kind::variable: {
            ast::Variable const* const variable = static_cast<ast::Variable const*>(definition);
            ast::Type const* const type = variable->type;
            ctx.add_node_type(node, type);
            return {anton::expected_value, type};
          }

          case ast::Node_Kind::func_parameter: {
            ast::Func_Parameter const* const parameter =
              static_cast<ast::Func_Parameter const*>(definition);
            ast::Type const* const type = parameter->type;
            ctx.add_node_type(node, type);
            return {anton::expected_value, type};
          }

          case ast::Node_Kind::decl_overloaded_function: {
            // We expected an identifier naming a variable or a parameter.
            return {anton::expected_error,
                    err_identifier_names_a_function_but_is_not_called(ctx, node->source_info)};
          }

          default:
            // Validated in the validation pass.
            ANTON_ASSERT(false, "invalid identifier definition kind");
            ANTON_UNREACHABLE();
        }
      } break;

      case ast::Node_Kind::expr_call: {
        ast::Expr_Call const* const expr = static_cast<ast::Expr_Call const*>(node);

        ast::Node const* const definition = ctx.find_node_definition(node);
        ANTON_ASSERT(definition != nullptr, "expr_call has no definition");
        ANTON_ASSERT(definition->node_kind == ast::Node_Kind::decl_overloaded_function,
                     "expr_call's definition is not a decl_overloaded_function");
        ast::Decl_Overloaded_Function const* const fn =
          static_cast<ast::Decl_Overloaded_Function const*>(definition);
        anton::Expected<ast::Decl_Function const*, Error> result = select_overload(ctx, expr, fn);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }

        ast::Decl_Function const* const overload = result.value();
        ctx.add_overload(node, overload);
        ast::Type const* const type = overload->return_type;
        ctx.add_node_type(node, type);
        return {anton::expected_value, type};
      } break;

      case ast::Node_Kind::expr_init: {
        ast::Expr_Init const* const expr = static_cast<ast::Expr_Init const*>(node);

        // TODO: Separate diagnostics for each type kind.
        ast::Type const* generic_type = expr->type;
        switch(generic_type->node_kind) {
          case ast::Node_Kind::type_builtin: {
            // Initialization of builtin types has already been validated, hence we proceed with
            // vectors.
            // TODO: We ought to make swizzles an exception to member lookup since generating all
            //       permutations would result in considerably too many members. Additionally, we
            //       would have to include an exception for their initialization.
            // return {anton::expected_error, err_unimplemented(ctx, expr->source_info)};
            // return {anton::expected_value, generic_type};
          } break;

          case ast::Node_Kind::type_user_defined: {
            ast::Type_User_Defined const* const type =
              static_cast<ast::Type_User_Defined const*>(generic_type);
            ast::Node const* const definition_node = ctx.find_type_definition(type->value);
            ANTON_ASSERT(definition_node->node_kind == ast::Node_Kind::decl_struct,
                         "type definition node is not a decl_struct");
            ast::Decl_Struct const* const decl =
              static_cast<ast::Decl_Struct const*>(definition_node);
            ast::Member_List::iterator const members_begin = decl->members.begin();
            ast::Member_List::iterator const members_end = decl->members.end();
            for(ast::Initializer const* const generic_initializer: expr->initializers) {
              ANTON_ASSERT(generic_initializer->node_kind == ast::Node_Kind::named_initializer,
                           "struct initializer is not named_initializer");
              ast::Named_Initializer const* const initializer =
                static_cast<ast::Named_Initializer const*>(generic_initializer);
              anton::String_View const identifier = initializer->identifier->value;
              ast::Member_List::iterator const member = anton::find_if(
                members_begin, members_end, [identifier](ast::Struct_Member const* const member) {
                  return member->identifier->value == identifier;
                });
              if(member == members_end) {
                return {anton::expected_error,
                        err_named_initializer_no_field_named(ctx, decl, initializer)};
              }

              anton::Expected<ast::Type const*, Error> result =
                evaluate_expression_type(ctx, initializer->expression);
              if(!result) {
                return ANTON_MOV(result);
              }

              if(!compare_types_equal(*(*member)->type, *result.value())) {
                return {anton::expected_error,
                        err_cannot_convert_type(ctx, (*member)->type, result.value())};
              }
            }
          } break;

          case ast::Node_Kind::type_array: {
            // TODO: Implement
            return {anton::expected_error, err_unimplemented(ctx, expr->source_info)};
          } break;

          default:
            ANTON_ASSERT(false, "invalid node kind");
            ANTON_UNREACHABLE();
        }

        ctx.add_node_type(node, expr->type);
        return {anton::expected_value, expr->type};
      } break;

      case ast::Node_Kind::expr_assignment: {
        // TODO: We have to verify that the type we are assigning to is not an opaque type
        //       or a user defined type with opaque types.
        ast::Expr_Assignment const* const expr = static_cast<ast::Expr_Assignment const*>(node);
        anton::Expected<ast::Type const*, Error> result_lhs =
          evaluate_expression_type(ctx, expr->lhs);
        if(!result_lhs) {
          return ANTON_MOV(result_lhs);
        }

        anton::Expected<ast::Type const*, Error> result_rhs =
          evaluate_expression_type(ctx, expr->rhs);
        if(!result_rhs) {
          return ANTON_MOV(result_rhs);
        }

        ast::Type const* const lhs_type = result_lhs.value();
        ast::Type const* const rhs_type = result_rhs.value();
        if(compare_types_equal(*lhs_type, *rhs_type)) {
          ctx.add_node_type(node, lhs_type);
          return {anton::expected_value, lhs_type};
        } else {
          return {anton::expected_error, err_no_assignment_operator(ctx, rhs_type, lhs_type, expr)};
        }
      } break;

      case ast::Node_Kind::expr_parentheses: {
        ast::Expr_Parentheses const* const expr = static_cast<ast::Expr_Parentheses const*>(node);
        anton::Expected<ast::Type const*, Error> result =
          evaluate_expression_type(ctx, expr->expression);
        if(result) {
          ctx.add_node_type(node, result.value());
        }
        return ANTON_MOV(result);
      } break;

      case ast::Node_Kind::expr_if: {
        ast::Expr_If const* const expr = static_cast<ast::Expr_If const*>(node);
        anton::Expected<ast::Type const*, Error> condition_result =
          evaluate_expression_type(ctx, expr->condition);
        if(!condition_result) {
          return ANTON_MOV(condition_result);
        }

        ast::Type const* const condition_type = condition_result.value();
        ast::Type const* const bool_type = get_builtin_type(ast::GLSL_Type::glsl_bool);
        if(!compare_types_equal(*bool_type, *condition_type)) {
          return {anton::expected_error,
                  err_condition_not_of_bool_type(ctx, expr->condition, condition_type)};
        }

        anton::Expected<ast::Type const*, Error> then_result =
          evaluate_expression_type(ctx, expr->then_branch);
        if(!then_result) {
          return ANTON_MOV(then_result);
        }

        anton::Expected<ast::Type const*, Error> else_result =
          evaluate_expression_type(ctx, expr->else_branch);
        if(!else_result) {
          return ANTON_MOV(else_result);
        }

        ast::Type const* const then_type = then_result.value();
        ast::Type const* const else_type = else_result.value();
        if(!compare_types_equal(*then_type, *else_type)) {
          return {anton::expected_error,
                  err_incompatible_if_expression_types(ctx, then_type, expr->then_branch, else_type,
                                                       expr->else_branch)};
        }

        ctx.add_node_type(node, then_type);
        return {anton::expected_value, then_type};
      } break;

      case ast::Node_Kind::expr_array_access: {
        ast::Expr_Array_Access const* const expr = static_cast<ast::Expr_Array_Access const*>(node);
        anton::Expected<ast::Type const*, Error> base_result =
          evaluate_expression_type(ctx, expr->base);
        if(!base_result) {
          return ANTON_MOV(base_result);
        }

        ast::Type const* const base_type = base_result.value();
        if(!is_array(*base_type)) {
          return {anton::expected_error, err_type_is_not_array(ctx, base_type, expr->base)};
        }

        anton::Expected<ast::Type const*, Error> index_result =
          evaluate_expression_type(ctx, expr->index);
        if(!index_result) {
          return ANTON_MOV(index_result);
        }

        if(!is_integer(*index_result.value())) {
          return {anton::expected_error,
                  err_array_index_is_not_integer(ctx, index_result.value(), expr->index)};
        }

        ctx.add_node_type(node, base_type);
        return {anton::expected_value, base_type};
      } break;

      case ast::Node_Kind::expr_member_access: {
        ast::Expr_Member_Access const* const expr =
          static_cast<ast::Expr_Member_Access const*>(node);
        anton::Expected<ast::Type const*, Error> result = evaluate_expression_type(ctx, expr->base);
        if(!result) {
          return ANTON_MOV(result);
        }

        // Arrays have no members. Builtin types with the exception of vectors do not have members.
        // TODO: Separate diagnostics for each type kind.
        ast::Type const* generic_type = result.value();
        switch(generic_type->node_kind) {
          case ast::Node_Kind::type_builtin: {
            // TODO: We temporarily do not make an exception for the vectors and reject all
            //       member accesses on builtin types. Allow swizzles on vectors.
            return {anton::expected_error,
                    err_type_has_no_member_named(ctx, generic_type, expr->member)};
          } break;

          case ast::Node_Kind::type_user_defined: {
            ast::Type_User_Defined const* const type =
              static_cast<ast::Type_User_Defined const*>(generic_type);
            ast::Node const* const definition_node = ctx.find_type_definition(type->value);
            ANTON_ASSERT(definition_node->node_kind == ast::Node_Kind::decl_struct,
                         "type definition node is not a decl_struct");
            ast::Decl_Struct const* const decl =
              static_cast<ast::Decl_Struct const*>(definition_node);
            for(ast::Struct_Member const* const member: decl->members) {
              if(member->identifier->value != expr->member->value) {
                continue;
              }

              ctx.add_node_type(node, member->type);
              return {anton::expected_value, member->type};
            }

            return {anton::expected_error,
                    err_type_has_no_member_named(ctx, generic_type, expr->member)};
          } break;

          case ast::Node_Kind::type_array: {
            return {anton::expected_error,
                    err_type_has_no_member_named(ctx, generic_type, expr->member)};
          } break;

          default:
            ANTON_ASSERT(false, "invalid node kind");
            ANTON_UNREACHABLE();
        }
      } break;

      case ast::Node_Kind::expr_reinterpret: {
        // TODO: Implement once transform for reinterpret is implemented.
        ANTON_ASSERT(false, "unimplemented");
        ANTON_UNREACHABLE();
      } break;

      // TODO: Should we handle expr_default here?
      default:
        ANTON_ASSERT(false, "invalid expression kind");
        ANTON_UNREACHABLE();
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  typecheck_statements(Context& ctx, ast::Node_List const statements)
  {
    for(ast::Node const* const stmt: statements) {
      switch(stmt->node_kind) {
        case ast::Node_Kind::variable: {
          ast::Variable const* const node = static_cast<ast::Variable const*>(stmt);
          if(node->initializer != nullptr) {
            anton::Expected<ast::Type const*, Error> result =
              evaluate_expression_type(ctx, node->initializer);
            if(!result) {
              return {anton::expected_error, ANTON_MOV(result.error())};
            }

            ast::Type const* const type = result.value();
            if(!compare_types_equal(*node->type, *type)) {
              return {anton::expected_error, err_cannot_convert_type(ctx, node->type, type)};
            }
          } else {
            // Immutable variables must have an initializer.
            bool const immutable = !node->type->qualifiers.mut;
            if(immutable) {
              return {anton::expected_error,
                      err_immutable_variable_missing_initializer(ctx, node->source_info)};
            }
          }
        } break;

        case ast::Node_Kind::stmt_block: {
          ast::Stmt_Block const* const node = static_cast<ast::Stmt_Block const*>(stmt);
          anton::Expected<void, Error> result = typecheck_statements(ctx, node->statements);
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        case ast::Node_Kind::stmt_if: {
          ast::Stmt_If const* const node = static_cast<ast::Stmt_If const*>(stmt);
          anton::Expected<ast::Type const*, Error> condition_result =
            evaluate_expression_type(ctx, node->condition);
          if(!condition_result) {
            return {anton::expected_error, ANTON_MOV(condition_result.error())};
          }

          ast::Type const* const condition_type = condition_result.value();
          ast::Type const* const bool_type = get_builtin_type(ast::GLSL_Type::glsl_bool);
          if(!compare_types_equal(*condition_type, *bool_type)) {
            return {anton::expected_error,
                    err_condition_not_of_bool_type(ctx, node->condition, condition_type)};
          }

          if(anton::Expected<void, Error> result = typecheck_statements(ctx, node->then_branch);
             !result) {
            return ANTON_MOV(result);
          }

          if(anton::Expected<void, Error> result = typecheck_statements(ctx, node->else_branch);
             !result) {
            return ANTON_MOV(result);
          }
        } break;

        case ast::Node_Kind::stmt_loop: {
          ast::Stmt_Loop const* const node = static_cast<ast::Stmt_Loop const*>(stmt);
          anton::Expected<ast::Type const*, Error> condition_result =
            evaluate_expression_type(ctx, node->condition);
          if(!condition_result) {
            return {anton::expected_error, ANTON_MOV(condition_result.error())};
          }

          ast::Type const* const condition_type = condition_result.value();
          ast::Type const* const bool_type = get_builtin_type(ast::GLSL_Type::glsl_bool);
          if(!compare_types_equal(*condition_type, *bool_type)) {
            return {anton::expected_error,
                    err_condition_not_of_bool_type(ctx, node->condition, condition_type)};
          }

          anton::Expected<void, Error> continuation_result =
            typecheck_statements(ctx, node->continuation);
          if(!continuation_result) {
            return ANTON_MOV(continuation_result);
          }

          anton::Expected<void, Error> statements_result =
            typecheck_statements(ctx, node->statements);
          if(!statements_result) {
            return ANTON_MOV(statements_result);
          }
        } break;

        case ast::Node_Kind::stmt_switch: {
          ast::Stmt_Switch const* const node = static_cast<ast::Stmt_Switch const*>(stmt);
          anton::Expected<ast::Type const*, Error> expression_result =
            evaluate_expression_type(ctx, node->expression);
          if(!expression_result) {
            return {anton::expected_error, ANTON_MOV(expression_result.error())};
          }

          ast::Type const* const expression_type = expression_result.value();
          ast::Type const* const i32_type = get_builtin_type(ast::GLSL_Type::glsl_int);
          ast::Type const* const u32_type = get_builtin_type(ast::GLSL_Type::glsl_uint);
          if(!compare_types_equal(*expression_type, *i32_type) &&
             !compare_types_equal(*expression_type, *u32_type)) {
            return {anton::expected_error,
                    err_condition_not_of_bool_type(ctx, node->expression, expression_type)};
          }

          for(ast::Switch_Arm const* const arm: node->arms) {
            anton::Expected<void, Error> statements_result =
              typecheck_statements(ctx, arm->statements);
            if(!statements_result) {
              return ANTON_MOV(statements_result);
            }
          }
        } break;

        case ast::Node_Kind::stmt_break: {
          // Nothing to do.
        } break;

        case ast::Node_Kind::stmt_continue: {
          // Nothing to do.
        } break;

        case ast::Node_Kind::stmt_return: {
          ast::Stmt_Switch const* const node = static_cast<ast::Stmt_Switch const*>(stmt);
          anton::Expected<ast::Type const*, Error> expression_result =
            evaluate_expression_type(ctx, node->expression);
          if(!expression_result) {
            return {anton::expected_error, ANTON_MOV(expression_result.error())};
          }

          // TODO: Check the type of the expression being returned against the return type of the function.
        } break;

        case ast::Node_Kind::stmt_expression: {
          ast::Stmt_Expression const* const node = static_cast<ast::Stmt_Expression const*>(stmt);
          anton::Expected<ast::Type const*, Error> expression_result =
            evaluate_expression_type(ctx, node->expression);
          if(!expression_result) {
            return {anton::expected_error, ANTON_MOV(expression_result.error())};
          }
        } break;

        default:
          break;
      }
    }
    return {anton::expected_value};
  }

  anton::Expected<void, Error> run_ast_typecheck_pass(Context& ctx, ast::Node_List const ast)
  {
    for(ast::Node const* const node: ast) {
      switch(node->node_kind) {
        case ast::Node_Kind::decl_overloaded_function: {
          ast::Decl_Overloaded_Function const* const fn =
            static_cast<ast::Decl_Overloaded_Function const*>(node);
          // TODO: Identical problem like in validation. Errors are reported out of order.
          //       We have to keep functions in the ast in the order they appear and
          //       gather them to a separate storage of overloaded functions elsewhere.
          for(ast::Decl_Function const* const fn_overload: fn->overloads) {
            anton::Expected<void, Error> result = typecheck_statements(ctx, fn_overload->body);
            if(!result) {
              return ANTON_MOV(result);
            }
          }
        } break;

        case ast::Node_Kind::decl_stage_function: {
          ast::Decl_Stage_Function const* const fn =
            static_cast<ast::Decl_Stage_Function const*>(node);
          anton::Expected<void, Error> result = typecheck_statements(ctx, fn->body);
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        default:
          // Nothing.
          break;
      }
    }
    return anton::expected_value;
  }
} // namespace vush
