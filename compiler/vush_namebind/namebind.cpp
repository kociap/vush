#include <vush_namebind/namebind.hpp>

#include <anton/assert.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/context.hpp>
#include <vush_core/scoped_map.hpp>
#include <vush_diagnostics/diagnostics.hpp>

namespace vush {
  enum struct Symbol_Kind {
    e_variable,
    e_parameter,
    e_struct,
    e_overload_group,
  };

  struct Symbol {
    anton::String_View identifier;
    union {
      ast::Variable* value_variable;
      ast::Fn_Parameter* value_parameter;
      ast::Decl_Struct* value_struct;
      ast::Overload_Group* value_overload_group;
    };
    Symbol_Kind kind;

    Symbol(anton::String_View identifier, ast::Variable* value)
      : identifier(identifier), value_variable(value), kind(Symbol_Kind::e_variable)
    {
    }

    Symbol(anton::String_View identifier, ast::Fn_Parameter* value)
      : identifier(identifier), value_parameter(value), kind(Symbol_Kind::e_parameter)
    {
    }

    Symbol(anton::String_View identifier, ast::Decl_Struct* value)
      : identifier(identifier), value_struct(value), kind(Symbol_Kind::e_struct)
    {
    }

    Symbol(anton::String_View identifier, ast::Overload_Group* value)
      : identifier(identifier), value_overload_group(value), kind(Symbol_Kind::e_overload_group)
    {
    }
  };

  using Symbol_Table = Scoped_Map<anton::String_View, Symbol>;

  [[nodiscard]] static anton::Expected<void, Error>
  namebind_call(Context& ctx, Symbol_Table const& symtable, ast::Expr_Call* const call)
  {
    Symbol const* const symbol = symtable.find_entry(call->identifier.value);
    if(!symbol) {
      return {anton::expected_error, err_undefined_symbol(ctx, call->source_info)};
    }

    switch(symbol->kind) {
    case Symbol_Kind::e_overload_group: {
      call->overload_group = symbol->value_overload_group;
      return anton::expected_value;
    }

    default:
      // TODO: Error.
      return {anton::expected_error, Error()};
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  namebind_identifier(Context& ctx, Symbol_Table const& symtable, ast::Expr_Identifier* identifier)
  {
    Symbol const* const symbol = symtable.find_entry(identifier->value);
    if(!symbol) {
      return {anton::expected_error, err_undefined_symbol(ctx, identifier->source_info)};
    }

    switch(symbol->kind) {
    case Symbol_Kind::e_variable: {
      identifier->definition = symbol->value_variable;
      return anton::expected_value;
    }

    case Symbol_Kind::e_parameter: {
      identifier->definition = symbol->value_parameter;
      return anton::expected_value;
    }

    case Symbol_Kind::e_overload_group: {
      return {anton::expected_error,
              err_identifier_names_a_function_but_is_not_called(ctx, identifier->source_info)};
    } break;

    default:
      // TODO: Error.
      return {anton::expected_error, Error()};
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  namebind_type(Context& ctx, Symbol_Table const& symtable, ast::Type* const type)
  {
    switch(type->node_kind) {
    case ast::Node_Kind::type_builtin: {
      return anton::expected_value;
    }

    case ast::Node_Kind::type_struct: {
      auto const type_struct = static_cast<ast::Type_Struct*>(type);
      Symbol const* const symbol = symtable.find_entry(type_struct->value);
      if(symbol == nullptr) {
        return {anton::expected_error, err_undefined_symbol(ctx, type->source_info)};
      }

      switch(symbol->kind) {
      case Symbol_Kind::e_struct: {
        type_struct->definition = symbol->value_struct;
        return anton::expected_value;
      }

      default:
        // TODO: Error.
        return {anton::expected_error, Error()};
      }

      type_struct->definition = symbol->value_struct;
      return anton::expected_value;
    }

    case ast::Node_Kind::type_array: {
      auto const array = static_cast<ast::Type_Array*>(type);
      return namebind_type(ctx, symtable, array->base);
    }

    default:
      ANTON_ASSERT(false, "unhandled type kind");
      ANTON_UNREACHABLE();
    }
  }

  // add_symbol
  // Checks whether a symbol already exists and if not, adds it to the symbol table.
  // Otherwise returns an error diagnostic.
  //
  [[nodiscard]] static anton::Expected<void, Error> add_symbol(Context& ctx, Symbol_Table& symtable,
                                                               Symbol const& symbol)
  {
    Symbol const* const original_symbol = symtable.find_entry(symbol.identifier);
    if(original_symbol != nullptr) {
      auto get_symbol_identifier_source = [](Symbol const& symbol) -> Source_Info {
        switch(symbol.kind) {
        case Symbol_Kind::e_variable: {
          return symbol.value_variable->identifier.source_info;
        }

        case Symbol_Kind::e_parameter: {
          return symbol.value_parameter->identifier.source_info;
        }

        case Symbol_Kind::e_struct: {
          return symbol.value_struct->identifier.source_info;
        }

        case Symbol_Kind::e_overload_group: {
          // Use the first overload as the source location.
          ast::Decl_Function* const overload = symbol.value_overload_group->overloads[0];
          return overload->identifier.source_info;
        }

        default:
          ANTON_FAIL(false, "unknown symbol type");
          ANTON_UNREACHABLE();
        }
      };

      Source_Info const old_name = get_symbol_identifier_source(*original_symbol);
      Source_Info const new_name = get_symbol_identifier_source(symbol);
      return {anton::expected_error, err_symbol_redefinition(ctx, old_name, new_name)};
    }

    symtable.add_entry(symbol.identifier, symbol);
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  defcheck_expression(Context& ctx, Symbol_Table& symtable, ast::Expr* const expression)
  {
    switch(expression->node_kind) {
    case ast::Node_Kind::expr_identifier: {
      auto const node = static_cast<ast::Expr_Identifier*>(expression);
      anton::Expected<void, Error> result = namebind_identifier(ctx, symtable, node);
      if(!result) {
        return ANTON_MOV(result);
      } else {
        return anton::expected_value;
      }
    }

    case ast::Node_Kind::expr_call: {
      auto const node = static_cast<ast::Expr_Call*>(expression);
      anton::Expected<void, Error> result = namebind_call(ctx, symtable, node);
      if(!result) {
        return ANTON_MOV(result);
      }

      for(ast::Expr* const argument: node->arguments) {
        anton::Expected<void, Error> result = defcheck_expression(ctx, symtable, argument);
        if(!result) {
          return ANTON_MOV(result);
        }
      }

      return anton::expected_value;
    }

    case ast::Node_Kind::expr_init: {
      auto const node = static_cast<ast::Expr_Init*>(expression);
      anton::Expected<void, Error> type_result = namebind_type(ctx, symtable, node->type);
      if(!type_result) {
        return ANTON_MOV(type_result);
      }

      for(ast::Initializer* const generic_initializer: node->initializers) {
        switch(generic_initializer->node_kind) {
        case ast::Node_Kind::named_initializer: {
          ast::Named_Initializer* const initializer =
            static_cast<ast::Named_Initializer*>(generic_initializer);
          // Checking struct members is done at typecheck stage, hence we do not check the
          // identifier.
          anton::Expected<void, Error> result =
            defcheck_expression(ctx, symtable, initializer->expression);
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        case ast::Node_Kind::indexed_initializer: {
          ast::Indexed_Initializer* const initializer =
            static_cast<ast::Indexed_Initializer*>(generic_initializer);
          // Checking bounds is done at typecheck stage, hence we do not check the index.
          anton::Expected<void, Error> result =
            defcheck_expression(ctx, symtable, initializer->expression);
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        case ast::Node_Kind::basic_initializer: {
          ast::Basic_Initializer* const initializer =
            static_cast<ast::Basic_Initializer*>(generic_initializer);
          anton::Expected<void, Error> result =
            defcheck_expression(ctx, symtable, initializer->expression);
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        default:
          ANTON_ASSERT(false, "invalid initializer node kind");
          ANTON_UNREACHABLE();
        }
      }

      return anton::expected_value;
    } break;

    case ast::Node_Kind::expr_field: {
      ast::Expr_Field* const node = static_cast<ast::Expr_Field*>(expression);
      return defcheck_expression(ctx, symtable, node->base);
    }

    case ast::Node_Kind::expr_index: {
      ast::Expr_Index* const node = static_cast<ast::Expr_Index*>(expression);
      anton::Expected<void, Error> base_result = defcheck_expression(ctx, symtable, node->base);
      if(!base_result) {
        return ANTON_MOV(base_result);
      }

      anton::Expected<void, Error> index_result = defcheck_expression(ctx, symtable, node->index);
      if(!index_result) {
        return ANTON_MOV(index_result);
      }

      return anton::expected_value;
    }

    case ast::Node_Kind::expr_parentheses: {
      ast::Expr_Parentheses* const node = static_cast<ast::Expr_Parentheses*>(expression);
      return defcheck_expression(ctx, symtable, node->expression);
    }

    case ast::Node_Kind::expr_assignment: {
      ast::Expr_Assignment* const node = static_cast<ast::Expr_Assignment*>(expression);
      if(anton::Expected<void, Error> lhs = defcheck_expression(ctx, symtable, node->lhs); !lhs) {
        return ANTON_MOV(lhs);
      }

      if(anton::Expected<void, Error> rhs = defcheck_expression(ctx, symtable, node->rhs); !rhs) {
        return ANTON_MOV(rhs);
      }

      return anton::expected_value;
    }

    case ast::Node_Kind::expr_if: {
      ast::Expr_If* const node = static_cast<ast::Expr_If*>(expression);
      anton::Expected<void, Error> result_condition =
        defcheck_expression(ctx, symtable, node->condition);
      if(!result_condition) {
        return ANTON_MOV(result_condition);
      }

      anton::Expected<void, Error> result_then =
        defcheck_expression(ctx, symtable, node->then_branch);
      if(!result_then) {
        return ANTON_MOV(result_then);
      }

      anton::Expected<void, Error> result_else =
        defcheck_expression(ctx, symtable, node->else_branch);
      if(!result_else) {
        return ANTON_MOV(result_else);
      }

      return anton::expected_value;
    }

    case ast::Node_Kind::expr_reinterpret: {
      // TODO: Implement once we implement transform for reinterpret.
      // Owning_Ptr<Reinterpret_Expression>& node = (Owning_Ptr<Reinterpret_Expression>&)expression;
      // anton::Expected<void, anton::String> index_res = defcheck_expression(ctx, node->index);
      // if(!index_res) {
      //     return {anton::expected_error, ANTON_MOV(index_res.error())};
      // }

      // anton::Expected<void, anton::String> source_res = defcheck_expression(ctx, node->source);
      // if(!source_res) {
      //     return {anton::expected_error, ANTON_MOV(source_res.error())};
      // }
      return anton::expected_value;
    }

    default: {
      // Nothing to do.
      return anton::expected_value;
    }
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  defcheck_statements(Context& ctx, Symbol_Table& symtable, ast::Node_List const statements)
  {
    // We push new scope and pop it only at the end of the function. We do
    // not pop the scope when we fail because an error always leads to termination.
    symtable.push_scope();

    for(ast::Node* const stmt: statements) {
      switch(stmt->node_kind) {
      case ast::Node_Kind::variable: {
        ast::Variable* const node = static_cast<ast::Variable*>(stmt);
        anton::Expected<void, Error> type_res = namebind_type(ctx, symtable, node->type);
        if(!type_res) {
          return ANTON_MOV(type_res);
        }

        // We first check the initialiser, then add the symbol for
        // the variable, so that if the initialiser uses the variable,
        // an error is reported.
        if(node->initializer != nullptr) {
          anton::Expected<void, Error> res = defcheck_expression(ctx, symtable, node->initializer);
          if(!res) {
            return ANTON_MOV(res);
          }
        }

        anton::Expected<void, Error> symbol_res =
          add_symbol(ctx, symtable, Symbol(node->identifier.value, node));
        if(!symbol_res) {
          return ANTON_MOV(symbol_res);
        }
      } break;

      case ast::Node_Kind::stmt_block: {
        ast::Stmt_Block* const node = static_cast<ast::Stmt_Block*>(stmt);
        anton::Expected<void, Error> result = defcheck_statements(ctx, symtable, node->statements);
        if(!result) {
          return ANTON_MOV(result);
        }
      } break;

      case ast::Node_Kind::stmt_if: {
        ast::Stmt_If* const node = static_cast<ast::Stmt_If*>(stmt);
        if(anton::Expected<void, Error> res = defcheck_expression(ctx, symtable, node->condition);
           !res) {
          return ANTON_MOV(res);
        }

        if(anton::Expected<void, Error> res = defcheck_statements(ctx, symtable, node->then_branch);
           !res) {
          return ANTON_MOV(res);
        }

        if(anton::Expected<void, Error> res = defcheck_statements(ctx, symtable, node->else_branch);
           !res) {
          return ANTON_MOV(res);
        }
      } break;

      case ast::Node_Kind::stmt_loop: {
        ast::Stmt_Loop* const node = static_cast<ast::Stmt_Loop*>(stmt);
        if(node->condition) {
          anton::Expected<void, Error> res = defcheck_expression(ctx, symtable, node->condition);
          if(!res) {
            return ANTON_MOV(res);
          }
        }

        anton::Expected<void, Error> continuation_result =
          defcheck_statements(ctx, symtable, node->continuation);
        if(!continuation_result) {
          return ANTON_MOV(continuation_result);
        }

        anton::Expected<void, Error> statements_result =
          defcheck_statements(ctx, symtable, node->statements);
        if(!statements_result) {
          return ANTON_MOV(statements_result);
        }
      } break;

      case ast::Node_Kind::stmt_switch: {
        auto const node = static_cast<ast::Stmt_Switch*>(stmt);
        anton::Expected<void, Error> switch_expr_result =
          defcheck_expression(ctx, symtable, node->expression);
        if(!switch_expr_result) {
          return ANTON_MOV(switch_expr_result);
        }

        for(ast::Switch_Arm* const arm: node->arms) {
          anton::Expected<void, Error> result = defcheck_statements(ctx, symtable, arm->statements);
          if(!result) {
            return ANTON_MOV(result);
          }
        }
      } break;

      case ast::Node_Kind::stmt_return: {
        ast::Stmt_Return* const node = static_cast<ast::Stmt_Return*>(stmt);
        if(node->expression) {
          anton::Expected<void, Error> result =
            defcheck_expression(ctx, symtable, node->expression);
          if(!result) {
            return ANTON_MOV(result);
          }
        }
      } break;

      case ast::Node_Kind::stmt_expression: {
        ast::Stmt_Expression* const node = static_cast<ast::Stmt_Expression*>(stmt);
        anton::Expected<void, Error> result = defcheck_expression(ctx, symtable, node->expression);
        if(!result) {
          return ANTON_MOV(result);
        }
      } break;

      default:
        // stmt_break, stmt_continue, stmt_discard need no validation.
        break;
      }
    }

    symtable.pop_scope();
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  defcheck_struct(Context& ctx, Symbol_Table& symtable, ast::Decl_Struct* const d)
  {
    for(ast::Struct_Member* const member: d->members) {
      anton::Expected<void, Error> result = namebind_type(ctx, symtable, member->type);
      if(!result) {
        return ANTON_MOV(result);
      }
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  defcheck_function(Context& ctx, Symbol_Table& symtable, ast::Decl_Function* const fn)
  {
    // We do not defcheck the identifier of the function because it has
    // already been added as an overloaded function.

    anton::Expected<void, Error> return_result = namebind_type(ctx, symtable, fn->return_type);
    if(!return_result) {
      return ANTON_MOV(return_result);
    }

    // Push a new scope for the function body and parameters.
    symtable.push_scope();
    for(ast::Fn_Parameter* const parameter: fn->parameters) {
      anton::Expected<void, Error> type_result = namebind_type(ctx, symtable, parameter->type);
      if(!type_result) {
        return ANTON_MOV(type_result);
      }

      anton::Expected<void, Error> symbol_result =
        add_symbol(ctx, symtable, Symbol(parameter->identifier.value, parameter));
      if(!symbol_result) {
        return ANTON_MOV(symbol_result);
      }
    }

    anton::Expected<void, Error> statements_result = defcheck_statements(ctx, symtable, fn->body);
    if(!statements_result) {
      return ANTON_MOV(statements_result);
    }

    symtable.pop_scope();
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  defcheck_stage_function(Context& ctx, Symbol_Table& symtable, ast::Decl_Stage_Function* const fn)
  {
    anton::Expected<void, Error> return_result = namebind_type(ctx, symtable, fn->return_type);
    if(!return_result) {
      return ANTON_MOV(return_result);
    }

    // Push a new scope for the function body and parameters.
    symtable.push_scope();
    for(ast::Fn_Parameter* const parameter: fn->parameters) {
      anton::Expected<void, Error> type_result = namebind_type(ctx, symtable, parameter->type);
      if(!type_result) {
        return ANTON_MOV(type_result);
      }

      anton::Expected<void, Error> symbol_result =
        add_symbol(ctx, symtable, Symbol(parameter->identifier.value, parameter));
      if(!symbol_result) {
        return ANTON_MOV(symbol_result);
      }
    }

    anton::Expected<void, Error> statements_result = defcheck_statements(ctx, symtable, fn->body);
    if(!statements_result) {
      return ANTON_MOV(statements_result);
    }

    symtable.pop_scope();
    return anton::expected_value;
  }

  anton::Expected<void, Error> run_namebind_pass(Context& ctx, ast::Node_List const ast)
  {
    Symbol_Table symtable(ctx.allocator);
    symtable.push_scope();
    // First add all overload groups...
    for(auto [key, group]: ctx.overload_groups) {
      symtable.add_entry(key, Symbol(key, group));
    }
    // ...then all global symbols from the AST apart from functions...
    for(ast::Node* const decl: ast) {
      switch(decl->node_kind) {
      case ast::Node_Kind::variable: {
        ast::Variable* const d = static_cast<ast::Variable*>(decl);
        anton::Expected<void, Error> result =
          add_symbol(ctx, symtable, Symbol(d->identifier.value, d));
        if(!result) {
          return ANTON_MOV(result);
        }
      } break;

      case ast::Node_Kind::decl_struct: {
        ast::Decl_Struct* const d = static_cast<ast::Decl_Struct*>(decl);
        anton::Expected<void, Error> result =
          add_symbol(ctx, symtable, Symbol(d->identifier.value, d));
        if(!result) {
          return ANTON_MOV(result);
        }
      } break;

      default:
        // Nothing.
        break;
      }
    }

    // ...then run all defchecks.

    // TODO: Constants defcheck.

    for(ast::Node* const node: ast) {
      switch(node->node_kind) {
      case ast::Node_Kind::decl_struct: {
        auto const decl = static_cast<ast::Decl_Struct*>(node);
        anton::Expected<void, Error> result = defcheck_struct(ctx, symtable, decl);
        if(!result) {
          return ANTON_MOV(result);
        }
      } break;

      case ast::Node_Kind::decl_function: {
        auto const fn = static_cast<ast::Decl_Function*>(node);
        anton::Expected<void, Error> result = defcheck_function(ctx, symtable, fn);
        if(!result) {
          return ANTON_MOV(result);
        }
      } break;

      case ast::Node_Kind::decl_stage_function: {
        auto const fn = static_cast<ast::Decl_Stage_Function* const>(node);
        anton::Expected<void, Error> result = defcheck_stage_function(ctx, symtable, fn);
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
