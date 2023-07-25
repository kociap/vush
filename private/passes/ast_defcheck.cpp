#include <passes.hpp>

#include <anton/assert.hpp>
#include <anton/flat_hash_map.hpp>

#include <ast.hpp>
#include <context.hpp>
#include <diagnostics.hpp>

namespace vush {
  using Symbol_Kind = ast::Node_Kind;

  struct Symbol {
  private:
    anton::String_View name;
    ast::Node const* node;

  public:
    Symbol(anton::String_View name, ast::Node const* node): name(name), node(node) {}

    [[nodiscard]] anton::String_View get_name() const
    {
      return name;
    }

    [[nodiscard]] ast::Node const* get_node() const
    {
      return node;
    }

    [[nodiscard]] Symbol_Kind get_kind() const
    {
      return node->node_kind;
    }
  };

  struct Scoped_Symbol_Table {
  private:
    Array<anton::Flat_Hash_Map<anton::String_View, Symbol>> symbols;
    Allocator* allocator;

  public:
    Scoped_Symbol_Table(Allocator* allocator): symbols(allocator), allocator(allocator)
    {
      // Push global scope.
      push_scope();
    }

    // find_symbol
    // Looks up a symbol with the given name in scopes starting from the innermost and progressing
    // towards the outermost.
    //
    // Returns:
    // Pointer to the symbol or nullptr if not found.
    // Adding a new symbol to the scope might invalidate the pointer.
    //
    [[nodiscard]] Symbol const* find_symbol(anton::String_View const name) const
    {
      for(i64 i = symbols.size() - 1; i >= 0; --i) {
        auto result = symbols[i].find(name);
        if(result != symbols[i].end()) {
          return &result->value;
        }
      }
      return nullptr;
    }

    // add_symbol
    // Adds a symbol to the current scope.
    // Adding a symbol might invalidate pointers previously returned by find_symbol.
    //
    void add_symbol(Symbol const symbol)
    {
      auto& symbol_map = symbols.back();
      symbol_map.emplace(symbol.get_name(), symbol);
    }

    // push_scope
    // Add a new scope.
    //
    void push_scope()
    {
      symbols.emplace_back(allocator);
    }

    // pop_scope
    // Pops the current scope. Does not pop the global scope.
    //
    void pop_scope()
    {
      if(symbols.size() > 1) {
        symbols.pop_back();
      }
    }
  };

  // check_and_add_symbol
  // Checks whether a symbol already exists and if not, adds it to the symbol table.
  // Otherwise returns an error diagnostic.
  //
  [[nodiscard]] static anton::Expected<void, Error>
  check_and_add_symbol(Context& ctx, Scoped_Symbol_Table& symtable, Symbol const& symbol)
  {
    Symbol const* const original_symbol = symtable.find_symbol(symbol.get_name());
    if(original_symbol != nullptr) {
      auto get_symbol_name = [](Symbol const& symbol) -> Source_Info {
        switch(symbol.get_kind()) {
          case Symbol_Kind::variable: {
            auto const v = static_cast<ast::Variable const*>(symbol.get_node());
            return v->identifier->source_info;
          }

          case Symbol_Kind::fn_parameter: {
            auto const v = static_cast<ast::Fn_Parameter const*>(symbol.get_node());
            return v->identifier->source_info;
          }

          case Symbol_Kind::decl_overloaded_function: {
            auto const v = static_cast<ast::Decl_Overloaded_Function const*>(symbol.get_node());
            // Use the first overload as the source location.
            ast::Decl_Function const* const overload = v->overloads[0];
            return overload->identifier->source_info;
          }

          case Symbol_Kind::decl_struct: {
            auto const v = static_cast<ast::Decl_Struct const*>(symbol.get_node());
            return v->identifier->source_info;
          }

          default:
            ANTON_FAIL(false, "unknown symbol type");
            ANTON_UNREACHABLE();
        }
      };

      Source_Info const old_name = get_symbol_name(*original_symbol);
      Source_Info const new_name = get_symbol_name(symbol);
      return {anton::expected_error, err_symbol_redefinition(ctx, old_name, new_name)};
    }

    symtable.add_symbol(symbol);
    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  defcheck_type(Context& ctx, Scoped_Symbol_Table& symtable, ast::Type const* const type)
  {
    switch(type->node_kind) {
      case ast::Node_Kind::type_builtin: {
        return anton::expected_value;
      }

      case ast::Node_Kind::type_user_defined: {
        ast::Type_User_Defined const* const udt = static_cast<ast::Type_User_Defined const*>(type);
        Symbol const* const symbol = symtable.find_symbol(udt->value);
        if(symbol == nullptr) {
          return {anton::expected_error, err_undefined_symbol(ctx, type->source_info)};
        }

        ast::Node const* const symbol_node = symbol->get_node();
        ctx.add_node_definition(type, symbol_node);
        return anton::expected_value;
      }

      case ast::Node_Kind::type_array: {
        ast::Type_Array const* const array = static_cast<ast::Type_Array const*>(type);
        return defcheck_type(ctx, symtable, array->base);
      }

      default:
        ANTON_ASSERT(false, "unhandled type kind");
        ANTON_UNREACHABLE();
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  defcheck_expression(Context& ctx, Scoped_Symbol_Table& symtable,
                      ast::Expr const* const expression)
  {
    switch(expression->node_kind) {
      case ast::Node_Kind::expr_identifier: {
        ast::Expr_Identifier const* const node =
          static_cast<ast::Expr_Identifier const*>(expression);

        // TODO: Add symbols for builtin variables.

        Symbol const* const symbol = symtable.find_symbol(node->value);
        if(!symbol) {
          return {anton::expected_error, err_undefined_symbol(ctx, node->source_info)};
        }

        ast::Node const* const symbol_node = symbol->get_node();
        ctx.add_node_definition(node, symbol_node);
        return anton::expected_value;
      }

      case ast::Node_Kind::expr_init: {
        ast::Expr_Init const* const node = static_cast<ast::Expr_Init const*>(expression);
        anton::Expected<void, Error> type_result = defcheck_type(ctx, symtable, node->type);
        if(!type_result) {
          return ANTON_MOV(type_result);
        }

        for(ast::Initializer const* const generic_initializer: node->initializers) {
          switch(generic_initializer->node_kind) {
            case ast::Node_Kind::named_initializer: {
              ast::Named_Initializer const* const initializer =
                static_cast<ast::Named_Initializer const*>(generic_initializer);
              // Checking struct members is done at typecheck stage, hence we do not check the
              // identifier.
              anton::Expected<void, Error> result =
                defcheck_expression(ctx, symtable, initializer->expression);
              if(!result) {
                return ANTON_MOV(result);
              }
            } break;

            case ast::Node_Kind::indexed_initializer: {
              ast::Indexed_Initializer const* const initializer =
                static_cast<ast::Indexed_Initializer const*>(generic_initializer);
              // Checking bounds is done at typecheck stage, hence we do not check the index.
              anton::Expected<void, Error> result =
                defcheck_expression(ctx, symtable, initializer->expression);
              if(!result) {
                return ANTON_MOV(result);
              }
            } break;

            case ast::Node_Kind::basic_initializer: {
              ast::Basic_Initializer const* const initializer =
                static_cast<ast::Basic_Initializer const*>(generic_initializer);
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

      case ast::Node_Kind::expr_call: {
        ast::Expr_Call const* const node = static_cast<ast::Expr_Call const*>(expression);
        Symbol const* const symbol = symtable.find_symbol(node->identifier->value);
        if(!symbol) {
          return {anton::expected_error, err_undefined_symbol(ctx, node->identifier->source_info)};
        }

        ast::Node const* const symbol_node = symbol->get_node();
        ctx.add_node_definition(node, symbol_node);

        for(ast::Expr const* const argument: node->arguments) {
          anton::Expected<void, Error> result = defcheck_expression(ctx, symtable, argument);
          if(!result) {
            return ANTON_MOV(result);
          }
        }

        return anton::expected_value;
      }

      case ast::Node_Kind::expr_field: {
        ast::Expr_Field const* const node = static_cast<ast::Expr_Field const*>(expression);
        return defcheck_expression(ctx, symtable, node->base);
      }

      case ast::Node_Kind::expr_index: {
        ast::Expr_Index const* const node = static_cast<ast::Expr_Index const*>(expression);
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
        ast::Expr_Parentheses const* const node =
          static_cast<ast::Expr_Parentheses const*>(expression);
        return defcheck_expression(ctx, symtable, node->expression);
      }

      case ast::Node_Kind::expr_assignment: {
        ast::Expr_Assignment const* const node =
          static_cast<ast::Expr_Assignment const*>(expression);
        if(anton::Expected<void, Error> lhs = defcheck_expression(ctx, symtable, node->lhs); !lhs) {
          return ANTON_MOV(lhs);
        }

        if(anton::Expected<void, Error> rhs = defcheck_expression(ctx, symtable, node->rhs); !rhs) {
          return ANTON_MOV(rhs);
        }

        return anton::expected_value;
      }

      case ast::Node_Kind::expr_if: {
        ast::Expr_If const* const node = static_cast<ast::Expr_If const*>(expression);
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
  defcheck_statements(Context& ctx, Scoped_Symbol_Table& symtable, ast::Node_List const statements)
  {
    // We push new scope and pop it only at the end of the function. We do
    // not pop the scope when we fail because an error always leads to termination.
    symtable.push_scope();

    for(ast::Node const* const stmt: statements) {
      switch(stmt->node_kind) {
        case ast::Node_Kind::variable: {
          ast::Variable const* const node = static_cast<ast::Variable const*>(stmt);
          anton::Expected<void, Error> type_res = defcheck_type(ctx, symtable, node->type);
          if(!type_res) {
            return ANTON_MOV(type_res);
          }

          // We first check the initialiser, then add the symbol for
          // the variable, so that if the initialiser uses the variable,
          // an error is reported.
          if(node->initializer != nullptr) {
            anton::Expected<void, Error> res =
              defcheck_expression(ctx, symtable, node->initializer);
            if(!res) {
              return ANTON_MOV(res);
            }
          }

          anton::Expected<void, Error> symbol_res =
            check_and_add_symbol(ctx, symtable, Symbol(node->identifier->value, node));
          if(!symbol_res) {
            return ANTON_MOV(symbol_res);
          }
        } break;

        case ast::Node_Kind::stmt_block: {
          ast::Stmt_Block const* const node = static_cast<ast::Stmt_Block const*>(stmt);
          anton::Expected<void, Error> result =
            defcheck_statements(ctx, symtable, node->statements);
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        case ast::Node_Kind::stmt_if: {
          ast::Stmt_If const* const node = static_cast<ast::Stmt_If const*>(stmt);
          if(anton::Expected<void, Error> res = defcheck_expression(ctx, symtable, node->condition);
             !res) {
            return ANTON_MOV(res);
          }

          if(anton::Expected<void, Error> res =
               defcheck_statements(ctx, symtable, node->then_branch);
             !res) {
            return ANTON_MOV(res);
          }

          if(anton::Expected<void, Error> res =
               defcheck_statements(ctx, symtable, node->else_branch);
             !res) {
            return ANTON_MOV(res);
          }
        } break;

        case ast::Node_Kind::stmt_loop: {
          ast::Stmt_Loop const* const node = static_cast<ast::Stmt_Loop const*>(stmt);
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
          ast::Stmt_Switch const* const node = static_cast<ast::Stmt_Switch const*>(stmt);
          anton::Expected<void, Error> switch_expr_result =
            defcheck_expression(ctx, symtable, node->expression);
          if(!switch_expr_result) {
            return ANTON_MOV(switch_expr_result);
          }

          for(ast::Switch_Arm const* const arm: node->arms) {
            anton::Expected<void, Error> result =
              defcheck_statements(ctx, symtable, arm->statements);
            if(!result) {
              return ANTON_MOV(result);
            }
          }
        } break;

        case ast::Node_Kind::stmt_return: {
          ast::Stmt_Return const* const node = static_cast<ast::Stmt_Return const*>(stmt);
          if(node->expression) {
            anton::Expected<void, Error> result =
              defcheck_expression(ctx, symtable, node->expression);
            if(!result) {
              return ANTON_MOV(result);
            }
          }
        } break;

        case ast::Node_Kind::stmt_expression: {
          ast::Stmt_Expression const* const node = static_cast<ast::Stmt_Expression const*>(stmt);
          anton::Expected<void, Error> result =
            defcheck_expression(ctx, symtable, node->expression);
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
  defcheck_struct(Context& ctx, Scoped_Symbol_Table& symtable, ast::Decl_Struct const* const d)
  {
    for(ast::Struct_Member const* const member: d->members) {
      anton::Expected<void, Error> result = defcheck_type(ctx, symtable, member->type);
      if(!result) {
        return ANTON_MOV(result);
      }
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  defcheck_function(Context& ctx, Scoped_Symbol_Table& symtable, ast::Decl_Function const* const fn)
  {
    // We do not defcheck the identifier of the function because it has
    // already been added as an overloaded function.

    anton::Expected<void, Error> return_result = defcheck_type(ctx, symtable, fn->return_type);
    if(!return_result) {
      return ANTON_MOV(return_result);
    }

    // Push a new scope for the function body and parameters.
    symtable.push_scope();
    for(ast::Fn_Parameter const* const parameter: fn->parameters) {
      anton::Expected<void, Error> type_result = defcheck_type(ctx, symtable, parameter->type);
      if(!type_result) {
        return ANTON_MOV(type_result);
      }

      anton::Expected<void, Error> symbol_result =
        check_and_add_symbol(ctx, symtable, Symbol(parameter->identifier->value, parameter));
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
  defcheck_stage_function(Context& ctx, Scoped_Symbol_Table& symtable,
                          ast::Decl_Stage_Function const* const fn)
  {
    anton::Expected<void, Error> return_result = defcheck_type(ctx, symtable, fn->return_type);
    if(!return_result) {
      return ANTON_MOV(return_result);
    }

    // Push a new scope for the function body and parameters.
    symtable.push_scope();
    for(ast::Fn_Parameter const* const parameter: fn->parameters) {
      anton::Expected<void, Error> type_result = defcheck_type(ctx, symtable, parameter->type);
      if(!type_result) {
        return ANTON_MOV(type_result);
      }

      anton::Expected<void, Error> symbol_result =
        check_and_add_symbol(ctx, symtable, Symbol(parameter->identifier->value, parameter));
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

  anton::Expected<void, Error> run_ast_defcheck_pass(Context& ctx, ast::Node_List const ast)
  {
    Scoped_Symbol_Table symtable(ctx.allocator);
    symtable.push_scope();
    // First add all global symbols to the symtab...
    for(ast::Node const* const decl: ast) {
      switch(decl->node_kind) {
        case ast::Node_Kind::variable: {
          ast::Variable const* const d = static_cast<ast::Variable const*>(decl);
          anton::Expected<void, Error> result =
            check_and_add_symbol(ctx, symtable, Symbol(d->identifier->value, d));
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        case ast::Node_Kind::decl_struct: {
          ast::Decl_Struct const* const d = static_cast<ast::Decl_Struct const*>(decl);
          anton::Expected<void, Error> result =
            check_and_add_symbol(ctx, symtable, Symbol(d->identifier->value, d));
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        case ast::Node_Kind::decl_overloaded_function: {
          ast::Decl_Overloaded_Function const* const d =
            static_cast<ast::Decl_Overloaded_Function const*>(decl);
          anton::Expected<void, Error> result =
            check_and_add_symbol(ctx, symtable, Symbol(d->identifier, d));
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

    for(ast::Node const* const node: ast) {
      if(node->node_kind == ast::Node_Kind::decl_struct) {
        ast::Decl_Struct const* const decl = static_cast<ast::Decl_Struct const*>(node);
        anton::Expected<void, Error> result = defcheck_struct(ctx, symtable, decl);
        if(!result) {
          return ANTON_MOV(result);
        }

        ctx.add_type_definition(decl->identifier->value, decl);
      } else if(node->node_kind == ast::Node_Kind::decl_overloaded_function) {
        ast::Decl_Overloaded_Function const* const ofn =
          static_cast<ast::Decl_Overloaded_Function const*>(node);
        for(ast::Decl_Function const* const fn: ofn->overloads) {
          anton::Expected<void, Error> result = defcheck_function(ctx, symtable, fn);
          if(!result) {
            return ANTON_MOV(result);
          }
        }
      } else if(node->node_kind == ast::Node_Kind::decl_stage_function) {
        ast::Decl_Stage_Function const* const fn =
          static_cast<ast::Decl_Stage_Function const* const>(node);
        anton::Expected<void, Error> result = defcheck_stage_function(ctx, symtable, fn);
        if(!result) {
          return ANTON_MOV(result);
        }
      }
    }

    return anton::expected_value;
  }
} // namespace vush
