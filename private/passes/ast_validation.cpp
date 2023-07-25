#include <passes.hpp>

#include <anton/algorithm/sort.hpp>
#include <anton/flat_hash_map.hpp>

#include <ast.hpp>
#include <context.hpp>
#include <diagnostics.hpp>

namespace vush {
  using namespace anton::literals;

  enum Strong_Ordering {
    less = -1,
    equal,
    greater,
  };

  // compare_integer_literals
  // Three-way compare integer literals' value regardless of their base. The literal values must be
  // trimmed to contain only plus or minus and digits.
  //
  // Returns:
  // Ordering of the numeric values of the literals.
  //
  [[nodiscard]] static Strong_Ordering compare_integer_literals(ast::Lt_Integer const* const lhs,
                                                                ast::Lt_Integer const* const rhs)
  {
    i64 const lhs_value = anton::str_to_i64(lhs->value, static_cast<i32>(lhs->base));
    i64 const rhs_value = anton::str_to_i64(rhs->value, static_cast<i32>(rhs->base));
    if(lhs_value < rhs_value) {
      return Strong_Ordering::less;
    } else if(lhs_value == rhs_value) {
      return Strong_Ordering::equal;
    } else {
      return Strong_Ordering::greater;
    }
  }

  [[nodiscard]] static anton::Expected<void, Error> check_array_is_sized(Context const& ctx,
                                                                         ast::Type const& type)
  {
    if(ast::is_unsized_array(type)) {
      return {anton::expected_error, err_unsized_array_not_allowed(ctx, type.source_info)};
    }

    if(type.node_kind == ast::Node_Kind::type_array) {
      ast::Type_Array const& t = static_cast<ast::Type_Array const&>(type);
      return check_array_is_sized(ctx, *t.base);
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error> validate_expression(Context const& ctx,
                                                                        ast::Node const* const node)
  {
    switch(node->node_kind) {
      case ast::Node_Kind::expr_init: {
        // Initialization rules:
        // - Builtin types: we do not allow initialization of builtin types with the exception of
        //   vectors which have members x, y, z, w and alternatives, and matrices.
        // - Struct types: we allow initialization of struct types using only named initializers.
        // - Array types: we allow initialization of struct types using only indexed or basic
        //   initializers. Usage of both kinds of initializers in one initialization is disallowed.
        // Duplicate initializers are not allowed.

        ast::Expr_Init const* const expr = static_cast<ast::Expr_Init const*>(node);
        switch(expr->type->node_kind) {
          case ast::Node_Kind::type_builtin: {
            if(!is_vector(*expr->type) && !is_matrix(*expr->type)) {
              return {anton::expected_error, err_init_type_is_builtin(ctx, expr->type)};
            }

            return anton::expected_value;
          } break;

          case ast::Node_Kind::type_user_defined: {
            Array<ast::Identifier const*> identifiers{ctx.allocator};
            for(ast::Initializer const* const generic_initializer: expr->initializers) {
              if(generic_initializer->node_kind != ast::Node_Kind::named_initializer) {
                return {anton::expected_error,
                        err_init_invalid_struct_initializer_kind(ctx, generic_initializer)};
              }

              ast::Named_Initializer const* const initializer =
                static_cast<ast::Named_Initializer const*>(generic_initializer);
              anton::Expected<void, Error> result =
                validate_expression(ctx, initializer->expression);
              if(!result) {
                return ANTON_MOV(result);
              }

              identifiers.push_back(initializer->identifier);
            }

            // Ensure there are no duplicate identifiers.
            if(identifiers.size() > 0) {
              // We use a stable sort to ensure the duplicates are reported in the correct order.
              anton::merge_sort(
                identifiers.begin(), identifiers.end(),
                [](ast::Identifier const* const v1, ast::Identifier const* const v2) {
                  return compare(v1->value, v2->value) == -1;
                });

              for(auto i = identifiers.begin(), j = identifiers.begin() + 1, e = identifiers.end();
                  j != e; ++i, ++j) {
                bool const equal = compare((*i)->value, (*j)->value);
                if(equal) {
                  Source_Info const& src1 = (*i)->source_info;
                  Source_Info const& src2 = (*j)->source_info;
                  return {anton::expected_error, err_duplicate_label(ctx, src1, src2)};
                }
              }
            }

            return anton::expected_value;
          } break;

          case ast::Node_Kind::type_array: {
            Array<ast::Lt_Integer const*> indices{ctx.allocator};
            ast::Indexed_Initializer const* indexed_initializer = nullptr;
            ast::Basic_Initializer const* basic_initializer = nullptr;
            for(ast::Initializer const* const generic_initializer: expr->initializers) {
              if(generic_initializer->node_kind != ast::Node_Kind::indexed_initializer &&
                 generic_initializer->node_kind != ast::Node_Kind::basic_initializer) {
                return {anton::expected_error,
                        err_init_invalid_array_initializer_kind(ctx, generic_initializer)};
              }

              if(generic_initializer->node_kind == ast::Node_Kind::indexed_initializer) {
                ast::Indexed_Initializer const* const initializer =
                  static_cast<ast::Indexed_Initializer const*>(generic_initializer);
                if(basic_initializer != nullptr) {
                  return {anton::expected_error,
                          err_init_array_initialization_must_not_have_both_initializer_kinds(
                            ctx, basic_initializer, initializer)};
                }

                indexed_initializer = initializer;

                anton::Expected<void, Error> result =
                  validate_expression(ctx, initializer->expression);
                if(!result) {
                  return ANTON_MOV(result);
                }

                indices.push_back(initializer->index);
              } else {
                ast::Basic_Initializer const* const initializer =
                  static_cast<ast::Basic_Initializer const*>(generic_initializer);
                if(indexed_initializer != nullptr) {
                  return {anton::expected_error,
                          err_init_array_initialization_must_not_have_both_initializer_kinds(
                            ctx, indexed_initializer, initializer)};
                }

                basic_initializer = initializer;

                anton::Expected<void, Error> result =
                  validate_expression(ctx, initializer->expression);
                if(!result) {
                  return ANTON_MOV(result);
                }
              }
            }

            // TODO: Comparison of literals will not work correctly if they
            //       are not trimmed to contain only value. Ensure the
            //       value strings are always trimmed.
            // Ensure there are no duplicate indices.
            if(indices.size() > 0) {
              // We use a stable sort to ensure the duplicates are reported in the correct order.
              anton::merge_sort(
                indices.begin(), indices.end(),
                [](ast::Lt_Integer const* const v1, ast::Lt_Integer const* const v2) {
                  return compare_integer_literals(v1, v2) == Strong_Ordering::less;
                });

              for(auto i = indices.begin(), j = indices.begin() + 1, e = indices.end(); j != e;
                  ++i, ++j) {
                bool const equal = compare_integer_literals(*i, *j);
                if(equal) {
                  Source_Info const& src1 = (*i)->source_info;
                  Source_Info const& src2 = (*j)->source_info;
                  // TODO: Change diagnostic.
                  return {anton::expected_error, err_duplicate_label(ctx, src1, src2)};
                }
              }
            }

            return anton::expected_value;
          } break;

          default:
            ANTON_ASSERT(false, "invalid initializer type");
            ANTON_UNREACHABLE();
        }
      } break;

      case ast::Node_Kind::expr_if: {
        ast::Expr_If const* const expr = static_cast<ast::Expr_If const*>(node);
        anton::Expected<void, Error> condition_result = validate_expression(ctx, expr->condition);
        if(!condition_result) {
          return ANTON_MOV(condition_result);
        }

        anton::Expected<void, Error> then_result = validate_expression(ctx, expr->then_branch);
        if(!then_result) {
          return ANTON_MOV(then_result);
        }

        anton::Expected<void, Error> else_result = validate_expression(ctx, expr->else_branch);
        if(!else_result) {
          return ANTON_MOV(else_result);
        }

        return anton::expected_value;
      } break;

      case ast::Node_Kind::expr_assignment: {
        ast::Expr_Assignment const* const expr = static_cast<ast::Expr_Assignment const*>(node);
        anton::Expected<void, Error> lhs_result = validate_expression(ctx, expr->lhs);
        if(!lhs_result) {
          return ANTON_MOV(lhs_result);
        }

        anton::Expected<void, Error> rhs_result = validate_expression(ctx, expr->rhs);
        if(!rhs_result) {
          return ANTON_MOV(rhs_result);
        }

        return anton::expected_value;
      } break;

      case ast::Node_Kind::expr_call: {
        ast::Expr_Call const* const expr = static_cast<ast::Expr_Call const*>(node);
        for(ast::Expr const* const argument: expr->arguments) {
          anton::Expected<void, Error> result = validate_expression(ctx, argument);
          if(!result) {
            return ANTON_MOV(result);
          }
        }

        return anton::expected_value;
      } break;

      case ast::Node_Kind::expr_member_access: {
        ast::Expr_Member_Access const* const expr =
          static_cast<ast::Expr_Member_Access const*>(node);
        anton::Expected<void, Error> result = validate_expression(ctx, expr->base);
        if(!result) {
          return ANTON_MOV(result);
        }

        return anton::expected_value;
      } break;

      case ast::Node_Kind::expr_index: {
        ast::Expr_Index const* const expr = static_cast<ast::Expr_Index const*>(node);
        anton::Expected<void, Error> base_result = validate_expression(ctx, expr->base);
        if(!base_result) {
          return ANTON_MOV(base_result);
        }

        anton::Expected<void, Error> index_result = validate_expression(ctx, expr->index);
        if(!index_result) {
          return ANTON_MOV(index_result);
        }

        return anton::expected_value;
      } break;

      case ast::Node_Kind::expr_parentheses: {
        ast::Expr_Parentheses const* const expr = static_cast<ast::Expr_Parentheses const*>(node);
        anton::Expected<void, Error> result = validate_expression(ctx, expr->expression);
        if(!result) {
          return ANTON_MOV(result);
        }

        return anton::expected_value;
      } break;

      case ast::Node_Kind::expr_reinterpret: {
        // TODO: Implement.
        return anton::expected_value;
      } break;

      case ast::Node_Kind::lt_integer: {
        ast::Lt_Integer const* const expr = static_cast<ast::Lt_Integer const*>(node);
        // Integer literals must require at most 32 bits.
        // TODO: Literals may include leading 0s.
        // TODO: Validate overflow.
        // The max allowed value is 4294967295
        switch(expr->base) {
          case ast::Lt_Integer_Base::dec: {
          } break;

          case ast::Lt_Integer_Base::bin: {
          } break;

          case ast::Lt_Integer_Base::hex: {
          } break;
        }

        return anton::expected_value;
      } break;

      case ast::Node_Kind::lt_float: {
        // TODO: Implement validation.
        return anton::expected_value;
      } break;

      case ast::Node_Kind::lt_bool:
      case ast::Node_Kind::expr_identifier:
      case ast::Node_Kind::expr_default:
        // No validation.
        return anton::expected_value;

      default:
        ANTON_ASSERT(false, "invalid expression type");
        ANTON_UNREACHABLE();
    }
  }

  enum struct Statement_Context {
    sc_none,
    sc_loop,
    sc_switch,
    sc_continuation,
  };

  [[nodiscard]] static anton::Expected<void, Error>
  validate_statements(Context& ctx, ast::Node_List const statements, Statement_Context const sc)
  {
    for(ast::Node const* const stmt: statements) {
      switch(stmt->node_kind) {
        case ast::Node_Kind::variable: {
          ast::Variable const* const node = static_cast<ast::Variable const*>(stmt);
          if(node->initializer != nullptr) {
            anton::Expected<void, Error> result = validate_expression(ctx, node->initializer);
            if(!result) {
              return ANTON_MOV(result);
            }
          }
        } break;

        case ast::Node_Kind::stmt_block: {
          ast::Stmt_Block const* const node = static_cast<ast::Stmt_Block const*>(stmt);
          anton::Expected<void, Error> result = validate_statements(ctx, node->statements, sc);
          if(!result) {
            return ANTON_MOV(result);
          }
        } break;

        case ast::Node_Kind::stmt_if: {
          ast::Stmt_If const* const node = static_cast<ast::Stmt_If const*>(stmt);
          anton::Expected<void, Error> condition_result = validate_expression(ctx, node->condition);
          if(!condition_result) {
            return ANTON_MOV(condition_result);
          }

          anton::Expected<void, Error> then_result =
            validate_statements(ctx, node->then_branch, sc);
          if(!then_result) {
            return ANTON_MOV(then_result);
          }

          anton::Expected<void, Error> else_result =
            validate_statements(ctx, node->else_branch, sc);
          if(!else_result) {
            return ANTON_MOV(else_result);
          }
        } break;

        case ast::Node_Kind::stmt_loop: {
          ast::Stmt_Loop const* const node = static_cast<ast::Stmt_Loop const*>(stmt);
          if(node->condition != nullptr) {
            anton::Expected<void, Error> condition_result =
              validate_expression(ctx, node->condition);
            if(!condition_result) {
              return ANTON_MOV(condition_result);
            }
          }

          anton::Expected<void, Error> continuation_result =
            validate_statements(ctx, node->continuation, Statement_Context::sc_continuation);
          if(!continuation_result) {
            return ANTON_MOV(continuation_result);
          }

          anton::Expected<void, Error> statements_result =
            validate_statements(ctx, node->statements, Statement_Context::sc_loop);
          if(!statements_result) {
            return ANTON_MOV(statements_result);
          }
        } break;

        case ast::Node_Kind::stmt_switch: {
          ast::Stmt_Switch const* const node = static_cast<ast::Stmt_Switch const*>(stmt);

          if(anton::Expected<void, Error> result = validate_expression(ctx, node->expression);
             !result) {
            return ANTON_MOV(result);
          }

          ast::Expr const* default_label = nullptr;
          Array<ast::Lt_Integer const*> labels{ctx.allocator};
          for(ast::Switch_Arm const* const arm: node->arms) {
            for(ast::Expr const* const label: arm->labels) {
              anton::Expected<void, Error> result = validate_expression(ctx, label);
              if(!result) {
                return ANTON_MOV(result);
              }

              if(label->node_kind == ast::Node_Kind::expr_default) {
                // Ensure the default label is unique.
                if(default_label == nullptr) {
                  default_label = label;
                } else {
                  return {anton::expected_error,
                          err_duplicate_default_label(ctx, default_label->source_info,
                                                      label->source_info)};
                }
              } else if(label->node_kind == ast::Node_Kind::lt_integer) {
                labels.push_back(static_cast<ast::Lt_Integer const*>(label));
              } else {
                Source_Info const& src = label->source_info;
                return {anton::expected_error, err_invalid_switch_arm_expression(ctx, src)};
              }
            }

            anton::Expected<void, Error> result =
              validate_statements(ctx, arm->statements, Statement_Context::sc_switch);
            if(!result) {
              return ANTON_MOV(result);
            }
          }

          // TODO: Comparison of literals will not work correctly if they are not trimmed to contain
          //       only value. Ensure the value strings are always trimmed.
          // Ensure there are no duplicate labels.
          if(labels.size() > 0) {
            // We use a stable sort to ensure the duplicates are reported in the correct order.
            anton::merge_sort(labels.begin(), labels.end(),
                              [](ast::Lt_Integer const* const v1, ast::Lt_Integer const* const v2) {
                                return compare_integer_literals(v1, v2) == Strong_Ordering::less;
                              });
            for(auto i = labels.begin(), j = labels.begin() + 1, e = labels.end(); j != e;
                ++i, ++j) {
              bool const equal = (*i)->value == (*j)->value;
              if(equal) {
                Source_Info const& src1 = (*i)->source_info;
                Source_Info const& src2 = (*j)->source_info;
                return {anton::expected_error, err_duplicate_label(ctx, src1, src2)};
              }
            }
          }
        } break;

        case ast::Node_Kind::stmt_break: {
          if(sc != Statement_Context::sc_loop) {
            return {anton::expected_error, err_break_used_outside_loop(ctx, stmt->source_info)};
          }
        } break;

        case ast::Node_Kind::stmt_continue: {
          if(sc != Statement_Context::sc_loop) {
            return {anton::expected_error, err_continue_used_outside_loop(ctx, stmt->source_info)};
          }
        } break;

        case ast::Node_Kind::stmt_return: {
          ast::Stmt_Return const* const node = static_cast<ast::Stmt_Return const*>(stmt);
          if(node->expression) {
            anton::Expected<void, Error> result = validate_expression(ctx, node->expression);
            if(!result) {
              return ANTON_MOV(result);
            }
          }
        } break;

        case ast::Node_Kind::stmt_expression: {
          ast::Stmt_Expression const* const node = static_cast<ast::Stmt_Expression const*>(stmt);
          if(node->expression) {
            anton::Expected<void, Error> result = validate_expression(ctx, node->expression);
            if(!result) {
              return ANTON_MOV(result);
            }
          }
        } break;

        default:
          // TODO: Verify stmt_discard is in a fragment shader.
          break;
      }
    }

    return anton::expected_value;
  }

  // validate_struct_member_type
  // Validate that a type is a complete type, i.e. not opaque and is not a recursive definition.
  //
  [[nodiscard]] static anton::Expected<void, Error>
  validate_struct_member_type(Context const& ctx, ast::Type const& type,
                              ast::Identifier const& struct_identifier)
  {
    switch(type.node_kind) {
      case ast::Node_Kind::type_builtin: {
        ast::Type_Builtin const& t = static_cast<ast::Type_Builtin const&>(type);
        if(ast::is_opaque_glsl_type(t.value)) {
          return {anton::expected_error, err_opaque_type_in_struct(ctx, t.source_info)};
        }

        return {anton::expected_value};
      } break;

      case ast::Node_Kind::type_user_defined: {
        ast::Type_User_Defined const& t = static_cast<ast::Type_User_Defined const&>(type);
        if(t.value == struct_identifier.value) {
          return {anton::expected_error,
                  err_recursive_type_definition(ctx, struct_identifier.source_info, t.source_info)};
        }

        return anton::expected_value;
      } break;

      case ast::Node_Kind::type_array: {
        ast::Type_Array const& t = static_cast<ast::Type_Array const&>(type);
        return validate_struct_member_type(ctx, *t.base, struct_identifier);
      } break;

      default:
        ANTON_ASSERT(false, "unreachable");
        ANTON_UNREACHABLE();
    }
  }

  [[nodiscard]] static anton::Expected<void, Error>
  validate_struct(Context const& ctx, ast::Decl_Struct const* const dstruct)
  {
    if(dstruct->members.size() == 0) {
      return {anton::expected_error, err_empty_struct(ctx, dstruct->identifier->source_info)};
    }

    // Member types must be complete types and must not be self.
    for(ast::Struct_Member const* const member: dstruct->members) {
      anton::Expected<void, Error> result =
        validate_struct_member_type(ctx, *member->type, *dstruct->identifier);
      if(!result) {
        return result;
      }
    }

    // Member names must be unique.
    {
      anton::Flat_Hash_Map<anton::String_View, ast::Identifier const*> member_identifiers;
      for(ast::Struct_Member const* const member: dstruct->members) {
        auto iter = member_identifiers.find(member->identifier->value);
        if(iter != member_identifiers.end()) {
          return {anton::expected_error,
                  err_duplicate_struct_member(ctx, iter->value->source_info, member->source_info)};
        } else {
          member_identifiers.emplace(member->identifier->value, member->identifier);
        }
      }
    }

    // TODO: Attributes and initializers.
    // noperspective, flat, smooth, invariant

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  validate_constant(Context const& ctx, ast::Variable const* const constant)
  {
    // Constant_Declaration& node = static_cast<Constant_Declaration&>(*ast_node);
    // if(!node.initializer) {
    //   return {anton::expected_error, format_constant_missing_initializer(ctx, node.source_info)};
    // }

    // if(anton::Expected<void, anton::String> res = validate_expression(ctx, node.initializer);
    //    !res) {
    //   return {anton::expected_error, ANTON_MOV(res.error())};
    // }
    return {anton::expected_value};
  }

  [[nodiscard]] static anton::Expected<void, Error>
  validate_function(Context& ctx, ast::Decl_Function const* const fn)
  {
    // Validate attributes. Currently there are no attributes that are not allowed on ordinary functions.
    for(ast::Attribute const* const attribute: fn->attributes) {
      return {anton::expected_error,
              err_illegal_attribute(ctx, attribute->identifier->source_info)};
    }

    // Validate the return type:
    // - if the type is an array, it must be sized.
    {
      anton::Expected<void, Error> result = check_array_is_sized(ctx, *fn->return_type);
      if(!result) {
        return result;
      }
    }

    // Validate parameters:
    // - only ordinary parameters are allowed.
    for(ast::Fn_Parameter const* const p: fn->parameters) {
      if(ast::is_sourced_parameter(*p)) {
        return {anton::expected_error, err_fn_sourced_parameter_not_allowed(ctx, p->source_info)};
      }
    }

    if(anton::Expected<void, Error> result =
         validate_statements(ctx, fn->body, Statement_Context::sc_none);
       !result) {
      return result;
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  validate_stage_function(Context& ctx, ast::Decl_Stage_Function const* const fn)
  {
    // Validate attributes:
    // - compute stage might have the workgroup attribute (at most 1).
    // - other stages must not have any attributes.
    switch(fn->stage.value) {
      case Stage_Kind::compute: {
        ast::Attribute const* workgroup = nullptr;
        for(ast::Attribute const* const attribute: fn->attributes) {
          if(attribute->identifier->value == "workgroup"_sv) {
            if(!workgroup) {
              workgroup = attribute;
            } else {
              return {anton::expected_error,
                      err_duplicate_attribute(ctx, workgroup->identifier->source_info,
                                              attribute->identifier->source_info)};
            }
          } else {
            return {anton::expected_error,
                    err_illegal_attribute(ctx, attribute->identifier->source_info)};
          }
        }
      } break;

      default: {
        for(ast::Attribute const* const attribute: fn->attributes) {
          return {anton::expected_error, err_illegal_attribute(ctx, attribute->source_info)};
        }
      } break;
    }

    // Validate the return type:
    // - vertex: must be builtin or UDT.
    // - fragment: must be builtin or UDT.
    // - compute: must be void.
    {
      bool const void_return = ast::is_void(*fn->return_type);
      bool const builtin_return = fn->return_type->node_kind == ast::Node_Kind::type_builtin;
      bool const udt_return = fn->return_type->node_kind == ast::Node_Kind::type_user_defined;
      switch(fn->stage.value) {
        case Stage_Kind::vertex: {
          if(!builtin_return && !udt_return) {
            return {anton::expected_error,
                    err_stage_return_must_be_builtin_or_udt(
                      ctx, fn->pass->value, fn->stage.source_info, fn->return_type->source_info)};
          }
        } break;

        case Stage_Kind::fragment: {
          if(!builtin_return && !udt_return) {
            return {anton::expected_error,
                    err_stage_return_must_be_builtin_or_udt(
                      ctx, fn->pass->value, fn->stage.source_info, fn->return_type->source_info)};
          }
        } break;

        case Stage_Kind::compute: {
          if(!void_return) {
            return {anton::expected_error, err_compute_return_must_be_void(
                                             ctx, fn->pass->value, fn->return_type->source_info)};
          }
        } break;
      }
    }

    // Validate parameters:
    // - all parameters must be builtin or UDT. Arrays are not supported yet.
    // - vertex: only vertex input parameters and sourced parameters are allowed. vertex input
    //   parameters must not be opaque.
    // - fragment: all parameters must be sourced with the exception of the first one which might be
    //   an ordinary parameter that is used as an input from the previous stage.
    // - compute: only sourced parameters are allowed.
    {
      bool first = true;
      for(ast::Fn_Parameter const* const p: fn->parameters) {
        {
          bool const builtin_type = fn->return_type->node_kind == ast::Node_Kind::type_builtin;
          bool const udt_type = fn->return_type->node_kind == ast::Node_Kind::type_user_defined;
          if(!builtin_type && !udt_type) {
            return {anton::expected_error,
                    err_stage_parameter_must_be_builtin_or_udt(ctx, p->type->source_info)};
          }
        }

        switch(fn->stage.value) {
          case Stage_Kind::vertex: {
            if(!ast::is_sourced_parameter(*p)) {
              return {anton::expected_error,
                      err_vertex_ordinary_parameter_not_allowed(ctx, p->source_info)};
            }

            if(is_vertex_input_parameter(*p)) {
              if(is_opaque_type(*p->type)) {
                return {anton::expected_error,
                        err_vertex_vin_must_not_be_opaque(ctx, p->type->source_info)};
              }

              if(p->type->node_kind == ast::Node_Kind::type_array) {
                return {anton::expected_error,
                        err_vertex_vin_must_not_be_array(ctx, p->type->source_info)};
              }
            }
          } break;

          case Stage_Kind::fragment: {
            bool const ordinary_parameter = !ast::is_sourced_parameter(*p);
            if(!first && ordinary_parameter) {
              return {anton::expected_error,
                      err_fragment_ordinary_parameter_not_allowed(ctx, p->source_info)};
            }

            if(ast::is_vertex_input_parameter(*p)) {
              return {anton::expected_error,
                      err_fragment_vin_not_allowed(ctx, p->source->source_info)};
            }
          } break;

          case Stage_Kind::compute: {
            if(!ast::is_sourced_parameter(*p)) {
              return {anton::expected_error,
                      err_compute_ordinary_parameter_not_allowed(ctx, p->source_info)};
            }

            if(ast::is_vertex_input_parameter(*p)) {
              return {anton::expected_error,
                      err_compute_vin_not_allowed_on_stage(ctx, p->source_info)};
            }
          } break;
        }

        first = false;
      }
    }

    if(anton::Expected<void, Error> result =
         validate_statements(ctx, fn->body, Statement_Context::sc_none);
       !result) {
      return result;
    }

    return anton::expected_value;
  }

  [[nodiscard]] static anton::Expected<void, Error>
  validate_overloaded_function(Context& ctx, ast::Decl_Overloaded_Function const* const fn)
  {
    // Ensure all overloads have different sets of parameters.
    // Compare all with all. O(n^2) comparisons.
    for(i64 i = 0; i < fn->overloads.size(); ++i) {
      ast::Decl_Function const* const overload1 = fn->overloads[i];
      for(i64 j = i + 1; j < fn->overloads.size(); ++j) {
        ast::Decl_Function const* const overload2 = fn->overloads[j];
        if(overload1->parameters.size() != overload2->parameters.size()) {
          continue;
        }

        bool identical = true;
        anton::Zip_Iterator begin{overload1->parameters.begin(), overload2->parameters.begin()};
        anton::Zip_Iterator end{overload1->parameters.end(), overload2->parameters.end()};
        for(auto const [p1, p2]: anton::Range(ANTON_MOV(begin), ANTON_MOV(end))) {
          if(!ast::compare_types_equal(*p1->type, *p2->type)) {
            identical = false;
            break;
          }
        }

        if(identical) {
          if(!ast::compare_types_equal(*overload1->return_type, *overload2->return_type)) {
            return {anton::expected_error,
                    err_overload_on_return_type(
                      ctx, overload1->identifier->source_info, overload1->return_type->source_info,
                      overload2->identifier->source_info, overload2->return_type->source_info)};
          } else {
            return {anton::expected_error,
                    err_symbol_redefinition(ctx, overload1->identifier->source_info,
                                            overload2->identifier->source_info)};
          }
        }
      }
    }
    return {anton::expected_value};
  }

  anton::Expected<void, Error> run_ast_validation_pass(Context& ctx, ast::Node_List const ast)
  {
    // There is yet no support for struct member initializers, however, for future considerations,
    // validating structs and constants separately prevents us from properly validating both.
    // Currently we validate structs first as that seems to be the best solution giving us the
    // option to conduct the most thorough analysis.

    // TODO: The following code results in out-of-order error reporting, i.e. code appearing sooner
    //       in the source might be validated after code appearing later in the source. The problem
    //       stems from the fact that we gather overloads before doing validation and validate
    //       groups of constructs. We cannot possibly move overload gather to after defcheck or
    //       validation because of the dependency of their symbols, therefore we must come up with a
    //       different solution so that we report errors in their order of occurence.

    // Validate structs.
    for(ast::Node const* const decl: ast) {
      if(decl->node_kind != ast::Node_Kind::decl_struct) {
        continue;
      }

      ast::Decl_Struct const* const dstruct = static_cast<ast::Decl_Struct const*>(decl);
      if(anton::Expected<void, Error> result = validate_struct(ctx, dstruct); !result) {
        return {anton::expected_error, ANTON_MOV(result.error())};
      }
    }

    // Validate constants.
    for(ast::Node const* const decl: ast) {
      if(decl->node_kind != ast::Node_Kind::variable) {
        continue;
      }

      ast::Variable const* const constant = static_cast<ast::Variable const*>(decl);
      if(anton::Expected<void, Error> result = validate_constant(ctx, constant); !result) {
        return {anton::expected_error, ANTON_MOV(result.error())};
      }
    }

    // Validate functions.
    for(ast::Node const* const decl: ast) {
      if(decl->node_kind != ast::Node_Kind::decl_overloaded_function) {
        continue;
      }

      ast::Decl_Overloaded_Function const* const ofn =
        static_cast<ast::Decl_Overloaded_Function const*>(decl);
      if(anton::Expected<void, Error> result = validate_overloaded_function(ctx, ofn); !result) {
        return {anton::expected_error, ANTON_MOV(result.error())};
      }

      for(ast::Decl_Function const* const fn: ofn->overloads) {
        if(anton::Expected<void, Error> result = validate_function(ctx, fn); !result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }
      }
    }

    // Validate stage functions.
    for(ast::Node const* const decl: ast) {
      if(decl->node_kind != ast::Node_Kind::decl_stage_function) {
        continue;
      }

      ast::Decl_Stage_Function const* const fn = static_cast<ast::Decl_Stage_Function const*>(decl);
      if(anton::Expected<void, Error> result = validate_stage_function(ctx, fn); !result) {
        return {anton::expected_error, ANTON_MOV(result.error())};
      }
    }

    return {anton::expected_value};
  }
} // namespace vush
