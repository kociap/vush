#include <vush_typecheck/typecheck.hpp>

#include <anton/algorithm.hpp>
#include <anton/pair.hpp>

#include <vush_ast/ast.hpp>
#include <vush_autogen/builtin_symbols.hpp>
#include <vush_core/context.hpp>
#include <vush_diagnostics/diagnostics.hpp>
#include <vush_typecheck/diagnostics.hpp>
#include <vush_typecheck/typeconv.hpp>

namespace vush {
  using namespace anton::literals;

  // select_overload
  //
  [[nodiscard]] static anton::Expected<ast::Decl_Function*, Error>
  select_overload(Context& ctx, ast::Expr_Call* const call, ast::Overload_Group const* const group);

  // evaluate_expression_type
  // Evaluate the type of an expression node. Recursively descend the hierarchy evaluating types of
  // all expressions in the tree. To prevent costly reevaluation, evaluated types are maintained in
  // a cache within Context.
  //
  [[nodiscard]] static anton::Expected<ast::Type const*, Error>
  evaluate_expression_type(Context& ctx, ast::Expr* const node);

  // evaluate_vector_field
  //
  [[nodiscard]] static anton::Expected<ast::Type const*, Error>
  evaluate_vector_field(Context const& ctx, ast::Type_Builtin const& type,
                        ast::Identifier const& field);

  // evaluate_matrix_field
  //
  [[nodiscard]] static anton::Expected<ast::Type const*, Error>
  evaluate_matrix_field(Context const& ctx, ast::Type_Builtin const& type,
                        ast::Identifier const& field);

  [[nodiscard]] static anton::Expected<void, Error>
  evaluate_vector_initializer(Context& ctx, ast::Type_Builtin const& type,
                              ast::Initializer const& generic_initializer)
  {
    ANTON_ASSERT(is_vector(type), "type is not vector");
    ANTON_ASSERT(generic_initializer.node_kind == ast::Node_Kind::named_initializer,
                 "vector initializer is not a field initializer");
    // Match the type of the swizzle initializer against the type of the expression.
    auto const initializer = static_cast<ast::Named_Initializer const&>(generic_initializer);
    auto initializer_result = evaluate_expression_type(ctx, initializer.expression);
    if(!initializer_result) {
      return {anton::expected_error, ANTON_MOV(initializer_result.error())};
    }

    // Validate out swizzles larger than the vector type.
    i64 const field_size = initializer.identifier.value.size_bytes();
    if(is_vector2(type) && field_size > 2) {
      return {anton::expected_error,
              err_vector_swizzle_overlong(ctx, &type, initializer.identifier)};
    }

    if(is_vector3(type) && field_size > 3) {
      return {anton::expected_error,
              err_vector_swizzle_overlong(ctx, &type, initializer.identifier)};
    }

    if(is_vector4(type) && field_size > 4) {
      return {anton::expected_error,
              err_vector_swizzle_overlong(ctx, &type, initializer.identifier)};
    }

    auto field_result = evaluate_vector_field(ctx, type, initializer.identifier);
    if(!field_result) {
      return {anton::expected_error, ANTON_MOV(field_result.error())};
    }

    ast::Type const* const field_type = field_result.value();
    ast::Type const* const initializer_type = initializer_result.value();
    if(is_convertible(field_type, initializer_type)) {
      return anton::expected_value;
    } else {
      return {anton::expected_error,
              err_cannot_convert_type(ctx, initializer.expression->source_info, field_type,
                                      initializer_type)};
    }
  }

  anton::Expected<ast::Type const*, Error> evaluate_vector_field(Context const& ctx,
                                                                 ast::Type_Builtin const& type,
                                                                 ast::Identifier const& field)
  {
    ANTON_ASSERT(is_vector(type), "type is not vector");
    // Vectors have swizzle members of sizes 1 (scalar), 2 (vec2), 3 (vec3), 4 (vec4). The set of
    // allowed characters is { x, y, z, w, s, t, u, v, r, g, b, a }.
    anton::String_View const value = field.value;
    i64 const size = value.size_bytes();
    if(size > 4 || size < 1) {
      // Less than 1 check for sanity.
      return {anton::expected_error, err_vector_swizzle_overlong(ctx, &type, field)};
    }

    for(char8 const c: value.bytes()) {
      bool const is_allowed = c == 'x' | c == 'y' | c == 'z' | c == 'w' | c == 's' | c == 't' |
                              c == 'u' | c == 'v' | c == 'r' | c == 'g' | c == 'b' | c == 'a';
      if(!is_allowed) {
        return {anton::expected_error, err_vector_swizzle_invalid(ctx, field)};
      }
    }

    if(is_bool_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_bool)};
      case 2:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_bvec2)};
      case 3:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_bvec3)};
      case 4:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_bvec4)};
      }
    }

    if(is_i32_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_int)};
      case 2:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_ivec2)};
      case 3:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_ivec3)};
      case 4:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_ivec4)};
      }
    }

    if(is_u32_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_uint)};
      case 2:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_uvec2)};
      case 3:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_uvec3)};
      case 4:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_uvec4)};
      }
    }

    if(is_f32_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_float)};
      case 2:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_vec2)};
      case 3:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_vec3)};
      case 4:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_vec4)};
      }
    }

    if(is_f64_vector(type)) {
      switch(size) {
      case 1:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_double)};
      case 2:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_dvec2)};
      case 3:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_dvec3)};
      case 4:
        return {anton::expected_value, get_builtin_type(ast::Type_Builtin_Kind::e_dvec4)};
      }
    }

    return {anton::expected_error, err_unimplemented(ctx, field.source_info, __FILE__, __LINE__)};
  }

  [[nodiscard]] static anton::Expected<void, Error>
  evaluate_matrix_initializer(Context& ctx, ast::Type_Builtin const& type,
                              ast::Initializer const& generic_initializer)
  {
    ANTON_ASSERT(is_matrix(type), "type is not matrix");
    ANTON_ASSERT(generic_initializer.node_kind == ast::Node_Kind::named_initializer ||
                   generic_initializer.node_kind == ast::Node_Kind::indexed_initializer,
                 "matrix initializer is not a field or range initializer");

    switch(generic_initializer.node_kind) {
    case ast::Node_Kind::named_initializer: {
      auto initializer = static_cast<ast::Named_Initializer const&>(generic_initializer);
      auto initializer_result = evaluate_expression_type(ctx, initializer.expression);
      if(!initializer_result) {
        return {anton::expected_error, ANTON_MOV(initializer_result.error())};
      }

      auto field_result = evaluate_matrix_field(ctx, type, initializer.identifier);
      if(!field_result) {
        return {anton::expected_error, ANTON_MOV(field_result.error())};
      }

      ast::Type const* field_type = field_result.value();
      ast::Type const* initializer_type = initializer_result.value();
      if(is_convertible(field_type, initializer_type)) {
        return anton::expected_value;
      } else {
        return {anton::expected_error,
                err_cannot_convert_type(ctx, initializer.expression->source_info, field_type,
                                        initializer_type)};
      }
    }

    case ast::Node_Kind::indexed_initializer: {
      return {anton::expected_error,
              err_unimplemented(ctx, generic_initializer.source_info, __FILE__, __LINE__)};
    }

    default:
      return {anton::expected_error,
              err_unimplemented(ctx, generic_initializer.source_info, __FILE__, __LINE__)};
    }
  }

  anton::Expected<ast::Type const*, Error> evaluate_matrix_field(Context const& ctx,
                                                                 ast::Type_Builtin const& type,
                                                                 ast::Identifier const& field)
  {
    ANTON_ASSERT(is_matrix(type), "type is not matrix");
    // Matrices have fields named matX or matYxZ where X, Y, Z in {2, 3, 4}.

    auto select_kind_from_field =
      [](Context const& ctx, ast::Identifier const* const field,
         bool const is_f32) -> anton::Expected<ast::Type_Builtin_Kind, Error> {
      using Kind = ast::Type_Builtin_Kind;
      switch(anton::hash(field->value)) {
      case anton::hash("mat2"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat2 : Kind::e_dmat2};
      case anton::hash("mat2x3"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat2x3 : Kind::e_dmat2x3};
      case anton::hash("mat2x4"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat2x4 : Kind::e_dmat2x4};
      case anton::hash("mat3x2"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat3x2 : Kind::e_dmat3x2};
      case anton::hash("mat3"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat3 : Kind::e_dmat3};
      case anton::hash("mat3x4"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat3x4 : Kind::e_dmat3x4};
      case anton::hash("mat4x2"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat4x2 : Kind::e_dmat4x2};
      case anton::hash("mat4x3"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat4x3 : Kind::e_dmat4x3};
      case anton::hash("mat4"_sv):
        return {anton::expected_value, is_f32 ? Kind::e_mat4 : Kind::e_dmat4};
      default:
        return {anton::expected_error, err_matrix_field_invalid(ctx, field)};
      }
    };

    bool const is_f32 = is_f32_matrix(type);
    auto result = select_kind_from_field(ctx, &field, is_f32);
    if(!result) {
      return {anton::expected_error, ANTON_MOV(result.error())};
    }

    ast::Type_Builtin_Kind const field_kind = result.value();
    return {anton::expected_value, get_builtin_type(field_kind)};
  }

  anton::Expected<ast::Decl_Function*, Error>
  select_overload(Context& ctx, ast::Expr_Call* const call, ast::Overload_Group const* const group)
  {
    Array<ast::Decl_Function*> candidates(ctx.allocator);
    i64 best_score = anton::limits::maximum_i64;
    for(ast::Decl_Function* const fn: group->overloads) {
      if(fn->parameters.size() != call->arguments.size()) {
        continue;
      }

      bool viable = true;
      i64 overload_rank = 0;
      anton::Zip_Iterator begin{call->arguments.begin(), fn->parameters.begin()};
      anton::Zip_Iterator end{call->arguments.end(), fn->parameters.end()};
      for(auto const [argument, parameter]: anton::Range(ANTON_MOV(begin), ANTON_MOV(end))) {
        anton::Expected<ast::Type const*, Error> eval_result =
          evaluate_expression_type(ctx, argument);
        if(!eval_result) {
          return {anton::expected_error, ANTON_MOV(eval_result.error())};
        }

        ast::Type const* const type = eval_result.value();
        anton::Optional<i64> rank_result = rank_conversion(parameter->type, type);
        if(!rank_result) {
          viable = false;
          break;
        }

        overload_rank += rank_result.value();
      }

      if(viable) {
        if(overload_rank < best_score) {
          best_score = overload_rank;
          candidates.clear();
        }

        if(overload_rank == best_score) {
          candidates.push_back(fn);
        }
      }
    }

    if(candidates.size() == 0) {
      return {anton::expected_error, err_no_matching_overload(ctx, call, group->overloads)};
    }

    if(candidates.size() > 1) {
      return {anton::expected_error, err_ambiguous_overload(ctx, call, candidates)};
    }

    return {anton::expected_value, candidates[0]};
  }

  anton::Expected<ast::Type const*, Error> evaluate_expression_type(Context& ctx,
                                                                    ast::Expr* const generic_expr)
  {
    if(generic_expr->evaluated_type) {
      return {anton::expected_value, generic_expr->evaluated_type};
    }

    switch(generic_expr->node_kind) {
    case ast::Node_Kind::lt_bool: {
      ast::Type const* const type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
      generic_expr->evaluated_type = type;
      return {anton::expected_value, type};
    } break;

    case ast::Node_Kind::lt_integer: {
      ast::Lt_Integer const* const lt = static_cast<ast::Lt_Integer const*>(generic_expr);
      switch(lt->kind) {
      case ast::Lt_Integer_Kind::i32: {
        ast::Type const* const type = get_builtin_type(ast::Type_Builtin_Kind::e_int);
        generic_expr->evaluated_type = type;
        return {anton::expected_value, type};
      }

      case ast::Lt_Integer_Kind::u32: {
        ast::Type const* const type = get_builtin_type(ast::Type_Builtin_Kind::e_uint);
        generic_expr->evaluated_type = type;
        return {anton::expected_value, type};
      }
      }
    } break;

    case ast::Node_Kind::lt_float: {
      ast::Lt_Float const* const lt = static_cast<ast::Lt_Float const*>(generic_expr);
      switch(lt->kind) {
      case ast::Lt_Float_Kind::f32: {
        ast::Type const* const type = get_builtin_type(ast::Type_Builtin_Kind::e_float);
        generic_expr->evaluated_type = type;
        return {anton::expected_value, type};
      }

      case ast::Lt_Float_Kind::f64: {
        ast::Type const* const type = get_builtin_type(ast::Type_Builtin_Kind::e_double);
        generic_expr->evaluated_type = type;
        return {anton::expected_value, type};
      }
      }
    } break;

    case ast::Node_Kind::expr_identifier: {
      auto const expr = static_cast<ast::Expr_Identifier const*>(generic_expr);
      ast::Node const* const definition = expr->definition;
      switch(definition->node_kind) {
      case ast::Node_Kind::variable: {
        ast::Variable const* const variable = static_cast<ast::Variable const*>(definition);
        ast::Type const* const type = variable->type;
        generic_expr->evaluated_type = type;
        return {anton::expected_value, type};
      }

      case ast::Node_Kind::fn_parameter: {
        ast::Fn_Parameter const* const parameter =
          static_cast<ast::Fn_Parameter const*>(definition);
        ast::Type const* const type = parameter->type;
        generic_expr->evaluated_type = type;
        return {anton::expected_value, type};
      }

      default:
        // Validated in the validation pass.
        ANTON_ASSERT(false, "invalid identifier definition kind");
        ANTON_UNREACHABLE();
      }
    } break;

    case ast::Node_Kind::expr_call: {
      auto const expr = static_cast<ast::Expr_Call*>(generic_expr);
      ast::Overload_Group const* const group = expr->overload_group;
      ANTON_ASSERT(group != nullptr, "expr_call has no group");
      // Always evaluate all arguments before doing overload resolution. The types are cached, so there is no risk of
      // reevaluation or a major performance penalty, but this way we ensure that all expressions have their types
      // evaluated.
      for(ast::Expr* const argument: expr->arguments) {
        anton::Expected<ast::Type const*, Error> result = evaluate_expression_type(ctx, argument);
        if(!result) {
          return ANTON_MOV(result);
        }
      }

      anton::Expected<ast::Decl_Function*, Error> result = select_overload(ctx, expr, group);
      if(!result) {
        return {anton::expected_error, ANTON_MOV(result.error())};
      }

      auto const fn = result.value();
      expr->function = fn;
      ast::Type const* const type = fn->return_type;
      generic_expr->evaluated_type = type;
      return {anton::expected_value, type};
    } break;

    case ast::Node_Kind::expr_init: {
      ast::Expr_Init const* const expr = static_cast<ast::Expr_Init const*>(generic_expr);

      // TODO: Separate diagnostics for each type kind.
      ast::Type const* generic_type = expr->type;
      switch(generic_type->node_kind) {
      case ast::Node_Kind::type_builtin: {
        auto const type = static_cast<ast::Type_Builtin const*>(generic_type);
        if(is_vector(*type)) {
          for(ast::Initializer const* const initializer: expr->initializers) {
            auto result = evaluate_vector_initializer(ctx, *type, *initializer);
            if(!result) {
              return {anton::expected_error, ANTON_MOV(result.error())};
            }
          }
        } else if(is_matrix(*type)) {
          for(ast::Initializer const* const initializer: expr->initializers) {
            auto result = evaluate_matrix_initializer(ctx, *type, *initializer);
            if(!result) {
              return {anton::expected_error, ANTON_MOV(result.error())};
            }
          }
        } else {
          return {anton::expected_error, err_init_type_is_builtin(ctx, type)};
        }
      } break;

      case ast::Node_Kind::type_struct: {
        ast::Type_Struct const* const type = static_cast<ast::Type_Struct const*>(generic_type);
        ast::Decl_Struct const* const decl = type->definition;
        ast::Member_List::iterator const members_begin = decl->members.begin();
        ast::Member_List::iterator const members_end = decl->members.end();
        for(ast::Initializer const* const generic_initializer: expr->initializers) {
          ANTON_ASSERT(generic_initializer->node_kind == ast::Node_Kind::named_initializer,
                       "struct initializer is not named_initializer");
          ast::Named_Initializer const* const initializer =
            static_cast<ast::Named_Initializer const*>(generic_initializer);
          anton::String_View const identifier = initializer->identifier.value;
          ast::Member_List::iterator const member = anton::find_if(
            members_begin, members_end, [identifier](ast::Struct_Member const* const member) {
              return member->identifier.value == identifier;
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

          ast::Type const* const field_type = (*member)->type;
          ast::Type const* const initializer_type = result.value();
          if(!is_convertible(field_type, initializer_type)) {
            return {anton::expected_error,
                    err_cannot_convert_type(ctx, initializer->expression->source_info, field_type,
                                            initializer_type)};
          }
        }
      } break;

      case ast::Node_Kind::type_array: {
        // TODO: Implement
        return {anton::expected_error,
                err_unimplemented(ctx, expr->source_info, __FILE__, __LINE__)};
      } break;

      default:
        ANTON_ASSERT(false, "invalid generic_expr kind");
        ANTON_UNREACHABLE();
      }

      generic_expr->evaluated_type = expr->type;
      return {anton::expected_value, expr->type};
    } break;

    case ast::Node_Kind::expr_assignment: {
      // TODO: We have to verify that the type we are assigning to is not an opaque type
      //       or a struct with opaque types.
      ast::Expr_Assignment const* const expr =
        static_cast<ast::Expr_Assignment const*>(generic_expr);
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
      if(is_convertible(lhs_type, rhs_type)) {
        generic_expr->evaluated_type = lhs_type;
        return {anton::expected_value, lhs_type};
      } else {
        return {anton::expected_error, err_no_assignment_operator(ctx, rhs_type, lhs_type, expr)};
      }
    } break;

    case ast::Node_Kind::expr_if: {
      ast::Expr_If const* const expr = static_cast<ast::Expr_If const*>(generic_expr);
      anton::Expected<ast::Type const*, Error> condition_result =
        evaluate_expression_type(ctx, expr->condition);
      if(!condition_result) {
        return ANTON_MOV(condition_result);
      }

      ast::Type const* const condition_type = condition_result.value();
      ast::Type const* const bool_type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
      if(!is_convertible(bool_type, condition_type)) {
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
      // 'then' and 'else' types must always be equal. Otherwise, there is no reliable way for us to convert the types.
      if(!compare_types_equal(*then_type, *else_type)) {
        return {anton::expected_error,
                err_incompatible_if_expression_types(ctx, then_type, expr->then_branch, else_type,
                                                     expr->else_branch)};
      }

      generic_expr->evaluated_type = then_type;
      return {anton::expected_value, then_type};
    } break;

    case ast::Node_Kind::expr_index: {
      ast::Expr_Index const* const expr = static_cast<ast::Expr_Index const*>(generic_expr);
      anton::Expected<ast::Type const*, Error> base_result =
        evaluate_expression_type(ctx, expr->base);
      if(!base_result) {
        return ANTON_MOV(base_result);
      }

      ast::Type const* const generic_base_type = base_result.value();
      if(!is_array(*generic_base_type)) {
        return {anton::expected_error,
                err_expression_is_not_indexable(ctx, generic_base_type, expr->base)};
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

      auto const base_type = static_cast<ast::Type_Array const*>(generic_base_type);
      generic_expr->evaluated_type = base_type->base;
      return {anton::expected_value, base_type->base};
    } break;

    case ast::Node_Kind::expr_field: {
      ast::Expr_Field const* const expr = static_cast<ast::Expr_Field const*>(generic_expr);
      anton::Expected<ast::Type const*, Error> result = evaluate_expression_type(ctx, expr->base);
      if(!result) {
        return ANTON_MOV(result);
      }

      // Arrays have no members. Builtin types with the exception of vectors and matrices do not
      // have members.
      // TODO: Separate diagnostics for each type kind.
      ast::Type const* generic_type = result.value();
      switch(generic_type->node_kind) {
      case ast::Node_Kind::type_builtin: {
        auto const type = static_cast<ast::Type_Builtin const*>(generic_type);
        if(is_vector(*type)) {
          auto field_result = evaluate_vector_field(ctx, *type, expr->member);
          if(field_result) {
            generic_expr->evaluated_type = field_result.value();
          }
          return ANTON_MOV(field_result);
        } else if(is_matrix(*type)) {
          auto field_result = evaluate_matrix_field(ctx, *type, expr->member);
          if(field_result) {
            generic_expr->evaluated_type = field_result.value();
          }
          return ANTON_MOV(field_result);
        } else {
          return {anton::expected_error,
                  err_builtin_type_has_no_member_named(ctx, generic_type, expr->member)};
        }
      } break;

      case ast::Node_Kind::type_struct: {
        ast::Type_Struct const* const type = static_cast<ast::Type_Struct const*>(generic_type);
        ast::Decl_Struct const* const decl = type->definition;
        for(ast::Struct_Member const* const member: decl->members) {
          if(member->identifier.value != expr->member.value) {
            continue;
          }

          generic_expr->evaluated_type = member->type;
          return {anton::expected_value, member->type};
        }

        return {anton::expected_error,
                err_type_has_no_field_named(ctx, generic_type, expr->member)};
      } break;

      case ast::Node_Kind::type_array: {
        return {anton::expected_error,
                err_type_has_no_field_named(ctx, generic_type, expr->member)};
      } break;

      default:
        ANTON_ASSERT(false, "invalid generic_expr kind");
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

          ast::Type const* const initializer_type = result.value();
          if(!is_convertible(node->type, initializer_type)) {
            return {anton::expected_error,
                    err_cannot_convert_type(ctx, node->initializer->source_info, node->type,
                                            initializer_type)};
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
        ast::Type const* const bool_type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
        if(!is_convertible(bool_type, condition_type)) {
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
        ast::Type const* const bool_type = get_builtin_type(ast::Type_Builtin_Kind::e_bool);
        if(!is_convertible(bool_type, condition_type)) {
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
        ast::Type const* const i32_type = get_builtin_type(ast::Type_Builtin_Kind::e_int);
        ast::Type const* const u32_type = get_builtin_type(ast::Type_Builtin_Kind::e_uint);
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
    for(ast::Node* const node: ast) {
      switch(node->node_kind) {
      case ast::Node_Kind::decl_function: {
        auto const fn = static_cast<ast::Decl_Function*>(node);
        anton::Expected<void, Error> result = typecheck_statements(ctx, fn->body);
        if(!result) {
          return ANTON_MOV(result);
        }
      } break;

      case ast::Node_Kind::decl_stage_function: {
        auto const fn = static_cast<ast::Decl_Stage_Function*>(node);
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
