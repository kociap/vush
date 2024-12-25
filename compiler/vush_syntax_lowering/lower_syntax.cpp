#include <vush_syntax_lowering/lower_syntax.hpp>

#include <anton/expected.hpp>
#include <anton/intrinsics.hpp>
#include <anton/optional.hpp>

#include <vush_ast/ast.hpp>
#include <vush_autogen/syntax_accessors.hpp>
#include <vush_core/context.hpp>
#include <vush_core/memory.hpp>
#include <vush_diagnostics/diagnostics.hpp>
#include <vush_lexer/lexer.hpp>
#include <vush_parser/parser.hpp>

namespace vush {
  using namespace anton::literals;

#define RETURN_ON_FAIL(variable, fn, ...)                        \
  auto variable = fn(__VA_ARGS__);                               \
  if(!variable) {                                                \
    return {anton::expected_error, ANTON_MOV(variable.error())}; \
  }


  [[nodiscard]] static ast::Identifier transform_identifier(Context const& ctx,
                                                            SNOT const* token)
  {
    anton::String const* const value =
      VUSH_ALLOCATE(anton::String, ctx.bump_allocator, token->get_value(),
                    ctx.bump_allocator);
    return ast::Identifier{*value, token->source_info};
  }

  [[nodiscard]] static anton::Expected<ast::Lt_Integer*, Error>
  lower_lt_integer(Context const& ctx, SNOT const* const node)
  {
    SNOT const* const value_token = get_expr_lt_integer_value(node);
    // TODO: Integer literals must require at most 32 bits.
    // TODO: Literals may include leading 0s.
    // TODO: Validate overflow.
    // The max allowed value is 4294967295
    i32 base = 0;
    switch(value_token->kind) {
    case SNOT_Kind::lt_bin_integer: {
      base = 2;
    } break;

    case SNOT_Kind::lt_dec_integer: {
      base = 10;
    } break;

    case SNOT_Kind::lt_hex_integer: {
      base = 16;
    } break;

    default:
      // TODO: Error
      ANTON_UNREACHABLE("unreachable");
    }

    // The default integer literal type is i32.
    ast::Lt_Integer_Kind kind = ast::Lt_Integer_Kind::i32;
    if(SNOT const* const suffix_token = get_expr_lt_integer_suffix(node)) {
      anton::String_View const suffix = suffix_token->get_value();
      if(suffix == "u"_sv || suffix == "U"_sv) {
        kind = ast::Lt_Integer_Kind::u32;
      } else {
        return {anton::expected_error,
                err_invalid_integer_suffix(ctx, suffix_token->source_info)};
      }
    }

    switch(kind) {
    case ast::Lt_Integer_Kind::i32: {
      i32 const value = anton::str_to_i64(value_token->get_value(), base);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Lt_Integer, ctx.bump_allocator,
                            ast::lt_integer_i32, value, node->source_info)};
    }

    case ast::Lt_Integer_Kind::u32: {
      u32 const value = anton::str_to_u64(value_token->get_value(), base);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Lt_Integer, ctx.bump_allocator,
                            ast::lt_integer_u32, value, node->source_info)};
    }
    }
  }

  [[nodiscard]] static anton::Expected<ast::Type*, Error>
  transform_type(Context const& ctx, SNOT const* const node)
  {
    switch(node->kind) {
    case SNOT_Kind::type_named: {
      ast::Qualifiers qualifiers;
      if(auto const mut = get_type_named_mut(node)) {
        qualifiers.mut = true;
      }

      SNOT const* const value_token = get_type_named_value(node);
      anton::Optional<ast::Type_Builtin_Kind> enumified =
        ast::enumify_builtin_type_kind(value_token->get_value());
      if(enumified) {
        ast::Type_Builtin_Kind const kind = enumified.value();
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.bump_allocator,
                              node->source_info, qualifiers, kind)};
      } else {
        return {anton::expected_value,
                VUSH_ALLOCATE(
                  ast::Type_Struct, ctx.bump_allocator, node->source_info,
                  qualifiers,
                  anton::String(value_token->get_value(), ctx.bump_allocator))};
      }
    } break;

    case SNOT_Kind::type_array: {
      ast::Qualifiers qualifiers;
      if(auto const mut = get_type_array_mut(node)) {
        qualifiers.mut = true;
      }

      SNOT const* const base_node = get_type_array_base(node);
      RETURN_ON_FAIL(base, transform_type, ctx, base_node);
      ast::Lt_Integer* size = nullptr;
      if(auto const size_node = get_type_array_size(node)) {
        RETURN_ON_FAIL(size_result, lower_lt_integer, ctx, size_node);
        size = size_result.value();
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Type_Array, ctx.bump_allocator,
                            node->source_info, qualifiers, base.value(), size)};
    } break;

    default:
      ANTON_UNREACHABLE("invalid type");
    }
  }

  [[nodiscard]] static anton::String_View
  get_operator_identifier_string(SNOT_Kind const kind)
  {
    switch(kind) {
    case SNOT_Kind::tk_langle:
      return "operator<"_sv;
    case SNOT_Kind::tk_rangle:
      return "operator>"_sv;
    case SNOT_Kind::tk_plus:
      return "operator+"_sv;
    case SNOT_Kind::tk_minus:
      return "operator-"_sv;
    case SNOT_Kind::tk_asterisk:
      return "operator*"_sv;
    case SNOT_Kind::tk_slash:
      return "operator/"_sv;
    case SNOT_Kind::tk_percent:
      return "operator%"_sv;
    case SNOT_Kind::tk_amp:
      return "operator&"_sv;
    case SNOT_Kind::tk_pipe:
      return "operator|"_sv;
    case SNOT_Kind::tk_hat:
      return "operator^"_sv;
    case SNOT_Kind::tk_amp2:
      return "operator&&"_sv;
    case SNOT_Kind::tk_pipe2:
      return "operator||"_sv;
    case SNOT_Kind::tk_hat2:
      return "operator^^"_sv;
    case SNOT_Kind::tk_shl:
      return "operator<<"_sv;
    case SNOT_Kind::tk_shr:
      return "operator>>"_sv;
    case SNOT_Kind::tk_eq2:
      return "operator=="_sv;
    case SNOT_Kind::tk_neq:
      return "operator!="_sv;
    case SNOT_Kind::tk_lteq:
      return "operator<="_sv;
    case SNOT_Kind::tk_gteq:
      return "operator>="_sv;
    case SNOT_Kind::tk_bang:
      return "operator!"_sv;
    case SNOT_Kind::tk_tilde:
      return "operator~"_sv;
    default:
      ANTON_UNREACHABLE("invalid syntax node kind");
    }
  }

  [[nodiscard]] static anton::Expected<ast::Expr*, Error>
  transform_expr(Context const& ctx, SNOT const* const node)
  {
    switch(node->kind) {
    case SNOT_Kind::expr_if: {
      SNOT const* const condition_node = get_expr_if_condition(node);
      RETURN_ON_FAIL(condition, transform_expr, ctx, condition_node);
      SNOT const* const then_expr_node =
        get_expr_block_expression(get_expr_if_then_branch(node));
      RETURN_ON_FAIL(then_branch, transform_expr, ctx, then_expr_node);
      SNOT const* const else_expr_node =
        get_expr_block_expression(get_expr_if_else_branch(node));
      anton::Expected<ast::Expr*, Error> else_branch =
        transform_expr(ctx, else_expr_node);
      if(!else_branch) {
        return {anton::expected_error, ANTON_MOV(else_branch.error())};
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_If, ctx.bump_allocator, condition.value(),
                            then_branch.value(), else_branch.value(),
                            node->source_info)};
    } break;

    case SNOT_Kind::expr_identifier: {
      SNOT const* const value_token = get_expr_identifier_value(node);
      anton::String const* const value =
        VUSH_ALLOCATE(anton::String, ctx.bump_allocator,
                      value_token->get_value(), ctx.bump_allocator);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Identifier, ctx.bump_allocator, *value,
                            node->source_info)};
    } break;

    case SNOT_Kind::expr_binary: {
      SNOT const* const lhs_node = get_expr_binary_lhs(node);
      RETURN_ON_FAIL(lhs, transform_expr, ctx, lhs_node);
      SNOT const* const rhs_node = get_expr_binary_rhs(node);
      RETURN_ON_FAIL(rhs, transform_expr, ctx, rhs_node);

      SNOT const* const operator_token = get_expr_binary_operator(node);
      anton::String_View const identifier_string =
        get_operator_identifier_string(operator_token->kind);
      ast::Identifier const identifier{identifier_string,
                                       operator_token->source_info};
      ast::Expr_List arguments;
      arguments.insert_back(lhs.value());
      arguments.insert_back(rhs.value());
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Call, ctx.bump_allocator, identifier,
                            ANTON_MOV(arguments), node->source_info)};
    } break;

    case SNOT_Kind::expr_prefix: {
      SNOT const* const expression_node = get_expr_prefix_expression(node);
      RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
      SNOT const* const operator_token = get_expr_prefix_operator(node);
      anton::String_View const identifier_string =
        get_operator_identifier_string(operator_token->kind);
      ast::Identifier const identifier{identifier_string,
                                       operator_token->source_info};
      ast::Expr_List arguments;
      arguments.insert_back(expression.value());
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Call, ctx.bump_allocator, identifier,
                            ANTON_MOV(arguments), node->source_info)};
    } break;

    case SNOT_Kind::expr_field: {
      SNOT const* const expression_node = get_expr_field_expression(node);
      RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
      SNOT const* const identifier_token = get_expr_field_identifier(node);
      ast::Identifier const identifier =
        transform_identifier(ctx, identifier_token);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Field, ctx.bump_allocator,
                            expression.value(), identifier, node->source_info)};
    } break;

    case SNOT_Kind::expr_index: {
      SNOT const* const expression_node = get_expr_index_expression(node);
      RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
      SNOT const* const index_node = get_expr_index_index(node);
      RETURN_ON_FAIL(index, transform_expr, ctx, index_node);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Index, ctx.bump_allocator,
                            expression.value(), index.value(),
                            node->source_info)};
    } break;

    case SNOT_Kind::expr_parentheses: {
      SNOT const* const expression_node = get_expr_parentheses_expression(node);
      return transform_expr(ctx, expression_node);
    } break;

    case SNOT_Kind::expr_reinterpret: {
      // TODO: implement.
      ANTON_UNREACHABLE("unimplemented");
    } break;

    case SNOT_Kind::expr_init: {
      auto transform_initializer =
        [](
          Context const& ctx,
          SNOT const* const node) -> anton::Expected<ast::Initializer*, Error> {
        switch(node->kind) {
        case SNOT_Kind::field_initializer: {
          SNOT const* const identifier_token =
            get_field_initializer_identifier(node);
          ast::Identifier const identifier =
            transform_identifier(ctx, identifier_token);

          SNOT const* const expression_node =
            get_field_initializer_expression(node);
          RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
          return {anton::expected_value,
                  VUSH_ALLOCATE(ast::Field_Initializer, ctx.bump_allocator,
                                identifier, expression.value(),
                                node->source_info)};
        } break;

        case SNOT_Kind::index_initializer: {
          SNOT const* const index_node = get_index_initializer_index(node);
          RETURN_ON_FAIL(index, lower_lt_integer, ctx, index_node);
          SNOT const* const expression_node =
            get_index_initializer_expression(node);
          RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
          return {anton::expected_value,
                  VUSH_ALLOCATE(ast::Index_Initializer, ctx.bump_allocator,
                                index.value(), expression.value(),
                                node->source_info)};
        } break;

        case SNOT_Kind::basic_initializer: {
          SNOT const* const expression_node =
            get_basic_initializer_expression(node);
          RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
          return {anton::expected_value,
                  VUSH_ALLOCATE(ast::Basic_Initializer, ctx.bump_allocator,
                                expression.value(), node->source_info)};
        } break;

        default:
          ANTON_UNREACHABLE("invalid syntax node kind");
        }
      };

      SNOT const* const type_node = get_expr_init_type(node);
      anton::Expected<ast::Type*, Error> type = transform_type(ctx, type_node);
      if(!type) {
        return {anton::expected_error, ANTON_MOV(type.error())};
      }

      SNOT const* const initializers_node = get_expr_init_initializers(node);
      ast::Initializer_List initializers;
      for(SNOT const* snot = initializers_node->children; snot != nullptr;
          snot = anton::ilist_next(snot)) {
        if(!snot->is_node()) {
          continue;
        }

        RETURN_ON_FAIL(initializer, transform_initializer, ctx, snot);
        initializers.insert_back(initializer.value());
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Init, ctx.bump_allocator, type.value(),
                            ANTON_MOV(initializers), node->source_info)};
    } break;

    case SNOT_Kind::expr_call: {
      SNOT const* const identifier_token = get_expr_call_identifier(node);
      ast::Identifier const identifier =
        transform_identifier(ctx, identifier_token);

      SNOT const* const arguments_node = get_expr_call_arguments(node);
      ast::Expr_List arguments;
      for(SNOT const* snot = arguments_node->children; snot != nullptr;
          snot = anton::ilist_next(snot)) {
        if(!snot->is_node()) {
          continue;
        }

        RETURN_ON_FAIL(expression, transform_expr, ctx, snot);
        arguments.insert_back(expression.value());
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Call, ctx.bump_allocator, identifier,
                            ANTON_MOV(arguments), node->source_info)};
    } break;

    case SNOT_Kind::expr_lt_bool: {
      SNOT const* const value_token = get_expr_lt_bool_value(node);
      bool const value = value_token->get_value() == "true"_sv;
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Lt_Bool, ctx.bump_allocator, value,
                            node->source_info)};
    } break;

    case SNOT_Kind::expr_lt_string: {
      // There are no string literals in the language besides the ones used by
      // decl_import which we have special handling for in place, therefore we
      // leave this unimplemented.
      ANTON_UNREACHABLE("unimplemented");
    } break;

    case SNOT_Kind::expr_lt_float: {
      SNOT const* const value_token = get_expr_lt_float_value(node);
      // TODO: Validate float string token.
      // The default float literal type is f32.
      if(SNOT const* const suffix_token = get_expr_lt_float_suffix(node)) {
        anton::String_View const suffix = suffix_token->get_value();
        if(suffix != "d"_sv && suffix != "D"_sv) {
          return {anton::expected_error,
                  err_invalid_float_suffix(ctx, suffix_token->source_info)};
        }

        f64 const value = anton::str_to_f64(
          anton::String(value_token->get_value(), ctx.bump_allocator));
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Lt_Float, ctx.bump_allocator,
                              ast::lt_float_f64, value, node->source_info)};
      } else {
        f32 const value = anton::str_to_f32(
          anton::String(value_token->get_value(), ctx.bump_allocator));
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Lt_Float, ctx.bump_allocator,
                              ast::lt_float_f32, value, node->source_info)};
      }
    } break;

    case SNOT_Kind::expr_lt_integer: {
      RETURN_ON_FAIL(result, lower_lt_integer, ctx, node);
      return {anton::expected_value, result.value()};
    } break;

    case SNOT_Kind::expr_default: {
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Default, ctx.bump_allocator,
                            node->source_info)};
    } break;

    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  [[nodiscard]] static ast::Assignment_Kind
  map_token_to_assignment_kind(SNOT_Kind const kind)
  {
    switch(kind) {
    case SNOT_Kind::tk_equals:
      return ast::Assignment_Kind::e_assign;
    case SNOT_Kind::tk_pluseq:
      return ast::Assignment_Kind::e_add;
    case SNOT_Kind::tk_minuseq:
      return ast::Assignment_Kind::e_sub;
    case SNOT_Kind::tk_asteriskeq:
      return ast::Assignment_Kind::e_mul;
    case SNOT_Kind::tk_slasheq:
      return ast::Assignment_Kind::e_div;
    case SNOT_Kind::tk_percenteq:
      return ast::Assignment_Kind::e_mod;
    case SNOT_Kind::tk_ampeq:
      return ast::Assignment_Kind::e_and;
    case SNOT_Kind::tk_pipeeq:
      return ast::Assignment_Kind::e_or;
    case SNOT_Kind::tk_hateq:
      return ast::Assignment_Kind::e_xor;
    case SNOT_Kind::tk_shleq:
      return ast::Assignment_Kind::e_shl;
    case SNOT_Kind::tk_shreq:
      return ast::Assignment_Kind::e_shr;
    default:
      ANTON_UNREACHABLE("invalid token kind");
    }
  }

  [[nodiscard]] static anton::Expected<ast::Attr_List, Error>
  transform_attribute_list(Context const& ctx, SNOT const* const node)
  {
    if(node->children == nullptr) {
      return {anton::expected_value, ast::Attr_List{}};
    }

    ast::Attr_List attributes;
    for(SNOT const* attribute_node = node->children; attribute_node != nullptr;
        attribute_node = anton::ilist_next(attribute_node)) {
      anton::IList<ast::Attribute_Parameter> parameters;
      if(SNOT const* const parameter_list_node =
           get_attribute_parameter_list(attribute_node)) {
        for(SNOT const* parameter = parameter_list_node->children;
            parameter != nullptr; parameter = anton::ilist_next(parameter)) {
          if(!parameter->is_node()) {
            continue;
          }

          ANTON_ASSERT(
            parameter->kind == SNOT_Kind::attribute_parameter_positional ||
              parameter->kind == SNOT_Kind::attribute_parameter_keyed,
            "Syntax_Node is not an attribute_parameter_positional "
            "or attribute_parameter_keyed");
          ast::Identifier key;
          ast::Expr* value = nullptr;
          if(parameter->kind == SNOT_Kind::attribute_parameter_keyed) {
            SNOT const* const key_node =
              get_attribute_parameter_keyed_key(parameter);
            key = transform_identifier(ctx, key_node);
            SNOT const* const value_node =
              get_attribute_parameter_keyed_value(parameter);
            RETURN_ON_FAIL(value_result, transform_expr, ctx, value_node);
            value = value_result.value();
          } else {
            SNOT const* const value_node =
              get_attribute_parameter_positional_value(parameter);
            RETURN_ON_FAIL(value_result, transform_expr, ctx, value_node);
            value = value_result.value();
          }
          parameters.insert_back(VUSH_ALLOCATE(ast::Attribute_Parameter,
                                               ctx.bump_allocator, key, value));
        }
      }

      ast::Identifier const identifier =
        transform_identifier(ctx, get_attribute_identifier(attribute_node));
      attributes.insert_back(VUSH_ALLOCATE(ast::Attribute, ctx.bump_allocator,
                                           identifier, ANTON_MOV(parameters),
                                           attribute_node->source_info));
    }
    return {anton::expected_value, ANTON_MOV(attributes)};
  }

  // transform_stmt
  //
  // Returns:
  // nullptr if the statement does not have a representation in the AST.
  //
  [[nodiscard]] static anton::Expected<ast::Node*, Error>
  transform_stmt(Context const& ctx, SNOT const* const node);
  [[nodiscard]] static anton::Expected<ast::Stmt_List, Error>
  transform_stmt_block_child_stmts(Context const& ctx, SNOT const* const node);

  anton::Expected<ast::Stmt_List, Error>
  transform_stmt_block_child_stmts(Context const& ctx, SNOT const* const node)
  {
    ANTON_ASSERT(node->kind == SNOT_Kind::stmt_block,
                 "Syntax_Node is not stmt_block");
    ast::Stmt_List statements;
    for(SNOT const* snot = node->children; snot != nullptr;
        snot = anton::ilist_next(snot)) {
      if(!snot->is_node()) {
        continue;
      }

      RETURN_ON_FAIL(stmt, transform_stmt, ctx, snot);
      if(stmt.value() != nullptr) {
        statements.insert_back(stmt.value());
      }
    }
    return {anton::expected_value, ANTON_MOV(statements)};
  }

  [[nodiscard]] static anton::Expected<ast::Variable*, Error>
  transform_variable(Context const& ctx, SNOT const* const node)
  {
    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_variable_attribute_list(node));

    ast::Identifier const identifier =
      transform_identifier(ctx, get_variable_identifier(node));
    RETURN_ON_FAIL(type, transform_type, ctx, get_variable_type(node));

    ast::Expr* initializer = nullptr;
    if(SNOT const* const initializer_node = get_variable_initializer(node)) {
      RETURN_ON_FAIL(result, transform_expr, ctx, initializer_node);
      initializer = result.value();
    }
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Variable, ctx.bump_allocator,
                          ANTON_MOV(attribute_list.value()), type.value(),
                          identifier, initializer, node->source_info)};
  }

  anton::Expected<ast::Node*, Error> transform_stmt(Context const& ctx,
                                                    SNOT const* const node)
  {
    switch(node->kind) {
    case SNOT_Kind::variable: {
      RETURN_ON_FAIL(variable, transform_variable, ctx, node);
      return {anton::expected_value, variable.value()};
    } break;

    case SNOT_Kind::stmt_block: {
      RETURN_ON_FAIL(result, transform_stmt_block_child_stmts, ctx, node);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Block, ctx.bump_allocator,
                            ANTON_MOV(result.value()), node->source_info)};
    } break;

    case SNOT_Kind::stmt_assignment: {
      RETURN_ON_FAIL(lhs, transform_expr, ctx, get_stmt_assignment_lhs(node));
      RETURN_ON_FAIL(rhs, transform_expr, ctx, get_stmt_assignment_rhs(node));
      SNOT const* const operator_token = get_stmt_assignment_operator(node);
      ast::Assignment_Kind const kind =
        map_token_to_assignment_kind(operator_token->kind);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Assignment, ctx.bump_allocator, kind,
                            lhs.value(), rhs.value(), node->source_info)};
    } break;

    case SNOT_Kind::stmt_if: {
      RETURN_ON_FAIL(condition, transform_expr, ctx,
                     get_stmt_if_condition(node));
      RETURN_ON_FAIL(then_branch, transform_stmt_block_child_stmts, ctx,
                     get_stmt_if_then_branch(node));
      ast::Stmt_List else_branch;
      if(SNOT const* const else_node = get_stmt_if_else_branch(node)) {
        if(else_node->kind == SNOT_Kind::stmt_block) {
          RETURN_ON_FAIL(result, transform_stmt_block_child_stmts, ctx,
                         else_node);
          else_branch = ANTON_MOV(result.value());
        } else {
          RETURN_ON_FAIL(result, transform_stmt, ctx, else_node);
          else_branch.insert_back(result.value());
        }
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_If, ctx.bump_allocator, condition.value(),
                            ANTON_MOV(then_branch.value()),
                            ANTON_MOV(else_branch), node->source_info)};
    } break;

    case SNOT_Kind::stmt_switch: {
      RETURN_ON_FAIL(expression, transform_expr, ctx,
                     get_stmt_switch_expression(node));
      anton::IList<ast::Switch_Arm> arm_list;
      {
        SNOT const* const arm_list_node = get_stmt_switch_arm_list(node);
        for(SNOT const* arm_node = arm_list_node->children; arm_node != nullptr;
            arm_node = anton::ilist_next(arm_node)) {
          if(!arm_node->is_node()) {
            continue;
          }

          ast::Expr_List labels;
          for(SNOT const* snot = arm_node->children; snot != nullptr;
              snot = anton::ilist_next(snot)) {
            if(snot->kind == SNOT_Kind::switch_arm_label) {
              SNOT const* const expression_node = snot->children;
              RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
              labels.insert_back(expression.value());
            }
          }

          RETURN_ON_FAIL(statements, transform_stmt_block_child_stmts, ctx,
                         get_switch_arm_body(arm_node));
          ast::Switch_Arm* const arm =
            VUSH_ALLOCATE(ast::Switch_Arm, ctx.bump_allocator,
                          ANTON_MOV(labels), ANTON_MOV(statements.value()));
          arm_list.insert_back(arm);
        }
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Switch, ctx.bump_allocator,
                            expression.value(), ANTON_MOV(arm_list),
                            node->source_info)};
    } break;

    case SNOT_Kind::stmt_for: {
      ast::Variable_List declarations;
      if(auto const variable_node = get_stmt_for_variable(node)) {
        RETURN_ON_FAIL(variable, transform_variable, ctx, variable_node);
        declarations.insert_back(variable.value());
      }

      ast::Expr* condition = nullptr;
      if(auto const condition_node = get_stmt_for_condition(node)) {
        RETURN_ON_FAIL(result, transform_expr, ctx, condition_node);
        condition = result.value();
      }

      ast::Expr_List actions;
      if(auto expression_node = get_stmt_for_expression(node)) {
        RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
        actions.insert_back(expression.value());
      }

      RETURN_ON_FAIL(statements, transform_stmt_block_child_stmts, ctx,
                     get_stmt_for_body(node));

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_For, ctx.bump_allocator, condition,
                            ANTON_MOV(declarations), ANTON_MOV(actions),
                            ANTON_MOV(statements.value()), node->source_info)};
    } break;

    case SNOT_Kind::stmt_while: {
      RETURN_ON_FAIL(condition, transform_expr, ctx,
                     get_stmt_while_condition(node));
      RETURN_ON_FAIL(statements, transform_stmt_block_child_stmts, ctx,
                     get_stmt_while_statements(node));
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_While, ctx.bump_allocator,
                            condition.value(), ANTON_MOV(statements.value()),
                            node->source_info)};
    } break;

    case SNOT_Kind::stmt_do_while: {
      RETURN_ON_FAIL(statements, transform_stmt_block_child_stmts, ctx,
                     get_stmt_do_while_body(node));
      RETURN_ON_FAIL(condition, transform_expr, ctx,
                     get_stmt_do_while_condition(node));
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Do_While, ctx.bump_allocator,
                            condition.value(), ANTON_MOV(statements.value()),
                            node->source_info)};
    } break;

    case SNOT_Kind::stmt_return: {
      ast::Expr* expression = nullptr;
      if(auto const expression_node = get_stmt_return_expression(node)) {
        RETURN_ON_FAIL(result, transform_expr, ctx, expression_node);
        expression = result.value();
      }
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Return, ctx.bump_allocator, expression,
                            node->source_info)};
    } break;

    case SNOT_Kind::stmt_break: {
      return {
        anton::expected_value,
        VUSH_ALLOCATE(ast::Stmt_Break, ctx.bump_allocator, node->source_info)};
    } break;

    case SNOT_Kind::stmt_continue: {
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Continue, ctx.bump_allocator,
                            node->source_info)};
    } break;

    case SNOT_Kind::stmt_discard: {
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Discard, ctx.bump_allocator,
                            node->source_info)};
    } break;

    case SNOT_Kind::stmt_expression: {
      RETURN_ON_FAIL(expression, transform_expr, ctx,
                     get_stmt_expression_expression(node));
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Expression, ctx.bump_allocator,
                            expression.value(), node->source_info)};
    } break;

    case SNOT_Kind::stmt_empty: {
      return {anton::expected_value, nullptr};
    } break;

    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Struct*, Error>
  transform_decl_struct(Context const& ctx, SNOT const* const node)
  {
    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_decl_struct_attribute_list(node));

    ast::Struct_Field_List members;
    SNOT const* const members_node = get_decl_struct_fields(node);
    for(SNOT const* member_node = members_node->children;
        member_node != nullptr; member_node = anton::ilist_next(member_node)) {
      if(!member_node->is_node()) {
        continue;
      }

      RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                     get_struct_field_attribute_list(member_node));
      ast::Identifier const identifier =
        transform_identifier(ctx, get_struct_field_identifier(member_node));
      RETURN_ON_FAIL(type, transform_type, ctx,
                     get_struct_field_type(member_node));
      ast::Expr* initializer = nullptr;
      auto const initializer_node = get_struct_field_initializer(member_node);
      if(initializer_node) {
        RETURN_ON_FAIL(result, transform_expr, ctx, initializer_node);
        initializer = result.value();
      }

      members.insert_back(VUSH_ALLOCATE(ast::Struct_Field, ctx.bump_allocator,
                                        ANTON_MOV(attribute_list.value()),
                                        identifier, type.value(), initializer));
    }

    ast::Identifier const identifier =
      transform_identifier(ctx, get_decl_struct_identifier(node));
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Decl_Struct, ctx.bump_allocator,
                          ANTON_MOV(attribute_list.value()), identifier,
                          ANTON_MOV(members), node->source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Buffer*, Error>
  transform_decl_buffer(Context const& ctx, SNOT const* const node)
  {
    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_decl_buffer_attribute_list(node));

    ast::Buffer_Field_List fields;
    SNOT const* const fields_node = get_decl_buffer_fields(node);
    for(SNOT const* field_node = fields_node->children; field_node != nullptr;
        field_node = anton::ilist_next(field_node)) {
      if(!field_node->is_node()) {
        continue;
      }

      RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                     get_buffer_field_attribute_list(field_node));

      ast::Identifier const identifier =
        transform_identifier(ctx, get_buffer_field_identifier(field_node));
      RETURN_ON_FAIL(type, transform_type, ctx,
                     get_buffer_field_type(field_node));
      fields.insert_back(VUSH_ALLOCATE(ast::Buffer_Field, ctx.bump_allocator,
                                       ANTON_MOV(attribute_list.value()),
                                       identifier, type.value()));
    }

    ast::Identifier const identifier =
      transform_identifier(ctx, get_decl_buffer_identifier(node));
    ast::Identifier const pass =
      transform_identifier(ctx, get_decl_buffer_pass(node));
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Decl_Buffer, ctx.bump_allocator,
                          ANTON_MOV(attribute_list.value()), pass, identifier,
                          ANTON_MOV(fields), node->source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Fn_Parameter*, Error>
  transform_parameter(Context const& ctx, SNOT const* const node)
  {
    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_fn_parameter_attribute_list(node));
    ast::Identifier const identifier =
      transform_identifier(ctx, get_fn_parameter_identifier(node));
    RETURN_ON_FAIL(type, transform_type, ctx, get_fn_parameter_type(node));
    ast::Identifier source;
    if(auto const result = get_fn_parameter_source(node)) {
      source = transform_identifier(ctx, result);
    }

    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Fn_Parameter, ctx.bump_allocator,
                          ANTON_MOV(attribute_list.value()), identifier,
                          type.value(), source, node->source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Fn_Parameter_List, Error>
  transform_parameter_list(Context const& ctx, SNOT const* const node)
  {
    ast::Fn_Parameter_List parameters;
    for(SNOT const* snot = node->children; snot != nullptr;
        snot = anton::ilist_next(snot)) {
      if(!snot->is_node()) {
        continue;
      }

      RETURN_ON_FAIL(parameter, transform_parameter, ctx, snot);
      parameters.insert_back(parameter.value());
    }
    return {anton::expected_value, ANTON_MOV(parameters)};
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Function*, Error>
  transform_decl_function(Context const& ctx, SNOT const* const node)
  {
    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_decl_function_attribute_list(node));
    ast::Identifier const identifier =
      transform_identifier(ctx, get_decl_function_identifier(node));
    RETURN_ON_FAIL(parameters, transform_parameter_list, ctx,
                   get_decl_function_parameter_list(node));
    RETURN_ON_FAIL(return_type, transform_type, ctx,
                   get_decl_function_return_type(node));
    RETURN_ON_FAIL(body, transform_stmt_block_child_stmts, ctx,
                   get_decl_function_body(node));
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Decl_Function, ctx.bump_allocator,
                          ANTON_MOV(attribute_list.value()), identifier,
                          ANTON_MOV(parameters.value()), return_type.value(),
                          ANTON_MOV(body.value()), false, node->source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Stage_Function*, Error>
  transform_decl_stage_function(Context const& ctx, SNOT const* const node)
  {
    auto transform_stage_kind =
      [](SNOT const* const token) -> ast::With_Source<Stage_Kind> {
      if(token->get_value() == "vertex"_sv) {
        return {Stage_Kind::vertex, token->source_info};
      } else if(token->get_value() == "fragment"_sv) {
        return {Stage_Kind::fragment, token->source_info};
      } else if(token->get_value() == "compute"_sv) {
        return {Stage_Kind::compute, token->source_info};
      } else {
        // TODO: Error
        ANTON_UNREACHABLE("unreachable");
      }
    };

    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_decl_stage_function_attribute_list(node));
    ast::Identifier const pass =
      transform_identifier(ctx, get_decl_stage_function_pass(node));
    ast::With_Source<Stage_Kind> const stage =
      transform_stage_kind(get_decl_stage_function_stage(node));
    RETURN_ON_FAIL(parameters, transform_parameter_list, ctx,
                   get_decl_stage_function_parameter_list(node));
    RETURN_ON_FAIL(body, transform_stmt_block_child_stmts, ctx,
                   get_decl_stage_function_body(node));
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Decl_Stage_Function, ctx.bump_allocator,
                          ANTON_MOV(attribute_list.value()), pass, stage,
                          ANTON_MOV(parameters.value()),
                          ANTON_MOV(body.value()), node->source_info)};
  }

  anton::Expected<anton::IList<ast::Node>, Error> lower_syntax(Context& ctx,
                                                               SNOT* snot)
  {
    anton::IList<ast::Node> nodes;
    for(; snot != nullptr; snot = anton::ilist_next(snot)) {
      if(!snot->is_node()) {
        continue;
      }

      switch(snot->kind) {
      case SNOT_Kind::variable: {
        RETURN_ON_FAIL(result, transform_variable, ctx, snot);
        nodes.insert_back(result.value());
      } break;

      case SNOT_Kind::decl_struct: {
        RETURN_ON_FAIL(result, transform_decl_struct, ctx, snot);
        ast::Decl_Struct* const decl = result.value();
        nodes.insert_back(decl);
      } break;

      case SNOT_Kind::decl_buffer: {
        RETURN_ON_FAIL(result, transform_decl_buffer, ctx, snot);
        ast::Decl_Buffer* const decl = result.value();
        nodes.insert_back(decl);
      } break;

      case SNOT_Kind::decl_settings: {
        // TODO
      } break;

      case SNOT_Kind::decl_function: {
        RETURN_ON_FAIL(result, transform_decl_function, ctx, snot);
        ast::Decl_Function* const decl = result.value();
        nodes.insert_back(decl);
      } break;

      case SNOT_Kind::decl_stage_function: {
        RETURN_ON_FAIL(result, transform_decl_stage_function, ctx, snot);
        ast::Decl_Stage_Function* const decl = result.value();
        nodes.insert_back(decl);
      } break;

      case SNOT_Kind::decl_if:
      case SNOT_Kind::decl_import:
      default:
        ANTON_UNREACHABLE("unreachable");
      }
    }
    return {anton::expected_value, ANTON_MOV(nodes)};
  }
} // namespace vush
