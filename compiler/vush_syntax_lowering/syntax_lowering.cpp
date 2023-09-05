#include <vush_syntax_lowering/syntax_lowering.hpp>

#include <anton/expected.hpp>
#include <anton/intrinsics.hpp>
#include <anton/optional.hpp>

#include <vush.hpp>
#include <vush_ast/ast.hpp>
#include <vush_autogen/syntax_accessors.hpp>
#include <vush_core/context.hpp>
#include <vush_core/memory.hpp>
#include <vush_diagnostics/diagnostics.hpp>
#include <vush_parser/parser.hpp>

namespace vush {
  using namespace anton::literals;

  anton::Expected<ast::Node_List, Error>
  import_source_code(Context& ctx, anton::String_View const source_name,
                     anton::Optional<Source_Info> const source_info)
  {
    anton::Expected<Source_Import_Result, anton::String> source_request_res =
      ctx.source_import_cb(*ctx.allocator, source_name, ctx.source_import_user_data);
    if(!source_request_res) {
      if(source_info) {
        return {anton::expected_error,
                err_source_import_failed(ctx, *source_info, source_request_res.error())};
      } else {
        return {anton::expected_error,
                err_source_import_failed_no_location(ctx, source_request_res.error())};
      }
    }

    Source_Import_Result& request_res = source_request_res.value();
    // Ensure we're not importing the same source multiple times.
    if(ctx.find_source(request_res.source_name) == nullptr) {
      Source_Data source{ANTON_MOV(request_res.source_name), ANTON_MOV(request_res.data)};
      anton::String_View const source_name = source.name;
      anton::String_View const source_data = source.data;
      ctx.add_source(ANTON_MOV(source));

      Parse_Syntax_Options options{.include_whitespace_and_comments = false};
      anton::Expected<Array<SNOT>, Error> parse_result =
        parse_source_to_syntax_tree(ctx, source_name, source_data, options);
      if(!parse_result) {
        return {anton::expected_error, ANTON_MOV(parse_result.error())};
      }

      anton::Expected<ast::Node_List, Error> transform_result =
        lower_syntax_to_ast(ctx, parse_result.value());
      if(!transform_result) {
        return {anton::expected_error, ANTON_MOV(transform_result.error())};
      }

      return {anton::expected_value, ANTON_MOV(transform_result.value())};
    } else {
      return {anton::expected_value, ast::Node_List{}};
    }
  }

  [[nodiscard]] static ast::Identifier transform_identifier(Context const& ctx,
                                                            Syntax_Token const& token)
  {
    anton::String const* const value =
      allocate<anton::String>(ctx.allocator, token.value, ctx.allocator);
    return ast::Identifier{*value, token.source_info};
  }

  [[nodiscard]] static anton::Expected<ast::Lt_Integer const*, Error>
  lower_lt_integer(Context const& ctx, Syntax_Node const& node)
  {
    Syntax_Token const& value_token = get_expr_lt_integer_value(node);
    // TODO: Integer literals must require at most 32 bits.
    // TODO: Literals may include leading 0s.
    // TODO: Validate overflow.
    // The max allowed value is 4294967295
    i32 base = 0;
    switch(value_token.kind) {
    case Syntax_Node_Kind::lt_bin_integer: {
      base = 2;
    } break;

    case Syntax_Node_Kind::lt_dec_integer: {
      base = 10;
    } break;

    case Syntax_Node_Kind::lt_hex_integer: {
      base = 16;
    } break;

    default:
      ANTON_ASSERT(false, ""); // TODO: Error
      ANTON_UNREACHABLE();
    }

    // The default integer literal type is i32.
    ast::Lt_Integer_Kind kind = ast::Lt_Integer_Kind::i32;
    if(anton::Optional<Syntax_Token const&> suffix_token = get_expr_lt_integer_suffix(node)) {
      anton::String_View const suffix = suffix_token->value;
      if(suffix == "u"_sv || suffix == "U"_sv) {
        kind = ast::Lt_Integer_Kind::u32;
      } else {
        return {anton::expected_error, err_invalid_integer_suffix(ctx, suffix_token->source_info)};
      }
    }

    switch(kind) {
    case ast::Lt_Integer_Kind::i32: {
      i32 const value = anton::str_to_i64(value_token.value, base);
      return {anton::expected_value, allocate<ast::Lt_Integer>(ctx.allocator, ast::lt_integer_i32,
                                                               value, node.source_info)};
    }

    case ast::Lt_Integer_Kind::u32: {
      u32 const value = anton::str_to_u64(value_token.value, base);
      return {anton::expected_value, allocate<ast::Lt_Integer>(ctx.allocator, ast::lt_integer_u32,
                                                               value, node.source_info)};
    }
    }
  }

  [[nodiscard]] static anton::Expected<ast::Type const*, Error>
  transform_type(Context const& ctx, Syntax_Node const& node)
  {
    switch(node.kind) {
    case Syntax_Node_Kind::type_named: {
      ast::Qualifiers qualifiers;
      if(anton::Optional mut = get_type_named_mut(node)) {
        qualifiers.mut = true;
      }

      Syntax_Token const& value_token = get_type_named_value(node);
      anton::Optional<ast::Type_Builtin_Kind> enumified =
        ast::enumify_builtin_type_kind(value_token.value);
      if(enumified) {
        return {anton::expected_value,
                allocate<ast::Type_Builtin>(ctx.allocator, qualifiers, enumified.value(),
                                            node.source_info)};
      } else {
        // We allocate the String for the value as the syntax tokens are destroyed at a later stage.
        anton::String const* const value =
          allocate<anton::String>(ctx.allocator, value_token.value, ctx.allocator);
        return {anton::expected_value,
                allocate<ast::Type_Struct>(ctx.allocator, qualifiers, *value, node.source_info)};
      }
    } break;

    case Syntax_Node_Kind::type_array: {
      ast::Qualifiers qualifiers;
      if(anton::Optional mut = get_type_array_mut(node)) {
        qualifiers.mut = true;
      }

      Syntax_Node const& base_node = get_type_array_base(node);
      anton::Expected<ast::Type const*, Error> base = transform_type(ctx, base_node);
      if(!base) {
        return {anton::expected_error, ANTON_MOV(base.error())};
      }

      ast::Lt_Integer const* size = nullptr;
      if(anton::Optional size_node = get_type_array_size(node)) {
        anton::Expected<ast::Lt_Integer const*, Error> size_result =
          lower_lt_integer(ctx, *size_node);
        if(size_result) {
          size = size_result.value();
        } else {
          return {anton::expected_error, ANTON_MOV(size_result.error())};
        }
      }

      return {
        anton::expected_value,
        allocate<ast::Type_Array>(ctx.allocator, qualifiers, base.value(), size, node.source_info)};
    } break;

    default:
      ANTON_ASSERT(false, "invalid type");
      ANTON_UNREACHABLE();
    }
  }

  [[nodiscard]] static anton::String_View
  get_operator_identifier_string(Syntax_Node_Kind const kind)
  {
    switch(kind) {
    case Syntax_Node_Kind::tk_langle:
      return "operator<"_sv;
    case Syntax_Node_Kind::tk_rangle:
      return "operator>"_sv;
    case Syntax_Node_Kind::tk_plus:
      return "operator+"_sv;
    case Syntax_Node_Kind::tk_minus:
      return "operator-"_sv;
    case Syntax_Node_Kind::tk_asterisk:
      return "operator*"_sv;
    case Syntax_Node_Kind::tk_slash:
      return "operator/"_sv;
    case Syntax_Node_Kind::tk_percent:
      return "operator%"_sv;
    case Syntax_Node_Kind::tk_amp:
      return "operator&"_sv;
    case Syntax_Node_Kind::tk_pipe:
      return "operator|"_sv;
    case Syntax_Node_Kind::tk_hat:
      return "operator^"_sv;
    case Syntax_Node_Kind::tk_amp2:
      return "operator&&"_sv;
    case Syntax_Node_Kind::tk_pipe2:
      return "operator||"_sv;
    case Syntax_Node_Kind::tk_hat2:
      return "operator^^"_sv;
    case Syntax_Node_Kind::tk_shl:
      return "operator<<"_sv;
    case Syntax_Node_Kind::tk_shr:
      return "operator>>"_sv;
    case Syntax_Node_Kind::tk_eq2:
      return "operator=="_sv;
    case Syntax_Node_Kind::tk_neq:
      return "operator!="_sv;
    case Syntax_Node_Kind::tk_lteq:
      return "operator<="_sv;
    case Syntax_Node_Kind::tk_gteq:
      return "operator>="_sv;
    case Syntax_Node_Kind::tk_bang:
      return "operator!"_sv;
    case Syntax_Node_Kind::tk_tilde:
      return "operator~"_sv;
      // TODO: Reintroduce.
      // case Syntax_Node_Kind::tk_pluseq:
      //     return ast::Expr_Binary_Kind::assign_add;
      // case Syntax_Node_Kind::tk_minuseq:
      //     return ast::Expr_Binary_Kind::assign_sub;
      // case Syntax_Node_Kind::tk_asteriskeq:
      //     return ast::Expr_Binary_Kind::assign_mul;
      // case Syntax_Node_Kind::tk_slasheq:
      //     return ast::Expr_Binary_Kind::assign_div;
      // case Syntax_Node_Kind::tk_percenteq:
      //     return ast::Expr_Binary_Kind::assign_mod;
      // case Syntax_Node_Kind::tk_ampeq:
      //     return ast::Expr_Binary_Kind::assign_bit_and;
      // case Syntax_Node_Kind::tk_pipeeq:
      //     return ast::Expr_Binary_Kind::assign_bit_or;
      // case Syntax_Node_Kind::tk_hateq:
      //     return ast::Expr_Binary_Kind::assign_bit_xor;
      // case Syntax_Node_Kind::tk_shleq:
      //     return ast::Expr_Binary_Kind::assign_shl;
      // case Syntax_Node_Kind::tk_shreq:
      //     return ast::Expr_Binary_Kind::assign_shr;

    default:
      ANTON_ASSERT(false, "invalid syntax node kind");
      ANTON_UNREACHABLE();
    }
  }

  [[nodiscard]] static anton::Expected<ast::Expr const*, Error>
  transform_expr(Context const& ctx, Syntax_Node const& node)
  {
    switch(node.kind) {
    case Syntax_Node_Kind::expr_if: {
      Syntax_Node const& condition_node = get_expr_if_condition(node);
      anton::Expected<ast::Expr const*, Error> condition = transform_expr(ctx, condition_node);
      if(!condition) {
        return {anton::expected_error, ANTON_MOV(condition.error())};
      }

      Syntax_Node const& then_expr_node = get_expr_block_expression(get_expr_if_then_branch(node));
      anton::Expected<ast::Expr const*, Error> then_branch = transform_expr(ctx, then_expr_node);
      if(!then_branch) {
        return {anton::expected_error, ANTON_MOV(then_branch.error())};
      }

      Syntax_Node const& else_expr_node = get_expr_block_expression(get_expr_if_else_branch(node));
      anton::Expected<ast::Expr const*, Error> else_branch = transform_expr(ctx, else_expr_node);
      if(!else_branch) {
        return {anton::expected_error, ANTON_MOV(else_branch.error())};
      }

      return {anton::expected_value,
              allocate<ast::Expr_If>(ctx.allocator, condition.value(), then_branch.value(),
                                     else_branch.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_identifier: {
      Syntax_Token const& value_token = get_expr_identifier_value(node);
      anton::String const* const value =
        allocate<anton::String>(ctx.allocator, value_token.value, ctx.allocator);
      return {anton::expected_value,
              allocate<ast::Expr_Identifier>(ctx.allocator, *value, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_binary: {
      Syntax_Node const& lhs_node = get_expr_binary_lhs(node);
      anton::Expected<ast::Expr const*, Error> lhs = transform_expr(ctx, lhs_node);
      if(!lhs) {
        return ANTON_MOV(lhs);
      }

      Syntax_Node const& rhs_node = get_expr_binary_rhs(node);
      anton::Expected<ast::Expr const*, Error> rhs = transform_expr(ctx, rhs_node);
      if(!rhs) {
        return ANTON_MOV(rhs);
      }

      Syntax_Token const& operator_token = get_expr_binary_operator(node);
      if(operator_token.kind == Syntax_Node_Kind::tk_equals) {
        return {anton::expected_value,
                allocate<ast::Expr_Assignment>(ctx.allocator, lhs.value(), rhs.value(),
                                               node.source_info)};
      } else {
        anton::String_View const identifier_string =
          get_operator_identifier_string(operator_token.kind);
        ast::Identifier const identifier{identifier_string, operator_token.source_info};
        Array<ast::Expr const*>& arguments =
          *allocate<Array<ast::Expr const*>>(ctx.allocator, ctx.allocator);
        arguments.push_back(lhs.value());
        arguments.push_back(rhs.value());
        return {anton::expected_value,
                allocate<ast::Expr_Call>(ctx.allocator, identifier, arguments, node.source_info)};
      }
    } break;

    case Syntax_Node_Kind::expr_prefix: {
      Syntax_Node const& expression_node = get_expr_prefix_expression(node);
      anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
      if(!expression) {
        return ANTON_MOV(expression);
      }

      Syntax_Token const& operator_token = get_expr_prefix_operator(node);
      anton::String_View const identifier_string =
        get_operator_identifier_string(operator_token.kind);
      ast::Identifier const identifier{identifier_string, operator_token.source_info};
      Array<ast::Expr const*>& arguments =
        *allocate<Array<ast::Expr const*>>(ctx.allocator, ctx.allocator);
      arguments.push_back(expression.value());
      return {anton::expected_value,
              allocate<ast::Expr_Call>(ctx.allocator, identifier, arguments, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_field: {
      Syntax_Node const& expression_node = get_expr_field_expression(node);
      anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
      if(!expression) {
        return ANTON_MOV(expression);
      }

      Syntax_Token const& identifier_token = get_expr_field_identifier(node);
      ast::Identifier const identifier = transform_identifier(ctx, identifier_token);
      return {anton::expected_value, allocate<ast::Expr_Field>(ctx.allocator, expression.value(),
                                                               identifier, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_index: {
      Syntax_Node const& expression_node = get_expr_index_expression(node);
      anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
      if(!expression) {
        return ANTON_MOV(expression);
      }

      Syntax_Node const& index_node = get_expr_index_index(node);
      anton::Expected<ast::Expr const*, Error> index = transform_expr(ctx, index_node);
      if(!index) {
        return ANTON_MOV(index);
      }

      return {anton::expected_value, allocate<ast::Expr_Index>(ctx.allocator, expression.value(),
                                                               index.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_parentheses: {
      Syntax_Node const& expression_node = get_expr_parentheses_expression(node);
      anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, expression_node);
      if(!expression) {
        return ANTON_MOV(expression);
      }

      return {anton::expected_value,
              allocate<ast::Expr_Parentheses>(ctx.allocator, expression.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_reinterpret: {
      // TODO: implement.
      ANTON_ASSERT(false, "unimplemented");
      ANTON_UNREACHABLE();
    } break;

    case Syntax_Node_Kind::expr_init: {
      auto transform_initializer =
        [](Context const& ctx,
           Syntax_Node const& node) -> anton::Expected<ast::Initializer const*, Error> {
        switch(node.kind) {
        case Syntax_Node_Kind::named_initializer: {
          Syntax_Token const& identifier_token = get_named_initializer_identifier(node);
          ast::Identifier const identifier = transform_identifier(ctx, identifier_token);

          Syntax_Node const& expression_node = get_named_initializer_expression(node);
          anton::Expected<ast::Expr const*, Error> expression =
            transform_expr(ctx, expression_node);
          if(!expression) {
            return {anton::expected_error, ANTON_MOV(expression.error())};
          }

          return {anton::expected_value,
                  allocate<ast::Named_Initializer>(ctx.allocator, identifier, expression.value(),
                                                   node.source_info)};
        } break;

        case Syntax_Node_Kind::indexed_initializer: {
          Syntax_Node const& index_node = get_indexed_initializer_index(node);
          anton::Expected<ast::Lt_Integer const*, Error> index = lower_lt_integer(ctx, index_node);
          if(!index) {
            return {anton::expected_error, ANTON_MOV(index.error())};
          }

          Syntax_Node const& expression_node = get_indexed_initializer_expression(node);
          anton::Expected<ast::Expr const*, Error> expression =
            transform_expr(ctx, expression_node);
          if(!expression) {
            return {anton::expected_error, ANTON_MOV(expression.error())};
          }

          return {anton::expected_value,
                  allocate<ast::Indexed_Initializer>(ctx.allocator, index.value(),
                                                     expression.value(), node.source_info)};
        } break;

        case Syntax_Node_Kind::basic_initializer: {
          Syntax_Node const& expression_node = get_basic_initializer_expression(node);
          anton::Expected<ast::Expr const*, Error> expression =
            transform_expr(ctx, expression_node);
          if(!expression) {
            return {anton::expected_error, ANTON_MOV(expression.error())};
          }

          return {anton::expected_value, allocate<ast::Basic_Initializer>(
                                           ctx.allocator, expression.value(), node.source_info)};
        } break;

        default:
          ANTON_ASSERT(false, "invalid syntax node kind");
          ANTON_UNREACHABLE();
        }
      };

      Syntax_Node const& type_node = get_expr_init_type(node);
      anton::Expected<ast::Type const*, Error> type = transform_type(ctx, type_node);
      if(!type) {
        return {anton::expected_error, ANTON_MOV(type.error())};
      }

      Syntax_Node const& initializers_node = get_expr_init_initializers(node);
      Array<ast::Initializer const*>& initializers =
        *allocate<Array<ast::Initializer const*>>(ctx.allocator, ctx.allocator);
      for(SNOT const& snot: initializers_node.children) {
        if(snot.is_left()) {
          anton::Expected<ast::Initializer const*, Error> initializer =
            transform_initializer(ctx, snot.left());
          if(initializer) {
            initializers.push_back(initializer.value());
          } else {
            return {anton::expected_error, ANTON_MOV(initializer.error())};
          }
        }
      }

      return {anton::expected_value, allocate<ast::Expr_Init>(ctx.allocator, type.value(),
                                                              initializers, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_call: {
      Syntax_Token const& identifier_token = get_expr_call_identifier(node);
      ast::Identifier const identifier = transform_identifier(ctx, identifier_token);

      Syntax_Node const& arguments_node = get_expr_call_arguments(node);
      Array<ast::Expr const*>& arguments =
        *allocate<Array<ast::Expr const*>>(ctx.allocator, ctx.allocator);
      for(SNOT const& snot: arguments_node.children) {
        if(snot.is_left()) {
          anton::Expected<ast::Expr const*, Error> expression = transform_expr(ctx, snot.left());
          if(expression) {
            arguments.push_back(expression.value());
          } else {
            return {anton::expected_error, ANTON_MOV(expression.error())};
          }
        }
      }

      return {anton::expected_value,
              allocate<ast::Expr_Call>(ctx.allocator, identifier, arguments, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_lt_bool: {
      Syntax_Token const& value_token = get_expr_lt_bool_value(node);
      bool const value = value_token.value == "true"_sv;
      return {anton::expected_value,
              allocate<ast::Lt_Bool>(ctx.allocator, value, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_lt_string: {
      // There are no string literals in the language besides the ones used by decl_import which
      // we have special handling for in place, therefore we leave this unimplemented.
      ANTON_ASSERT(false, "unimplemented");
      ANTON_UNREACHABLE();
    } break;

    case Syntax_Node_Kind::expr_lt_float: {
      Syntax_Token const& value_token = get_expr_lt_float_value(node);
      // The default float literal type is f32.
      ast::Lt_Float_Kind kind = ast::Lt_Float_Kind::f32;
      if(anton::Optional<Syntax_Token const&> suffix_token = get_expr_lt_float_suffix(node)) {
        anton::String_View const suffix = suffix_token->value;
        if(suffix == "d"_sv || suffix == "D"_sv) {
          kind = ast::Lt_Float_Kind::f64;
        } else {
          return {anton::expected_error, err_invalid_float_suffix(ctx, suffix_token->source_info)};
        }
      }

      anton::String const* const value =
        allocate<anton::String>(ctx.allocator, value_token.value, ctx.allocator);
      return {anton::expected_value,
              allocate<ast::Lt_Float>(ctx.allocator, *value, kind, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_lt_integer: {
      anton::Expected<ast::Lt_Integer const*, Error> result = lower_lt_integer(ctx, node);
      if(result) {
        return {anton::expected_value, result.value()};
      } else {
        return {anton::expected_error, result.error()};
      }
    } break;

    case Syntax_Node_Kind::expr_default: {
      return {anton::expected_value, allocate<ast::Expr_Default>(ctx.allocator, node.source_info)};
    } break;

    default:
      ANTON_UNREACHABLE();
    }
  }

  // transform_stmt
  //
  // Returns:
  // nullptr if the statement does not have a representation in the AST.
  //
  [[nodiscard]] static anton::Expected<ast::Node const*, Error>
  transform_stmt(Context const& ctx, Syntax_Node const& node);
  [[nodiscard]] static anton::Expected<ast::Node_List, Error>
  transform_stmt_block_child_stmts(Context const& ctx, Syntax_Node const& node);

  anton::Expected<ast::Node_List, Error> transform_stmt_block_child_stmts(Context const& ctx,
                                                                          Syntax_Node const& node)
  {
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::stmt_block, "Syntax_Node is not stmt_block");
    Array<ast::Node const*>& statements =
      *allocate<Array<ast::Node const*>>(ctx.allocator, ctx.allocator);
    for(SNOT const& snot: node.children) {
      if(!snot.is_left()) {
        continue;
      }

      Syntax_Node const& stmt_node = snot.left();
      anton::Expected<ast::Node const*, Error> stmt = transform_stmt(ctx, stmt_node);
      if(stmt) {
        if(stmt.value() != nullptr) {
          statements.push_back(stmt.value());
        }
      } else {
        return {anton::expected_error, ANTON_MOV(stmt.error())};
      }
    }
    return {anton::expected_value, statements};
  }

  [[nodiscard]] static anton::Expected<ast::Variable const*, Error>
  transform_variable(Context const& ctx, Syntax_Node const& node)
  {
    ast::Identifier const identifier = transform_identifier(ctx, get_variable_identifier(node));
    anton::Expected<ast::Type const*, Error> type = transform_type(ctx, get_variable_type(node));
    if(!type) {
      return {anton::expected_error, ANTON_MOV(type.error())};
    }

    ast::Expr const* initializer = nullptr;
    if(anton::Optional initializer_node = get_variable_initializer(node)) {
      anton::Expected<ast::Expr const*, Error> result =
        transform_expr(ctx, initializer_node.value());
      if(result) {
        initializer = result.value();
      } else {
        return {anton::expected_error, ANTON_MOV(result.error())};
      }
    }
    return {anton::expected_value, allocate<ast::Variable>(ctx.allocator, type.value(), identifier,
                                                           initializer, node.source_info)};
  }

  anton::Expected<ast::Node const*, Error> transform_stmt(Context const& ctx,
                                                          Syntax_Node const& node)
  {
    switch(node.kind) {
    case Syntax_Node_Kind::variable: {
      anton::Expected<ast::Variable const*, Error> variable = transform_variable(ctx, node);
      if(variable) {
        return {anton::expected_value, variable.value()};
      } else {
        return {anton::expected_error, ANTON_MOV(variable.error())};
      }
    } break;

    case Syntax_Node_Kind::stmt_block: {
      anton::Expected<ast::Node_List, Error> result = transform_stmt_block_child_stmts(ctx, node);
      if(!result) {
        return {anton::expected_error, ANTON_MOV(result.error())};
      }

      return {anton::expected_value,
              allocate<ast::Stmt_Block>(ctx.allocator, result.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_if: {
      anton::Expected<ast::Expr const*, Error> condition =
        transform_expr(ctx, get_stmt_if_condition(node));
      if(!condition) {
        return {anton::expected_error, ANTON_MOV(condition.error())};
      }

      anton::Expected<ast::Node_List, Error> then_branch =
        transform_stmt_block_child_stmts(ctx, get_stmt_if_then_branch(node));
      if(!then_branch) {
        return {anton::expected_error, ANTON_MOV(then_branch.error())};
      }

      ast::Node_List else_branch;
      if(anton::Optional<Syntax_Node const&> else_node = get_stmt_if_else_branch(node)) {
        anton::Expected<ast::Node_List, Error> result =
          transform_stmt_block_child_stmts(ctx, else_node.value());
        if(result) {
          else_branch = result.value();
        } else {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }
      }

      return {anton::expected_value,
              allocate<ast::Stmt_If>(ctx.allocator, condition.value(), then_branch.value(),
                                     else_branch, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_switch: {
      anton::Expected<ast::Expr const*, Error> expression =
        transform_expr(ctx, get_stmt_switch_expression(node));
      if(!expression) {
        return {anton::expected_error, ANTON_MOV(expression.error())};
      }

      Array<ast::Switch_Arm const*>* const arm_list =
        allocate<Array<ast::Switch_Arm const*>>(ctx.allocator, ctx.allocator);
      {
        Syntax_Node const& arm_list_node = get_stmt_switch_arm_list(node);
        for(SNOT const& snot: arm_list_node.children) {
          if(!snot.is_left()) {
            continue;
          }

          Syntax_Node const& arm_node = snot.left();
          Array<ast::Expr const*>* const labels =
            allocate<Array<ast::Expr const*>>(ctx.allocator, ctx.allocator);
          for(SNOT const& snot: arm_node.children) {
            if(snot.is_left() && snot.left().kind == Syntax_Node_Kind::switch_arm_label) {
              Syntax_Node const& expression_node = snot.left().children[0].left();
              anton::Expected<ast::Expr const*, Error> expression =
                transform_expr(ctx, expression_node);
              if(expression) {
                labels->push_back(expression.value());
              } else {
                return {anton::expected_error, ANTON_MOV(expression.error())};
              }
            }
          }

          anton::Expected<ast::Node_List, Error> statements =
            transform_stmt_block_child_stmts(ctx, get_switch_arm_body(arm_node));
          if(!statements) {
            return {anton::expected_error, ANTON_MOV(statements.error())};
          }

          ast::Switch_Arm const* const arm =
            allocate<ast::Switch_Arm>(ctx.allocator, *labels, statements.value(), node.source_info);
          arm_list->push_back(arm);
        }
      }

      return {anton::expected_value, allocate<ast::Stmt_Switch>(ctx.allocator, expression.value(),
                                                                *arm_list, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_for: {
      // We transform the for loop into a block statement that contains the definition of the
      // variable and the loop itself in order to limit scope.
      Array<ast::Node const*>* const statements =
        allocate<Array<ast::Node const*>>(ctx.allocator, ctx.allocator);
      if(anton::Optional variable_node = get_stmt_for_variable(node)) {
        anton::Expected<ast::Variable const*, Error> variable =
          transform_variable(ctx, variable_node.value());
        if(variable) {
          statements->push_back(variable.value());
        } else {
          return {anton::expected_error, ANTON_MOV(variable.error())};
        }
      }

      {
        ast::Expr const* condition = nullptr;
        if(anton::Optional condition_node = get_stmt_for_condition(node)) {
          anton::Expected<ast::Expr const*, Error> result =
            transform_expr(ctx, condition_node.value());
          if(result) {
            condition = result.value();
          } else {
            return {anton::expected_error, ANTON_MOV(result.error())};
          }
        }

        ast::Node_List continuation;
        if(anton::Optional expression_node = get_stmt_for_expression(node)) {
          anton::Expected<ast::Expr const*, Error> expression =
            transform_expr(ctx, expression_node.value());
          if(!expression) {
            return {anton::expected_error, ANTON_MOV(expression.error())};
          }

          Array<ast::Node const*>* const continuation_statements =
            allocate<Array<ast::Node const*>>(ctx.allocator, ctx.allocator);
          ast::Stmt_Expression const* const stmt = allocate<ast::Stmt_Expression>(
            ctx.allocator, expression.value(), expression.value()->source_info);
          continuation_statements->push_back(stmt);
          continuation = *continuation_statements;
        }

        anton::Expected<ast::Node_List, Error> body =
          transform_stmt_block_child_stmts(ctx, get_stmt_for_body(node));
        if(!body) {
          return {anton::expected_error, ANTON_MOV(body.error())};
        }

        ast::Stmt_Loop const* const loop = allocate<ast::Stmt_Loop>(
          ctx.allocator, condition, continuation, body.value(), node.source_info);
        statements->push_back(loop);
      }

      return {anton::expected_value,
              allocate<ast::Stmt_Block>(ctx.allocator, *statements, Source_Info{})};
    } break;

    case Syntax_Node_Kind::stmt_while: {
      anton::Expected<ast::Expr const*, Error> condition =
        transform_expr(ctx, get_stmt_while_condition(node));
      if(!condition) {
        return {anton::expected_error, ANTON_MOV(condition.error())};
      }

      anton::Expected<ast::Node_List, Error> statements =
        transform_stmt_block_child_stmts(ctx, get_stmt_while_statements(node));
      if(!statements) {
        return {anton::expected_error, ANTON_MOV(statements.error())};
      }

      return {anton::expected_value,
              allocate<ast::Stmt_Loop>(ctx.allocator, condition.value(), ast::Node_List{},
                                       statements.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_do_while: {
      anton::Expected<ast::Node_List, Error> statements =
        transform_stmt_block_child_stmts(ctx, get_stmt_do_while_body(node));
      if(!statements) {
        return {anton::expected_error, ANTON_MOV(statements.error())};
      }

      // We want to place the condition in the continuation block,
      // therefore we transform it into 'if condition { } else { break }'.
      Array<ast::Node const*>* const continuation =
        allocate<Array<ast::Node const*>>(ctx.allocator, ctx.allocator);
      {
        anton::Expected<ast::Expr const*, Error> condition =
          transform_expr(ctx, get_stmt_do_while_condition(node));
        if(!condition) {
          return {anton::expected_error, ANTON_MOV(condition.error())};
        }

        Array<ast::Node const*>* const else_block =
          allocate<Array<ast::Node const*>>(ctx.allocator, ctx.allocator);
        else_block->push_back(allocate<ast::Stmt_Break>(ctx.allocator, Source_Info{}));
        ast::Stmt_If const* const stmt_if = allocate<ast::Stmt_If>(
          ctx.allocator, condition.value(), ast::Node_List{}, *else_block, Source_Info{});
        continuation->push_back(stmt_if);
      }

      return {anton::expected_value,
              allocate<ast::Stmt_Loop>(ctx.allocator, nullptr, *continuation, statements.value(),
                                       node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_return: {
      ast::Expr const* expression = nullptr;
      if(anton::Optional expression_node = get_stmt_return_expression(node)) {
        anton::Expected<ast::Expr const*, Error> result =
          transform_expr(ctx, expression_node.value());
        if(result) {
          expression = result.value();
        } else {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }
      }
      return {anton::expected_value,
              allocate<ast::Stmt_Return>(ctx.allocator, expression, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_break: {
      return {anton::expected_value, allocate<ast::Stmt_Break>(ctx.allocator, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_continue: {
      return {anton::expected_value, allocate<ast::Stmt_Continue>(ctx.allocator, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_discard: {
      return {anton::expected_value, allocate<ast::Stmt_Discard>(ctx.allocator, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_expression: {
      anton::Expected<ast::Expr const*, Error> expression =
        transform_expr(ctx, get_stmt_expression_expression(node));
      if(!expression) {
        return {anton::expected_error, ANTON_MOV(expression.error())};
      }

      return {anton::expected_value,
              allocate<ast::Stmt_Expression>(ctx.allocator, expression.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_empty: {
      return {anton::expected_value, nullptr};
    } break;

    default:
      ANTON_UNREACHABLE();
    }
  }

  [[nodiscard]] static anton::Expected<ast::Attr_List, Error>
  transform_attribute_list(Context const& ctx, Syntax_Node const& node)
  {
    if(node.children.size() == 0) {
      return {anton::expected_value, ast::Attr_List{}};
    }

    Array<ast::Attribute const*>* const attributes =
      allocate<Array<ast::Attribute const*>>(ctx.allocator, ctx.allocator);
    for(SNOT const& attribute_snot: node.children) {
      Syntax_Node const& attribute_node = attribute_snot.left();
      anton::Slice<ast::Attribute_Parameter> parameters;
      if(anton::Optional const parameter_list_node = get_attribute_parameter_list(attribute_node)) {
        Array<ast::Attribute_Parameter>* const parameters_array =
          allocate<Array<ast::Attribute_Parameter>>(ctx.allocator, ctx.allocator);
        for(SNOT const& snot: parameter_list_node->children) {
          if(!snot.is_left()) {
            continue;
          }

          Syntax_Node const& parameter = snot.left();
          ANTON_ASSERT(
            parameter.kind == Syntax_Node_Kind::attribute_parameter_positional ||
              parameter.kind == Syntax_Node_Kind::attribute_parameter_keyed,
            "Syntax_Node is not an attribute_parameter_positional or attribute_parameter_keyed");
          ast::Identifier key;
          ast::Expr const* value = nullptr;
          if(parameter.kind == Syntax_Node_Kind::attribute_parameter_keyed) {
            Syntax_Token const& key_node = get_attribute_parameter_keyed_key(parameter);
            key = transform_identifier(ctx, key_node);
            Syntax_Node const& value_node = get_attribute_parameter_keyed_value(parameter);
            anton::Expected<ast::Expr const*, Error> value_result = transform_expr(ctx, value_node);
            if(!value_result) {
              return {anton::expected_error, ANTON_MOV(value_result.error())};
            }
            value = value_result.value();
          } else {
            Syntax_Node const& value_node = get_attribute_parameter_positional_value(parameter);
            anton::Expected<ast::Expr const*, Error> value_result = transform_expr(ctx, value_node);
            if(!value_result) {
              return {anton::expected_error, ANTON_MOV(value_result.error())};
            }
            value = value_result.value();
          }
          parameters_array->push_back(ast::Attribute_Parameter{key, value});
        }
        parameters = *parameters_array;
      }

      ast::Identifier const identifier =
        transform_identifier(ctx, get_attribute_identifier(attribute_node));
      attributes->push_back(allocate<ast::Attribute>(ctx.allocator, identifier, parameters,
                                                     attribute_node.source_info));
    }
    return {anton::expected_value, *attributes};
  }

  [[nodiscard]] static anton::Expected<bool, Error>
  evaluate_constant_expression(Context const& ctx, Syntax_Node const& node)
  {
    // TODO: Implement.
    return {anton::expected_value, true};
  }

  [[nodiscard]] static anton::Expected<ast::Node_List, Error>
  transform_decl_if(Context& ctx, Syntax_Node const& node)
  {
    anton::Expected<bool, Error> condition =
      evaluate_constant_expression(ctx, get_decl_if_condition(node));
    if(!condition) {
      return {anton::expected_error, ANTON_MOV(condition.error())};
    }

    if(condition.value()) {
      Syntax_Node const& then_node = get_decl_if_then_branch(node);
      return lower_syntax_to_ast(ctx, then_node.children);
    } else {
      anton::Optional<Syntax_Node const&> else_node = get_decl_if_else_branch(node);
      if(else_node) {
        return lower_syntax_to_ast(ctx, else_node.value().children);
      } else {
        return {anton::expected_value, ast::Node_List{}};
      }
    }
  }

  [[nodiscard]] static anton::Expected<ast::Node_List, Error>
  transform_decl_import(Context& ctx, Syntax_Node const& node)
  {
    Syntax_Node const& path_node = get_decl_import_path(node);
    Syntax_Token const& path_token = get_expr_lt_string_value(path_node);
    // Trim the double quotation marks.
    anton::String_View const source_name{path_token.value.bytes_begin() + 1,
                                         path_token.value.bytes_end() - 1};
    return import_source_code(ctx, source_name, node.source_info);
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Struct const*, Error>
  transform_decl_struct(Context const& ctx, Syntax_Node const& node)
  {
    Array<ast::Struct_Member const*>& members =
      *allocate<Array<ast::Struct_Member const*>>(ctx.allocator, ctx.allocator);
    Syntax_Node const& members_node = get_decl_struct_members(node);
    for(SNOT const& member_snot: members_node.children) {
      if(!member_snot.is_left()) {
        continue;
      }

      Syntax_Node const& member_node = member_snot.left();
      anton::Expected<ast::Attr_List, Error> attribute_list =
        transform_attribute_list(ctx, get_struct_member_attribute_list(member_node));
      if(!attribute_list) {
        return {anton::expected_error, ANTON_MOV(attribute_list.error())};
      }

      ast::Identifier const identifier =
        transform_identifier(ctx, get_struct_member_identifier(member_node));
      anton::Expected<ast::Type const*, Error> type =
        transform_type(ctx, get_struct_member_type(member_node));
      if(!type) {
        return {anton::expected_error, ANTON_MOV(type.error())};
      }
      ast::Expr const* initializer = nullptr;
      anton::Optional initializer_node = get_struct_member_initializer(member_node);
      if(initializer_node) {
        anton::Expected<ast::Expr const*, Error> result =
          transform_expr(ctx, initializer_node.value());
        if(result) {
          initializer = result.value();
        } else {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }
      }

      members.push_back(allocate<ast::Struct_Member>(ctx.allocator, attribute_list.value(),
                                                     identifier, type.value(), initializer,
                                                     node.source_info));
    }

    ast::Identifier const identifier = transform_identifier(ctx, get_decl_struct_identifier(node));
    return {anton::expected_value,
            allocate<ast::Decl_Struct>(ctx.allocator, identifier, members, node.source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Fn_Parameter const*, Error>
  transform_parameter(Context const& ctx, Syntax_Node const& node)
  {
    if(node.kind == Syntax_Node_Kind::fn_parameter) {
      ast::Identifier const identifier =
        transform_identifier(ctx, get_fn_parameter_identifier(node));
      anton::Expected<ast::Type const*, Error> type =
        transform_type(ctx, get_fn_parameter_type(node));
      if(!type) {
        return {anton::expected_error, ANTON_MOV(type.error())};
      }

      ast::Identifier source;
      if(anton::Optional result = get_fn_parameter_source(node)) {
        source = transform_identifier(ctx, result.value());
      }

      return {anton::expected_value,
              allocate<ast::Fn_Parameter>(ctx.allocator, identifier, type.value(), source,
                                          node.source_info)};
    } else if(node.kind == Syntax_Node_Kind::fn_parameter_if) {
      anton::Expected<bool, Error> condition =
        evaluate_constant_expression(ctx, get_fn_parameter_if_condition(node));
      if(!condition) {
        return {anton::expected_error, ANTON_MOV(condition.error())};
      }

      if(condition.value()) {
        Syntax_Node const& then_node = get_fn_parameter_if_then_branch(node);
        return transform_parameter(ctx, then_node);
      } else {
        Syntax_Node const& else_node = get_fn_parameter_if_else_branch(node);
        return transform_parameter(ctx, else_node);
      }
    } else {
      // TODO: Error.
      ANTON_ASSERT(false, "unreachable");
      ANTON_UNREACHABLE();
    }
  }

  [[nodiscard]] static anton::Expected<ast::Fn_Parameter_List, Error>
  transform_parameter_list(Context const& ctx, Syntax_Node const& node)
  {
    Array<ast::Fn_Parameter const*>& parameters =
      *allocate<Array<ast::Fn_Parameter const*>>(ctx.allocator, ctx.allocator);
    for(SNOT const& snot: node.children) {
      if(!snot.is_left()) {
        continue;
      }

      Syntax_Node const& parameter_node = snot.left();
      anton::Expected<ast::Fn_Parameter const*, Error> parameter =
        transform_parameter(ctx, parameter_node);
      if(parameter) {
        parameters.push_back(parameter.value());
      } else {
        return {anton::expected_error, ANTON_MOV(parameter.error())};
      }
    }
    return {anton::expected_value, parameters};
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Function const*, Error>
  transform_decl_function(Context const& ctx, Syntax_Node const& node)
  {
    anton::Expected<ast::Attr_List, Error> attribute_list =
      transform_attribute_list(ctx, get_decl_function_attribute_list(node));
    if(!attribute_list) {
      return {anton::expected_error, ANTON_MOV(attribute_list.error())};
    }

    ast::Identifier const identifier =
      transform_identifier(ctx, get_decl_function_identifier(node));
    anton::Expected<ast::Fn_Parameter_List, Error> parameters =
      transform_parameter_list(ctx, get_decl_function_parameter_list(node));
    if(!parameters) {
      return {anton::expected_error, ANTON_MOV(parameters.error())};
    }

    anton::Expected<ast::Type const*, Error> return_type =
      transform_type(ctx, get_decl_function_return_type(node));
    if(!return_type) {
      return {anton::expected_error, ANTON_MOV(return_type.error())};
    }

    anton::Expected<ast::Node_List, Error> body =
      transform_stmt_block_child_stmts(ctx, get_decl_function_body(node));
    if(!body) {
      return {anton::expected_error, ANTON_MOV(body.error())};
    }

    return {anton::expected_value,
            allocate<ast::Decl_Function>(ctx.allocator, attribute_list.value(), identifier,
                                         parameters.value(), return_type.value(), body.value(),
                                         false, node.source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Stage_Function const*, Error>
  transform_decl_stage_function(Context const& ctx, Syntax_Node const& node)
  {
    auto transform_stage_kind = [](Syntax_Token const& token) -> ast::With_Source<Stage_Kind> {
      if(token.value == "vertex"_sv) {
        return {Stage_Kind::vertex, token.source_info};
      } else if(token.value == "fragment"_sv) {
        return {Stage_Kind::fragment, token.source_info};
      } else if(token.value == "compute"_sv) {
        return {Stage_Kind::compute, token.source_info};
      } else {
        // TODO: Error
        ANTON_ASSERT(false, "unreachable");
        ANTON_UNREACHABLE();
      }
    };

    anton::Expected<ast::Attr_List, Error> attribute_list =
      transform_attribute_list(ctx, get_decl_stage_function_attribute_list(node));
    if(!attribute_list) {
      return {anton::expected_error, ANTON_MOV(attribute_list.error())};
    }

    ast::Identifier const pass = transform_identifier(ctx, get_decl_stage_function_pass(node));
    ast::With_Source<Stage_Kind> const stage =
      transform_stage_kind(get_decl_stage_function_stage(node));
    anton::Expected<ast::Fn_Parameter_List, Error> parameters =
      transform_parameter_list(ctx, get_decl_stage_function_parameter_list(node));
    if(!parameters) {
      return {anton::expected_error, ANTON_MOV(parameters.error())};
    }

    anton::Expected<ast::Type const*, Error> return_type =
      transform_type(ctx, get_decl_stage_function_return_type(node));
    if(!return_type) {
      return {anton::expected_error, ANTON_MOV(return_type.error())};
    }

    anton::Expected<ast::Node_List, Error> body =
      transform_stmt_block_child_stmts(ctx, get_decl_stage_function_body(node));
    if(!body) {
      return {anton::expected_error, ANTON_MOV(body.error())};
    }

    return {anton::expected_value,
            allocate<ast::Decl_Stage_Function>(ctx.allocator, attribute_list.value(), pass, stage,
                                               parameters.value(), return_type.value(),
                                               body.value(), node.source_info)};
  }

  anton::Expected<ast::Node_List, Error> lower_syntax_to_ast(Context& ctx,
                                                             Array<SNOT> const& syntax)
  {
    Array<ast::Node const*>& abstract =
      *allocate<Array<ast::Node const*>>(ctx.allocator, ctx.allocator);
    for(SNOT const& snot: syntax) {
      if(!snot.is_left()) {
        continue;
      }

      Syntax_Node const& syntax_node = snot.left();
      switch(syntax_node.kind) {
      case Syntax_Node_Kind::decl_if: {
        anton::Expected<ast::Node_List, Error> result = transform_decl_if(ctx, syntax_node);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }

        ast::Node_List const decls = result.value();
        abstract.insert(abstract.end(), decls.begin(), decls.end());
      } break;

      case Syntax_Node_Kind::decl_import: {
        anton::Expected<ast::Node_List, Error> result = transform_decl_import(ctx, syntax_node);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }

        ast::Node_List const decls = result.value();
        abstract.insert(abstract.end(), decls.begin(), decls.end());
      } break;

      case Syntax_Node_Kind::variable: {
        anton::Expected<ast::Variable const*, Error> result = transform_variable(ctx, syntax_node);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }

        abstract.insert(abstract.end(), result.value());
      } break;

      case Syntax_Node_Kind::decl_struct: {
        anton::Expected<ast::Decl_Struct const*, Error> result =
          transform_decl_struct(ctx, syntax_node);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }

        ast::Decl_Struct const* const decl = result.value();
        abstract.insert(abstract.end(), decl);
      } break;

      case Syntax_Node_Kind::decl_settings: {
        // TODO
      } break;

      case Syntax_Node_Kind::decl_function: {
        anton::Expected<ast::Decl_Function const*, Error> result =
          transform_decl_function(ctx, syntax_node);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }

        ast::Decl_Function const* const decl = result.value();
        abstract.insert(abstract.end(), decl);
      } break;

      case Syntax_Node_Kind::decl_stage_function: {
        anton::Expected<ast::Decl_Stage_Function const*, Error> result =
          transform_decl_stage_function(ctx, syntax_node);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }

        ast::Decl_Stage_Function const* const decl = result.value();
        abstract.insert(abstract.end(), decl);
      } break;

      default:
        ANTON_UNREACHABLE();
      }
    }
    return {anton::expected_value, abstract};
  }
} // namespace vush
