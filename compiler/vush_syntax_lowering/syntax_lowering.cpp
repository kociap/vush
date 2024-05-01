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

#define RETURN_ON_FAIL(variable, fn, ...)                        \
  auto variable = fn(__VA_ARGS__);                               \
  if(!variable) {                                                \
    return {anton::expected_error, ANTON_MOV(variable.error())}; \
  }

  anton::Expected<ast::Node_List, Error>
  import_source_code(Context& ctx, anton::String_View const source_name,
                     anton::Optional<Source_Info> const source_info)
  {
    anton::Expected<Source_Import_Result, anton::String> source_request_res =
      ctx.source_import_cb(*ctx.allocator, source_name,
                           ctx.source_import_user_data);
    if(!source_request_res) {
      if(source_info) {
        return {anton::expected_error,
                err_source_import_failed(ctx, *source_info,
                                         source_request_res.error())};
      } else {
        return {anton::expected_error, err_source_import_failed_no_location(
                                         ctx, source_request_res.error())};
      }
    }

    Source_Import_Result& request_res = source_request_res.value();
    // Ensure we're not importing the same source multiple times.
    if(ctx.source_registry->find_source(request_res.source_name) == nullptr) {
      Source_Data source{ANTON_MOV(request_res.source_name),
                         ANTON_MOV(request_res.data)};
      anton::String_View const source_name = source.name;
      anton::String_View const source_data = source.data;
      ctx.source_registry->add_source(ANTON_MOV(source));

      Parse_Syntax_Options options{.include_whitespace_and_comments = false};
      RETURN_ON_FAIL(parse_result, parse_source_to_syntax_tree, ctx,
                     source_name, source_data, options);
      RETURN_ON_FAIL(transform_result, lower_syntax_to_ast, ctx,
                     parse_result.value());
      return {anton::expected_value, ANTON_MOV(transform_result.value())};
    } else {
      return {anton::expected_value, ast::Node_List{}};
    }
  }

  [[nodiscard]] static ast::Identifier
  transform_identifier(Context const& ctx, Syntax_Token const& token)
  {
    anton::String const* const value =
      VUSH_ALLOCATE(anton::String, ctx.allocator, token.value, ctx.allocator);
    return ast::Identifier{*value, token.source_info};
  }

  [[nodiscard]] static anton::Expected<ast::Lt_Integer*, Error>
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
      // TODO: Error
      ANTON_UNREACHABLE("unreachable");
    }

    // The default integer literal type is i32.
    ast::Lt_Integer_Kind kind = ast::Lt_Integer_Kind::i32;
    if(anton::Optional<Syntax_Token const&> suffix_token =
         get_expr_lt_integer_suffix(node)) {
      anton::String_View const suffix = suffix_token->value;
      if(suffix == "u"_sv || suffix == "U"_sv) {
        kind = ast::Lt_Integer_Kind::u32;
      } else {
        return {anton::expected_error,
                err_invalid_integer_suffix(ctx, suffix_token->source_info)};
      }
    }

    switch(kind) {
    case ast::Lt_Integer_Kind::i32: {
      i32 const value = anton::str_to_i64(value_token.value, base);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Lt_Integer, ctx.allocator, ast::lt_integer_i32,
                            value, node.source_info)};
    }

    case ast::Lt_Integer_Kind::u32: {
      u32 const value = anton::str_to_u64(value_token.value, base);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Lt_Integer, ctx.allocator, ast::lt_integer_u32,
                            value, node.source_info)};
    }
    }
  }

  [[nodiscard]] static anton::Expected<ast::Type*, Error>
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
        ast::Type_Builtin_Kind const kind = enumified.value();
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Builtin, ctx.allocator,
                              node.source_info, qualifiers, kind)};
      } else {
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Type_Struct, ctx.allocator, node.source_info,
                              qualifiers,
                              anton::String(value_token.value, ctx.allocator))};
      }
    } break;

    case Syntax_Node_Kind::type_array: {
      ast::Qualifiers qualifiers;
      if(anton::Optional mut = get_type_array_mut(node)) {
        qualifiers.mut = true;
      }

      Syntax_Node const& base_node = get_type_array_base(node);
      RETURN_ON_FAIL(base, transform_type, ctx, base_node);
      ast::Lt_Integer* size = nullptr;
      if(anton::Optional size_node = get_type_array_size(node)) {
        RETURN_ON_FAIL(size_result, lower_lt_integer, ctx, size_node.value());
        size = size_result.value();
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Type_Array, ctx.allocator, node.source_info,
                            qualifiers, base.value(), size)};
    } break;

    default:
      ANTON_UNREACHABLE("invalid type");
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
    default:
      ANTON_UNREACHABLE("invalid syntax node kind");
    }
  }

  [[nodiscard]] static anton::Expected<ast::Expr*, Error>
  transform_expr(Context const& ctx, Syntax_Node const& node)
  {
    switch(node.kind) {
    case Syntax_Node_Kind::expr_if: {
      Syntax_Node const& condition_node = get_expr_if_condition(node);
      RETURN_ON_FAIL(condition, transform_expr, ctx, condition_node);
      Syntax_Node const& then_expr_node =
        get_expr_block_expression(get_expr_if_then_branch(node));
      RETURN_ON_FAIL(then_branch, transform_expr, ctx, then_expr_node);
      Syntax_Node const& else_expr_node =
        get_expr_block_expression(get_expr_if_else_branch(node));
      anton::Expected<ast::Expr*, Error> else_branch =
        transform_expr(ctx, else_expr_node);
      if(!else_branch) {
        return {anton::expected_error, ANTON_MOV(else_branch.error())};
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_If, ctx.allocator, condition.value(),
                            then_branch.value(), else_branch.value(),
                            node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_identifier: {
      Syntax_Token const& value_token = get_expr_identifier_value(node);
      anton::String const* const value = VUSH_ALLOCATE(
        anton::String, ctx.allocator, value_token.value, ctx.allocator);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Identifier, ctx.allocator, *value,
                            node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_binary: {
      Syntax_Node const& lhs_node = get_expr_binary_lhs(node);
      RETURN_ON_FAIL(lhs, transform_expr, ctx, lhs_node);
      Syntax_Node const& rhs_node = get_expr_binary_rhs(node);
      RETURN_ON_FAIL(rhs, transform_expr, ctx, rhs_node);

      Syntax_Token const& operator_token = get_expr_binary_operator(node);
      anton::String_View const identifier_string =
        get_operator_identifier_string(operator_token.kind);
      ast::Identifier const identifier{identifier_string,
                                       operator_token.source_info};
      auto& arguments =
        *VUSH_ALLOCATE(Array<ast::Expr*>, ctx.allocator, ctx.allocator);
      arguments.push_back(lhs.value());
      arguments.push_back(rhs.value());
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Call, ctx.allocator, identifier,
                            arguments, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_prefix: {
      Syntax_Node const& expression_node = get_expr_prefix_expression(node);
      RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
      Syntax_Token const& operator_token = get_expr_prefix_operator(node);
      anton::String_View const identifier_string =
        get_operator_identifier_string(operator_token.kind);
      ast::Identifier const identifier{identifier_string,
                                       operator_token.source_info};
      auto& arguments =
        *VUSH_ALLOCATE(Array<ast::Expr*>, ctx.allocator, ctx.allocator);
      arguments.push_back(expression.value());
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Call, ctx.allocator, identifier,
                            arguments, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_field: {
      Syntax_Node const& expression_node = get_expr_field_expression(node);
      RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
      Syntax_Token const& identifier_token = get_expr_field_identifier(node);
      ast::Identifier const identifier =
        transform_identifier(ctx, identifier_token);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Field, ctx.allocator, expression.value(),
                            identifier, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_index: {
      Syntax_Node const& expression_node = get_expr_index_expression(node);
      RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
      Syntax_Node const& index_node = get_expr_index_index(node);
      RETURN_ON_FAIL(index, transform_expr, ctx, index_node);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Index, ctx.allocator, expression.value(),
                            index.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_parentheses: {
      Syntax_Node const& expression_node =
        get_expr_parentheses_expression(node);
      return transform_expr(ctx, expression_node);
    } break;

    case Syntax_Node_Kind::expr_reinterpret: {
      // TODO: implement.
      ANTON_UNREACHABLE("unimplemented");
    } break;

    case Syntax_Node_Kind::expr_init: {
      auto transform_initializer = [](Context const& ctx,
                                      Syntax_Node const& node)
        -> anton::Expected<ast::Initializer*, Error> {
        switch(node.kind) {
        case Syntax_Node_Kind::field_initializer: {
          Syntax_Token const& identifier_token =
            get_field_initializer_identifier(node);
          ast::Identifier const identifier =
            transform_identifier(ctx, identifier_token);

          Syntax_Node const& expression_node =
            get_field_initializer_expression(node);
          RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
          return {anton::expected_value,
                  VUSH_ALLOCATE(ast::Field_Initializer, ctx.allocator,
                                identifier, expression.value(),
                                node.source_info)};
        } break;

        case Syntax_Node_Kind::index_initializer: {
          Syntax_Node const& index_node = get_index_initializer_index(node);
          RETURN_ON_FAIL(index, lower_lt_integer, ctx, index_node);
          Syntax_Node const& expression_node =
            get_index_initializer_expression(node);
          RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
          return {anton::expected_value,
                  VUSH_ALLOCATE(ast::Index_Initializer, ctx.allocator,
                                index.value(), expression.value(),
                                node.source_info)};
        } break;

        case Syntax_Node_Kind::basic_initializer: {
          Syntax_Node const& expression_node =
            get_basic_initializer_expression(node);
          RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
          return {anton::expected_value,
                  VUSH_ALLOCATE(ast::Basic_Initializer, ctx.allocator,
                                expression.value(), node.source_info)};
        } break;

        default:
          ANTON_UNREACHABLE("invalid syntax node kind");
        }
      };

      Syntax_Node const& type_node = get_expr_init_type(node);
      anton::Expected<ast::Type*, Error> type = transform_type(ctx, type_node);
      if(!type) {
        return {anton::expected_error, ANTON_MOV(type.error())};
      }

      Syntax_Node const& initializers_node = get_expr_init_initializers(node);
      auto& initializers =
        *VUSH_ALLOCATE(Array<ast::Initializer*>, ctx.allocator, ctx.allocator);
      for(SNOT const& snot: initializers_node.children) {
        if(snot.is_left()) {
          anton::Expected<ast::Initializer*, Error> initializer =
            transform_initializer(ctx, snot.left());
          if(initializer) {
            initializers.push_back(initializer.value());
          } else {
            return {anton::expected_error, ANTON_MOV(initializer.error())};
          }
        }
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Init, ctx.allocator, type.value(),
                            initializers, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_call: {
      Syntax_Token const& identifier_token = get_expr_call_identifier(node);
      ast::Identifier const identifier =
        transform_identifier(ctx, identifier_token);

      Syntax_Node const& arguments_node = get_expr_call_arguments(node);
      auto& arguments =
        *VUSH_ALLOCATE(Array<ast::Expr*>, ctx.allocator, ctx.allocator);
      for(SNOT const& snot: arguments_node.children) {
        if(snot.is_left()) {
          anton::Expected<ast::Expr*, Error> expression =
            transform_expr(ctx, snot.left());
          if(expression) {
            arguments.push_back(expression.value());
          } else {
            return {anton::expected_error, ANTON_MOV(expression.error())};
          }
        }
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Expr_Call, ctx.allocator, identifier,
                            arguments, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_lt_bool: {
      Syntax_Token const& value_token = get_expr_lt_bool_value(node);
      bool const value = value_token.value == "true"_sv;
      return {anton::expected_value, VUSH_ALLOCATE(ast::Lt_Bool, ctx.allocator,
                                                   value, node.source_info)};
    } break;

    case Syntax_Node_Kind::expr_lt_string: {
      // There are no string literals in the language besides the ones used by decl_import which
      // we have special handling for in place, therefore we leave this unimplemented.
      ANTON_UNREACHABLE("unimplemented");
    } break;

    case Syntax_Node_Kind::expr_lt_float: {
      Syntax_Token const& value_token = get_expr_lt_float_value(node);
      // TODO: Validate float string token.
      // The default float literal type is f32.
      if(anton::Optional<Syntax_Token const&> suffix_token =
           get_expr_lt_float_suffix(node)) {
        anton::String_View const suffix = suffix_token->value;
        if(suffix != "d"_sv && suffix != "D"_sv) {
          return {anton::expected_error,
                  err_invalid_float_suffix(ctx, suffix_token->source_info)};
        }

        f64 const value = anton::str_to_f64(value_token.value);
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Lt_Float, ctx.allocator, ast::lt_float_f64,
                              value, node.source_info)};
      } else {
        f32 const value = anton::str_to_f32(value_token.value);
        return {anton::expected_value,
                VUSH_ALLOCATE(ast::Lt_Float, ctx.allocator, ast::lt_float_f32,
                              value, node.source_info)};
      }
    } break;

    case Syntax_Node_Kind::expr_lt_integer: {
      RETURN_ON_FAIL(result, lower_lt_integer, ctx, node);
      return {anton::expected_value, result.value()};
    } break;

    case Syntax_Node_Kind::expr_default: {
      return {
        anton::expected_value,
        VUSH_ALLOCATE(ast::Expr_Default, ctx.allocator, node.source_info)};
    } break;

    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  [[nodiscard]] static ast::Assignment_Kind
  map_token_to_assignment_kind(Syntax_Node_Kind const kind)
  {
    switch(kind) {
    case Syntax_Node_Kind::tk_equals:
      return ast::Assignment_Kind::e_assign;
    case Syntax_Node_Kind::tk_pluseq:
      return ast::Assignment_Kind::e_add;
    case Syntax_Node_Kind::tk_minuseq:
      return ast::Assignment_Kind::e_sub;
    case Syntax_Node_Kind::tk_asteriskeq:
      return ast::Assignment_Kind::e_mul;
    case Syntax_Node_Kind::tk_slasheq:
      return ast::Assignment_Kind::e_div;
    case Syntax_Node_Kind::tk_percenteq:
      return ast::Assignment_Kind::e_mod;
    case Syntax_Node_Kind::tk_ampeq:
      return ast::Assignment_Kind::e_and;
    case Syntax_Node_Kind::tk_pipeeq:
      return ast::Assignment_Kind::e_or;
    case Syntax_Node_Kind::tk_hateq:
      return ast::Assignment_Kind::e_xor;
    case Syntax_Node_Kind::tk_shleq:
      return ast::Assignment_Kind::e_shl;
    case Syntax_Node_Kind::tk_shreq:
      return ast::Assignment_Kind::e_shr;
    default:
      ANTON_UNREACHABLE("invalid token kind");
    }
  }

  [[nodiscard]] static anton::Expected<ast::Attr_List, Error>
  transform_attribute_list(Context const& ctx, Syntax_Node const& node)
  {
    if(node.children.size() == 0) {
      return {anton::expected_value, ast::Attr_List{}};
    }

    auto& attributes =
      *VUSH_ALLOCATE(Array<ast::Attribute*>, ctx.allocator, ctx.allocator);
    for(SNOT const& attribute_snot: node.children) {
      Syntax_Node const& attribute_node = attribute_snot.left();
      anton::Slice<ast::Attribute_Parameter> parameters;
      if(anton::Optional const parameter_list_node =
           get_attribute_parameter_list(attribute_node)) {
        auto& parameters_array = *VUSH_ALLOCATE(Array<ast::Attribute_Parameter>,
                                                ctx.allocator, ctx.allocator);
        for(SNOT const& snot: parameter_list_node->children) {
          if(!snot.is_left()) {
            continue;
          }

          Syntax_Node const& parameter = snot.left();
          ANTON_ASSERT(parameter.kind ==
                           Syntax_Node_Kind::attribute_parameter_positional ||
                         parameter.kind ==
                           Syntax_Node_Kind::attribute_parameter_keyed,
                       "Syntax_Node is not an attribute_parameter_positional "
                       "or attribute_parameter_keyed");
          ast::Identifier key;
          ast::Expr* value = nullptr;
          if(parameter.kind == Syntax_Node_Kind::attribute_parameter_keyed) {
            Syntax_Token const& key_node =
              get_attribute_parameter_keyed_key(parameter);
            key = transform_identifier(ctx, key_node);
            Syntax_Node const& value_node =
              get_attribute_parameter_keyed_value(parameter);
            RETURN_ON_FAIL(value_result, transform_expr, ctx, value_node);
            value = value_result.value();
          } else {
            Syntax_Node const& value_node =
              get_attribute_parameter_positional_value(parameter);
            RETURN_ON_FAIL(value_result, transform_expr, ctx, value_node);
            value = value_result.value();
          }
          parameters_array.push_back(ast::Attribute_Parameter{key, value});
        }
        parameters = parameters_array;
      }

      ast::Identifier const identifier =
        transform_identifier(ctx, get_attribute_identifier(attribute_node));
      attributes.push_back(VUSH_ALLOCATE(ast::Attribute, ctx.allocator,
                                         identifier, parameters,
                                         attribute_node.source_info));
    }
    return {anton::expected_value, attributes};
  }

  // transform_stmt
  //
  // Returns:
  // nullptr if the statement does not have a representation in the AST.
  //
  [[nodiscard]] static anton::Expected<ast::Node*, Error>
  transform_stmt(Context const& ctx, Syntax_Node const& node);
  [[nodiscard]] static anton::Expected<ast::Node_List, Error>
  transform_stmt_block_child_stmts(Context const& ctx, Syntax_Node const& node);

  anton::Expected<ast::Node_List, Error>
  transform_stmt_block_child_stmts(Context const& ctx, Syntax_Node const& node)
  {
    ANTON_ASSERT(node.kind == Syntax_Node_Kind::stmt_block,
                 "Syntax_Node is not stmt_block");
    auto& statements =
      *VUSH_ALLOCATE(Array<ast::Node*>, ctx.allocator, ctx.allocator);
    for(SNOT const& snot: node.children) {
      if(!snot.is_left()) {
        continue;
      }

      Syntax_Node const& stmt_node = snot.left();
      RETURN_ON_FAIL(stmt, transform_stmt, ctx, stmt_node);
      if(stmt.value() != nullptr) {
        statements.push_back(stmt.value());
      }
    }
    return {anton::expected_value, statements};
  }

  [[nodiscard]] static anton::Expected<ast::Variable*, Error>
  transform_variable(Context const& ctx, Syntax_Node const& node)
  {
    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_variable_attribute_list(node));

    ast::Identifier const identifier =
      transform_identifier(ctx, get_variable_identifier(node));
    RETURN_ON_FAIL(type, transform_type, ctx, get_variable_type(node));

    ast::Expr* initializer = nullptr;
    if(anton::Optional initializer_node = get_variable_initializer(node)) {
      RETURN_ON_FAIL(result, transform_expr, ctx, initializer_node.value());
      initializer = result.value();
    }
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Variable, ctx.allocator, attribute_list.value(),
                          type.value(), identifier, initializer,
                          node.source_info)};
  }

  anton::Expected<ast::Node*, Error> transform_stmt(Context const& ctx,
                                                    Syntax_Node const& node)
  {
    switch(node.kind) {
    case Syntax_Node_Kind::variable: {
      RETURN_ON_FAIL(variable, transform_variable, ctx, node);
      return {anton::expected_value, variable.value()};
    } break;

    case Syntax_Node_Kind::stmt_block: {
      RETURN_ON_FAIL(result, transform_stmt_block_child_stmts, ctx, node);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Block, ctx.allocator, result.value(),
                            node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_assignment: {
      RETURN_ON_FAIL(lhs, transform_expr, ctx, get_stmt_assignment_lhs(node));
      RETURN_ON_FAIL(rhs, transform_expr, ctx, get_stmt_assignment_rhs(node));
      Syntax_Token const& operator_token = get_stmt_assignment_operator(node);
      ast::Assignment_Kind const kind =
        map_token_to_assignment_kind(operator_token.kind);
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Assignment, ctx.allocator, kind,
                            lhs.value(), rhs.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_if: {
      RETURN_ON_FAIL(condition, transform_expr, ctx,
                     get_stmt_if_condition(node));
      RETURN_ON_FAIL(then_branch, transform_stmt_block_child_stmts, ctx,
                     get_stmt_if_then_branch(node));
      ast::Node_List else_branch;
      if(anton::Optional<Syntax_Node const&> else_node =
           get_stmt_if_else_branch(node)) {
        RETURN_ON_FAIL(result, transform_stmt_block_child_stmts, ctx,
                       else_node.value());
        else_branch = result.value();
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_If, ctx.allocator, condition.value(),
                            then_branch.value(), else_branch,
                            node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_switch: {
      RETURN_ON_FAIL(expression, transform_expr, ctx,
                     get_stmt_switch_expression(node));
      auto& arm_list =
        *VUSH_ALLOCATE(Array<ast::Switch_Arm*>, ctx.allocator, ctx.allocator);
      {
        Syntax_Node const& arm_list_node = get_stmt_switch_arm_list(node);
        for(SNOT const& snot: arm_list_node.children) {
          if(!snot.is_left()) {
            continue;
          }

          Syntax_Node const& arm_node = snot.left();
          auto& labels =
            *VUSH_ALLOCATE(Array<ast::Expr*>, ctx.allocator, ctx.allocator);
          for(SNOT const& snot: arm_node.children) {
            if(snot.is_left() &&
               snot.left().kind == Syntax_Node_Kind::switch_arm_label) {
              Syntax_Node const& expression_node =
                snot.left().children[0].left();
              RETURN_ON_FAIL(expression, transform_expr, ctx, expression_node);
              labels.push_back(expression.value());
            }
          }

          RETURN_ON_FAIL(statements, transform_stmt_block_child_stmts, ctx,
                         get_switch_arm_body(arm_node));
          ast::Switch_Arm* const arm = VUSH_ALLOCATE(
            ast::Switch_Arm, ctx.allocator, labels, statements.value());
          arm_list.push_back(arm);
        }
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Switch, ctx.allocator, expression.value(),
                            arm_list, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_for: {
      auto& declarations =
        *VUSH_ALLOCATE(Array<ast::Variable*>, ctx.allocator, ctx.allocator);
      if(anton::Optional variable_node = get_stmt_for_variable(node)) {
        RETURN_ON_FAIL(variable, transform_variable, ctx,
                       variable_node.value());
        declarations.push_back(variable.value());
      }

      ast::Expr* condition = nullptr;
      if(anton::Optional condition_node = get_stmt_for_condition(node)) {
        RETURN_ON_FAIL(result, transform_expr, ctx, condition_node.value());
        condition = result.value();
      }

      auto& actions =
        *VUSH_ALLOCATE(Array<ast::Expr*>, ctx.allocator, ctx.allocator);
      if(anton::Optional expression_node = get_stmt_for_expression(node)) {
        RETURN_ON_FAIL(expression, transform_expr, ctx,
                       expression_node.value());
        actions.push_back(expression.value());
      }

      RETURN_ON_FAIL(statements, transform_stmt_block_child_stmts, ctx,
                     get_stmt_for_body(node));

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_For, ctx.allocator, condition,
                            declarations, actions, statements.value(),
                            node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_while: {
      RETURN_ON_FAIL(condition, transform_expr, ctx,
                     get_stmt_while_condition(node));
      RETURN_ON_FAIL(statements, transform_stmt_block_child_stmts, ctx,
                     get_stmt_while_statements(node));
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_While, ctx.allocator, condition.value(),
                            statements.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_do_while: {
      RETURN_ON_FAIL(statements, transform_stmt_block_child_stmts, ctx,
                     get_stmt_do_while_body(node));
      RETURN_ON_FAIL(condition, transform_expr, ctx,
                     get_stmt_do_while_condition(node));
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Do_While, ctx.allocator,
                            condition.value(), statements.value(),
                            node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_return: {
      ast::Expr* expression = nullptr;
      if(anton::Optional expression_node = get_stmt_return_expression(node)) {
        RETURN_ON_FAIL(result, transform_expr, ctx, expression_node.value());
        expression = result.value();
      }
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Return, ctx.allocator, expression,
                            node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_break: {
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Break, ctx.allocator, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_continue: {
      return {
        anton::expected_value,
        VUSH_ALLOCATE(ast::Stmt_Continue, ctx.allocator, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_discard: {
      return {
        anton::expected_value,
        VUSH_ALLOCATE(ast::Stmt_Discard, ctx.allocator, node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_expression: {
      RETURN_ON_FAIL(expression, transform_expr, ctx,
                     get_stmt_expression_expression(node));
      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Stmt_Expression, ctx.allocator,
                            expression.value(), node.source_info)};
    } break;

    case Syntax_Node_Kind::stmt_empty: {
      return {anton::expected_value, nullptr};
    } break;

    default:
      ANTON_UNREACHABLE("unreachable");
    }
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
    RETURN_ON_FAIL(condition, evaluate_constant_expression, ctx,
                   get_decl_if_condition(node));
    if(condition.value()) {
      Syntax_Node const& then_node = get_decl_if_then_branch(node);
      return lower_syntax_to_ast(ctx, then_node.children);
    } else {
      anton::Optional<Syntax_Node const&> else_node =
        get_decl_if_else_branch(node);
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

  [[nodiscard]] static anton::Expected<ast::Decl_Struct*, Error>
  transform_decl_struct(Context const& ctx, Syntax_Node const& node)
  {
    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_decl_struct_attribute_list(node));

    auto& members =
      *VUSH_ALLOCATE(Array<ast::Struct_Field*>, ctx.allocator, ctx.allocator);
    Syntax_Node const& members_node = get_decl_struct_fields(node);
    for(SNOT const& member_snot: members_node.children) {
      if(!member_snot.is_left()) {
        continue;
      }

      Syntax_Node const& member_node = member_snot.left();
      anton::Expected<ast::Attr_List, Error> attribute_list =
        transform_attribute_list(ctx,
                                 get_struct_field_attribute_list(member_node));
      if(!attribute_list) {
        return {anton::expected_error, ANTON_MOV(attribute_list.error())};
      }

      ast::Identifier const identifier =
        transform_identifier(ctx, get_struct_field_identifier(member_node));
      RETURN_ON_FAIL(type, transform_type, ctx,
                     get_struct_field_type(member_node));
      ast::Expr* initializer = nullptr;
      anton::Optional initializer_node =
        get_struct_field_initializer(member_node);
      if(initializer_node) {
        RETURN_ON_FAIL(result, transform_expr, ctx, initializer_node.value());
        initializer = result.value();
      }

      members.push_back(VUSH_ALLOCATE(ast::Struct_Field, ctx.allocator,
                                      attribute_list.value(), identifier,
                                      type.value(), initializer));
    }

    ast::Identifier const identifier =
      transform_identifier(ctx, get_decl_struct_identifier(node));
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Decl_Struct, ctx.allocator,
                          attribute_list.value(), identifier, members,
                          node.source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Buffer*, Error>
  transform_decl_buffer(Context const& ctx, Syntax_Node const& node)
  {
    RETURN_ON_FAIL(attribute_list, transform_attribute_list, ctx,
                   get_decl_buffer_attribute_list(node));

    auto& fields =
      *VUSH_ALLOCATE(Array<ast::Buffer_Field*>, ctx.allocator, ctx.allocator);
    Syntax_Node const& fields_node = get_decl_buffer_fields(node);
    for(SNOT const& field_snot: fields_node.children) {
      if(!field_snot.is_left()) {
        continue;
      }

      Syntax_Node const& field_node = field_snot.left();
      anton::Expected<ast::Attr_List, Error> attribute_list =
        transform_attribute_list(ctx,
                                 get_buffer_field_attribute_list(field_node));
      if(!attribute_list) {
        return {anton::expected_error, ANTON_MOV(attribute_list.error())};
      }

      ast::Identifier const identifier =
        transform_identifier(ctx, get_buffer_field_identifier(field_node));
      RETURN_ON_FAIL(type, transform_type, ctx,
                     get_buffer_field_type(field_node));
      fields.push_back(VUSH_ALLOCATE(ast::Buffer_Field, ctx.allocator,
                                     attribute_list.value(), identifier,
                                     type.value()));
    }

    ast::Identifier const identifier =
      transform_identifier(ctx, get_decl_buffer_identifier(node));
    ast::Identifier const pass =
      transform_identifier(ctx, get_decl_buffer_pass(node));
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Decl_Buffer, ctx.allocator,
                          attribute_list.value(), pass, identifier, fields,
                          node.source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Fn_Parameter*, Error>
  transform_parameter(Context const& ctx, Syntax_Node const& node)
  {
    if(node.kind == Syntax_Node_Kind::fn_parameter) {
      ast::Identifier const identifier =
        transform_identifier(ctx, get_fn_parameter_identifier(node));
      RETURN_ON_FAIL(type, transform_type, ctx, get_fn_parameter_type(node));
      ast::Identifier source;
      if(anton::Optional result = get_fn_parameter_source(node)) {
        source = transform_identifier(ctx, result.value());
      }

      return {anton::expected_value,
              VUSH_ALLOCATE(ast::Fn_Parameter, ctx.allocator, identifier,
                            type.value(), source, node.source_info)};
    } else if(node.kind == Syntax_Node_Kind::fn_parameter_if) {
      RETURN_ON_FAIL(condition, evaluate_constant_expression, ctx,
                     get_fn_parameter_if_condition(node));
      if(condition.value()) {
        Syntax_Node const& then_node = get_fn_parameter_if_then_branch(node);
        return transform_parameter(ctx, then_node);
      } else {
        Syntax_Node const& else_node = get_fn_parameter_if_else_branch(node);
        return transform_parameter(ctx, else_node);
      }
    } else {
      // TODO: Error.
      ANTON_UNREACHABLE("unreachable");
    }
  }

  [[nodiscard]] static anton::Expected<ast::Fn_Parameter_List, Error>
  transform_parameter_list(Context const& ctx, Syntax_Node const& node)
  {
    auto& parameters =
      *VUSH_ALLOCATE(Array<ast::Fn_Parameter*>, ctx.allocator, ctx.allocator);
    for(SNOT const& snot: node.children) {
      if(!snot.is_left()) {
        continue;
      }

      Syntax_Node const& parameter_node = snot.left();
      RETURN_ON_FAIL(parameter, transform_parameter, ctx, parameter_node);
      parameters.push_back(parameter.value());
    }
    return {anton::expected_value, parameters};
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Function*, Error>
  transform_decl_function(Context const& ctx, Syntax_Node const& node)
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
            VUSH_ALLOCATE(ast::Decl_Function, ctx.allocator,
                          attribute_list.value(), identifier,
                          parameters.value(), return_type.value(), body.value(),
                          false, node.source_info)};
  }

  [[nodiscard]] static anton::Expected<ast::Decl_Stage_Function*, Error>
  transform_decl_stage_function(Context const& ctx, Syntax_Node const& node)
  {
    auto transform_stage_kind =
      [](Syntax_Token const& token) -> ast::With_Source<Stage_Kind> {
      if(token.value == "vertex"_sv) {
        return {Stage_Kind::vertex, token.source_info};
      } else if(token.value == "fragment"_sv) {
        return {Stage_Kind::fragment, token.source_info};
      } else if(token.value == "compute"_sv) {
        return {Stage_Kind::compute, token.source_info};
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
    RETURN_ON_FAIL(return_type, transform_type, ctx,
                   get_decl_stage_function_return_type(node));
    RETURN_ON_FAIL(body, transform_stmt_block_child_stmts, ctx,
                   get_decl_stage_function_body(node));
    return {anton::expected_value,
            VUSH_ALLOCATE(ast::Decl_Stage_Function, ctx.allocator,
                          attribute_list.value(), pass, stage,
                          parameters.value(), return_type.value(), body.value(),
                          node.source_info)};
  }

  anton::Expected<ast::Node_List, Error>
  lower_syntax_to_ast(Context& ctx, Array<SNOT> const& syntax)
  {
    auto& abstract =
      *VUSH_ALLOCATE(Array<ast::Node*>, ctx.allocator, ctx.allocator);
    for(SNOT const& snot: syntax) {
      if(!snot.is_left()) {
        continue;
      }

      Syntax_Node const& syntax_node = snot.left();
      switch(syntax_node.kind) {
      case Syntax_Node_Kind::decl_if: {
        RETURN_ON_FAIL(result, transform_decl_if, ctx, syntax_node);
        ast::Node_List const decls = result.value();
        abstract.insert(abstract.end(), decls.begin(), decls.end());
      } break;

      case Syntax_Node_Kind::decl_import: {
        RETURN_ON_FAIL(result, transform_decl_import, ctx, syntax_node);
        ast::Node_List const decls = result.value();
        abstract.insert(abstract.end(), decls.begin(), decls.end());
      } break;

      case Syntax_Node_Kind::variable: {
        RETURN_ON_FAIL(result, transform_variable, ctx, syntax_node);
        abstract.insert(abstract.end(), result.value());
      } break;

      case Syntax_Node_Kind::decl_struct: {
        RETURN_ON_FAIL(result, transform_decl_struct, ctx, syntax_node);
        ast::Decl_Struct* const decl = result.value();
        abstract.insert(abstract.end(), decl);
      } break;

      case Syntax_Node_Kind::decl_buffer: {
        RETURN_ON_FAIL(result, transform_decl_buffer, ctx, syntax_node);
        ast::Decl_Buffer* const decl = result.value();
        abstract.insert(abstract.end(), decl);
      } break;

      case Syntax_Node_Kind::decl_settings: {
        // TODO
      } break;

      case Syntax_Node_Kind::decl_function: {
        RETURN_ON_FAIL(result, transform_decl_function, ctx, syntax_node);
        ast::Decl_Function* const decl = result.value();
        abstract.insert(abstract.end(), decl);
      } break;

      case Syntax_Node_Kind::decl_stage_function: {
        RETURN_ON_FAIL(result, transform_decl_stage_function, ctx, syntax_node);
        ast::Decl_Stage_Function* const decl = result.value();
        abstract.insert(abstract.end(), decl);
      } break;

      default:
        ANTON_UNREACHABLE("unreachable");
      }
    }
    return {anton::expected_value, abstract};
  }
} // namespace vush
