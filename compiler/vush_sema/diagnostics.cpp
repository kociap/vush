#include <vush_sema/diagnostics.hpp>

#include <anton/format.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/context.hpp>
#include <vush_diagnostics/utility.hpp>

namespace vush {
  using namespace anton::literals;

  Error err_undefined_symbol(Context const& ctx, Source_Info const& symbol)
  {
    Error error = error_from_source(ctx.allocator, symbol);
    anton::String_View const source =
      ctx.source_registry->find_source(symbol.source_path)->data;
    anton::String_View const name = get_source_bit(source, symbol);
    error.diagnostic =
      anton::format(ctx.allocator, u8"error: undefined symbol '{}'"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, symbol);
    error.extended_diagnostic +=
      anton::format(ctx.allocator, u8" '{}' used, but not defined"_sv, name);
    return error;
  }

  Error err_symbol_redefinition(Context const& ctx,
                                Source_Info const& old_symbol,
                                Source_Info const& new_symbol)
  {
    Error error = error_from_source(ctx.allocator, new_symbol);
    anton::String_View const new_source =
      ctx.source_registry->find_source(new_symbol.source_path)->data;
    anton::String_View const old_source =
      ctx.source_registry->find_source(old_symbol.source_path)->data;
    anton::String_View const name = get_source_bit(new_source, new_symbol);
    error.diagnostic = anton::format(
      ctx.allocator, u8"error: symbol '{}' is defined multiple times"_sv, name);
    error.extended_diagnostic =
      format_diagnostic_location(ctx.allocator, old_symbol);
    error.extended_diagnostic += '\n';
    print_source_snippet(ctx, error.extended_diagnostic, old_source,
                         old_symbol);
    error.extended_diagnostic +=
      anton::format(ctx.allocator, u8" definition of '{}' here\n"_sv, name);
    error.extended_diagnostic +=
      format_diagnostic_location(ctx.allocator, new_symbol);
    error.extended_diagnostic += '\n';
    print_source_snippet(ctx, error.extended_diagnostic, new_source,
                         new_symbol);
    error.extended_diagnostic += u8" redefined here"_sv;
    return error;
  }

  Error err_init_invalid_vector_initializer_kind(
    Context const& ctx, ast::Initializer const* const initializer)
  {
    Source_Info const source_info = initializer->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    error.diagnostic =
      "error: vector initializer is not a basic initializer"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic +=
      " vector initializers must be basic initializers"_sv;
    return error;
  }

  Error err_init_invalid_matrix_initializer_kind(
    Context const& ctx, ast::Initializer const* const initializer)
  {
    Source_Info const source_info = initializer->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    error.diagnostic =
      "error: matrix initializer is not a basic initializer"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic +=
      " matrix initializers must be basic initializers"_sv;
    return error;
  }

  Error err_init_invalid_struct_initializer_kind(
    Context const& ctx, ast::Initializer const* const initializer)
  {
    Source_Info const source_info = initializer->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    error.diagnostic = "error: initializer is not field initializer"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic +=
      " struct initializers must be field initializers"_sv;
    return error;
  }

  Error err_variable_type_unsized_array(Context const& ctx,
                                        ast::Variable const* variable)
  {
    Source_Info const source_info = variable->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    error.diagnostic = "error: variable type must not be unsized array"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    return error;
  }

  Error err_variable_type_opaque(Context const& ctx,
                                 ast::Variable const* variable)
  {
    Source_Info const source_info = variable->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    error.diagnostic = "error: variable type must not be opaque"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    return error;
  }

  Error err_opaque_type_non_assignable(Context const& ctx,
                                       ast::Stmt_Assignment const* assignment)
  {
    Source_Info const source_info = assignment->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    error.diagnostic =
      "error: the left hand side of the assignment has an opaque type";
    // TODO: Explain why the type is opaque.
    return error;
  }

  Error err_assignment_to_immutable(Context const& ctx,
                                    ast::Stmt_Assignment const* assignment)
  {
    Source_Info const source_info = assignment->source_info;
    Source_Info const lhs_source_info = assignment->lhs->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    error.diagnostic = "error: cannot assign to an immutable location"_sv;
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source,
                         lhs_source_info);
    error.extended_diagnostic += " LHS is immutable"_sv;
    return error;
  }

  Error err_arithmetic_assignment_to_non_arithmetic_type(
    Context const& ctx, ast::Stmt_Assignment const* assignment)
  {
    Source_Info const source_info = assignment->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    error.diagnostic =
      "error: arithmetic assignment to non-arithmetic type is not allowed";
    // TODO: Include the token of the assignment in the short diagnostic.
    return error;
  }

  [[nodiscard]] static anton::String
  stringify_call_argument_types(Context const& ctx,
                                ast::Expr_List const arguments)
  {
    anton::String result(ctx.allocator);
    result += "("_sv;
    for(bool first = true; ast::Expr const* const argument: arguments) {
      ast::Type const* const type = argument->evaluated_type;
      ANTON_ASSERT(type != nullptr, "argument does not have type");
      if(!first) {
        result += ", "_sv;
      }
      first = false;
      result += stringify_type(ctx, type);
    }
    result += ")"_sv;
    return result;
  }

  Error err_no_matching_overload(
    Context const& ctx, ast::Expr_Call const* const call,
    anton::Slice<ast::Decl_Function const* const> const overloads)
  {
    Error error = error_from_source(ctx.allocator, call->source_info);
    anton::String arguments =
      stringify_call_argument_types(ctx, call->arguments);
    bool const is_operator =
      anton::begins_with(call->identifier.value, "operator"_sv);
    if(is_operator) {
      error.diagnostic =
        anton::format("error: no matching '{}' for arguments '{}'"_sv,
                      call->identifier.value, arguments);
    } else {
      error.diagnostic = anton::format(
        "error: no matching function for call to '{}' with arguments '{}'"_sv,
        call->identifier.value, arguments);
    }
    anton::String_View const source =
      ctx.source_registry->find_source(call->source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source,
                         call->identifier.source_info);
    for(ast::Decl_Function const* const fn: overloads) {
      auto result =
        ctx.source_registry->find_source(fn->source_info.source_path);
      if(result != nullptr) {
        ANTON_ASSERT(!fn->builtin, "builtin function has source");
        error.extended_diagnostic += '\n';
        anton::String_View const source = result->data;
        Source_Info const& fn_info = fn->identifier.source_info;
        error.extended_diagnostic += format_diagnostic_location(
          ctx.allocator, fn_info.source_path, fn_info.line, fn_info.column);
        error.extended_diagnostic +=
          "note: function is not a viable candidate\n"_sv;
        print_source_snippet(ctx, error.extended_diagnostic, source,
                             fn->identifier.source_info);
        error.extended_diagnostic += " candidate function not viable"_sv;
      } else {
        // Do not output builtin operators as that leads to extremely long diagnostic messages.
        if(is_operator) {
          continue;
        }

        ANTON_ASSERT(fn->builtin, "non-builtin function has no source");
        error.extended_diagnostic += '\n';
        error.extended_diagnostic +=
          format_diagnostic_location(ctx.allocator, "<vush>", 1, 1);
        error.extended_diagnostic +=
          "note: builtin function is not a viable candidate\n"_sv;
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
        error.extended_diagnostic += '\n';
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
        error.extended_diagnostic += stringify_builtin_function(ctx, fn);
        error.extended_diagnostic += '\n';
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
      }
    }
    return error;
  }

  Error err_ambiguous_overload(
    Context const& ctx, ast::Expr_Call const* const call,
    anton::Slice<ast::Decl_Function const* const> const candidates)
  {
    Error error = error_from_source(ctx.allocator, call->source_info);
    anton::String arguments =
      stringify_call_argument_types(ctx, call->arguments);
    error.diagnostic =
      anton::format("error: ambiguous call to '{}' with arguments '{}'"_sv,
                    call->identifier.value, arguments);
    anton::String_View const source =
      ctx.source_registry->find_source(call->source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source,
                         call->source_info);
    for(ast::Decl_Function const* const fn: candidates) {
      error.extended_diagnostic += '\n';
      auto result =
        ctx.source_registry->find_source(fn->source_info.source_path);
      if(result != nullptr) {
        ANTON_ASSERT(!fn->builtin, "builtin function has source");
        anton::String_View const source = result->data;
        Source_Info const& fn_info = fn->identifier.source_info;
        error.extended_diagnostic += format_diagnostic_location(
          ctx.allocator, fn_info.source_path, fn_info.line, fn_info.column);
        error.extended_diagnostic += "note: viable candidate function\n"_sv;
        print_source_snippet(ctx, error.extended_diagnostic, source,
                             fn->identifier.source_info);
      } else {
        ANTON_ASSERT(fn->builtin, "non-builtin function has no source");
        error.extended_diagnostic +=
          format_diagnostic_location(ctx.allocator, "<vush>", 1, 1);
        error.extended_diagnostic +=
          "note: viable candidate builtin function\n"_sv;
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
        error.extended_diagnostic += '\n';
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
        error.extended_diagnostic += stringify_builtin_function(ctx, fn);
        error.extended_diagnostic += '\n';
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
      }
    }
    return error;
  }

  Error err_cannot_convert_type(Context const& ctx, Source_Info const& where,
                                ast::Type const* to, ast::Type const* from)
  {
    Error error = error_from_source(ctx.allocator, where);
    anton::String_View const source =
      ctx.source_registry->find_source(where.source_path)->data;
    anton::String from_string = stringify_type(ctx, from);
    anton::String to_string = stringify_type(ctx, to);
    error.diagnostic = anton::format("error: cannot convert '{}' to '{}'"_sv,
                                     from_string, to_string);
    print_source_snippet(ctx, error.extended_diagnostic, source, where);
    error.extended_diagnostic +=
      anton::format(" '{}' cannot be converted"_sv, from_string);
    return error;
  }

  Error err_no_assignment_operator(Context const& ctx,
                                   ast::Type const* const from_type,
                                   ast::Type const* const to_type,
                                   ast::Stmt_Assignment const* const assignment)
  {
    Error error = error_from_source(ctx.allocator, assignment->source_info);
    anton::String from_string = stringify_type(ctx, from_type);
    anton::String to_string = stringify_type(ctx, to_type);
    error.diagnostic =
      anton::format("error: no viable assignment from '{}' to '{}'"_sv,
                    from_string, to_string);
    anton::String_View const source =
      ctx.source_registry->find_source(assignment->source_info.source_path)
        ->data;
    print_source_snippet(ctx, error.extended_diagnostic, source,
                         assignment->source_info);
    error.extended_diagnostic += anton::format(
      " '{}' cannot be assigned to '{}'"_sv, from_string, to_string);
    return error;
  }

  Error err_vector_swizzle_invalid(Context const& ctx,
                                   ast::Identifier const& field)
  {
    Source_Info const source_info = field.source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    anton::String_View const field_code = get_source_bit(source, source_info);
    error.diagnostic =
      anton::format("error: invalid vector swizzle '{}'"_sv, field_code);
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic +=
      " vector swizzle must contain at most 4 of { x, y, z, w, r, g, b, a, s, t, u, v }"_sv;
    return error;
  }

  Error err_vector_swizzle_overlong(Context const& ctx,
                                    ast::Type_Builtin const* type,
                                    ast::Identifier const& field)
  {
    Source_Info const source_info = field.source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    anton::String_View const field_code = get_source_bit(source, source_info);
    error.diagnostic =
      anton::format("error: vector swizzle '{}' overlong for type '{}'"_sv,
                    field_code, ""_sv);
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    return error;
  }

  Error err_vector_lvalue_swizzle_overlong(Context const& ctx,
                                           ast::Expr_Field const* const expr)
  {
    Source_Info const field_source_info = expr->field.source_info;
    Error error = error_from_source(ctx.allocator, field_source_info);
    error.diagnostic =
      "error: lvalue vector swizzle longer than the vector type"_sv;
    return error;
  }

  Error err_vector_lvalue_swizzle_duplicate_components(
    Context const& ctx, ast::Expr_Field const* const expr)
  {
    Source_Info const field_source_info = expr->field.source_info;
    Error error = error_from_source(ctx.allocator, field_source_info);
    error.diagnostic =
      "error: lvalue vector swizzle contains duplicate components"_sv;
    return error;
  }

  Error err_expr_default_not_lvalue(Context const& ctx,
                                    ast::Expr_Default const* expr)
  {
    return Error{};
  }

  Error err_lt_bool_not_lvalue(Context const& ctx, ast::Lt_Bool const* expr)
  {
    return Error{};
  }

  Error err_lt_integer_not_lvalue(Context const& ctx,
                                  ast::Lt_Integer const* expr)
  {
    return Error{};
  }

  Error err_lt_float_not_lvalue(Context const& ctx, ast::Lt_Float const* expr)
  {
    return Error{};
  }

  Error err_expr_call_not_lvalue(Context const& ctx, ast::Expr_Call const* expr)
  {
    return Error{};
  }

  Error err_expr_init_not_lvalue(Context const& ctx, ast::Expr_Init const* expr)
  {
    return Error{};
  }

  Error err_expr_if_not_lvalue(Context const& ctx, ast::Expr_If const* expr)
  {
    return Error{};
  }

  Error err_unknown_vector_type(Context const& ctx, ast::Type const* type)
  {
    return Error{};
  }

  Error err_matrix_field_invalid(Context const& ctx,
                                 ast::Identifier const* field)
  {
    return Error{};
  }

  Error err_type_has_no_field_named(Context const& ctx, ast::Type const* type,
                                    ast::Identifier const& field_identifier)
  {
    Source_Info const field_source_info = field_identifier.source_info;
    Source_Info const type_source_info = type->source_info;
    Error error = error_from_source(ctx.allocator, field_source_info);
    anton::String_View const type_source =
      ctx.source_registry->find_source(type_source_info.source_path)->data;
    anton::String_View const type_value =
      get_source_bit(type_source, type_source_info);
    anton::String_View const field_source =
      ctx.source_registry->find_source(field_source_info.source_path)->data;
    anton::String_View const field_value =
      get_source_bit(field_source, field_source_info);
    error.diagnostic = anton::format("error: '{}' does not have field '{}'"_sv,
                                     type_value, field_value);
    print_source_snippet(ctx, error.extended_diagnostic, field_source,
                         field_source_info);
    return error;
  }
} // namespace vush
