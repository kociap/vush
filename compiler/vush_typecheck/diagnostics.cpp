#include <vush_typecheck/diagnostics.hpp>

#include <anton/format.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/context.hpp>
#include <vush_diagnostics/utility.hpp>

namespace vush {
  using namespace anton::literals;

  [[nodiscard]] static anton::String stringify_call_argument_types(Context const& ctx,
                                                                   ast::Expr_List const arguments)
  {
    anton::String result(ctx.allocator);
    result += "("_sv;
    for(bool first = true; ast::Expr const* const argument: arguments) {
      ast::Type const* const type = ctx.find_node_type(argument);
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

  Error err_no_matching_overload(Context const& ctx, ast::Expr_Call const* const call,
                                 anton::Slice<ast::Decl_Function const* const> const overloads)
  {
    Error error = error_from_source(ctx.allocator, call->source_info);
    anton::String arguments = stringify_call_argument_types(ctx, call->arguments);
    bool const is_operator = anton::begins_with(call->identifier.value, "operator"_sv);
    if(is_operator) {
      error.diagnostic = anton::format("error: no matching '{}' for arguments '{}'"_sv,
                                       call->identifier.value, arguments);
    } else {
      error.diagnostic =
        anton::format("error: no matching function for call to '{}' with arguments '{}'"_sv,
                      call->identifier.value, arguments);
    }
    anton::String_View const source = ctx.find_source(call->source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, call->identifier.source_info);
    for(ast::Decl_Function const* const fn: overloads) {
      auto result = ctx.find_source(fn->source_info.source_path);
      if(result != nullptr) {
        ANTON_ASSERT(!fn->builtin, "builtin function has source");
        error.extended_diagnostic += '\n';
        anton::String_View const source = result->data;
        Source_Info const& fn_info = fn->identifier.source_info;
        error.extended_diagnostic += format_diagnostic_location(ctx.allocator, fn_info.source_path,
                                                                fn_info.line, fn_info.column);
        error.extended_diagnostic += "note: function is not a viable candidate\n"_sv;
        print_source_snippet(ctx, error.extended_diagnostic, source, fn->identifier.source_info);
        error.extended_diagnostic += " candidate function not viable"_sv;
      } else {
        // Do not output builtin operators as that leads to extremely long diagnostic messages.
        if(is_operator) {
          continue;
        }

        ANTON_ASSERT(fn->builtin, "non-builtin function has no source");
        error.extended_diagnostic += '\n';
        error.extended_diagnostic += format_diagnostic_location(ctx.allocator, "<vush>", 1, 1);
        error.extended_diagnostic += "note: builtin function is not a viable candidate\n"_sv;
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

  Error err_ambiguous_overload(Context const& ctx, ast::Expr_Call const* const call,
                               anton::Slice<ast::Decl_Function const* const> const candidates)
  {
    Error error = error_from_source(ctx.allocator, call->source_info);
    anton::String arguments = stringify_call_argument_types(ctx, call->arguments);
    error.diagnostic = anton::format("error: ambiguous call to '{}' with arguments '{}'"_sv,
                                     call->identifier.value, arguments);
    anton::String_View const source = ctx.find_source(call->source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, call->source_info);
    for(ast::Decl_Function const* const fn: candidates) {
      error.extended_diagnostic += '\n';
      auto result = ctx.find_source(fn->source_info.source_path);
      if(result != nullptr) {
        ANTON_ASSERT(!fn->builtin, "builtin function has source");
        anton::String_View const source = result->data;
        Source_Info const& fn_info = fn->identifier.source_info;
        error.extended_diagnostic += format_diagnostic_location(ctx.allocator, fn_info.source_path,
                                                                fn_info.line, fn_info.column);
        error.extended_diagnostic += "note: viable candidate function\n"_sv;
        print_source_snippet(ctx, error.extended_diagnostic, source, fn->identifier.source_info);
      } else {
        ANTON_ASSERT(fn->builtin, "non-builtin function has no source");
        error.extended_diagnostic += format_diagnostic_location(ctx.allocator, "<vush>", 1, 1);
        error.extended_diagnostic += "note: viable candidate builtin function\n"_sv;
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

  Error err_cannot_convert_type(Context const& ctx, Source_Info const& where, ast::Type const* to,
                                ast::Type const* from)
  {
    Error error = error_from_source(ctx.allocator, where);
    anton::String_View const source = ctx.find_source(where.source_path)->data;
    anton::String from_string = stringify_type(ctx, from);
    anton::String to_string = stringify_type(ctx, to);
    error.diagnostic =
      anton::format("error: cannot convert '{}' to '{}'"_sv, from_string, to_string);
    print_source_snippet(ctx, error.extended_diagnostic, source, where);
    error.extended_diagnostic += anton::format(" '{}' cannot be converted"_sv, from_string);
    return error;
  }

  Error err_no_assignment_operator(Context const& ctx, ast::Type const* const from_type,
                                   ast::Type const* const to_type,
                                   ast::Expr_Assignment const* const expr)
  {
    Error error = error_from_source(ctx.allocator, expr->source_info);
    anton::String from_string = stringify_type(ctx, from_type);
    anton::String to_string = stringify_type(ctx, to_type);
    error.diagnostic =
      anton::format("error: no viable assignment from '{}' to '{}'"_sv, from_string, to_string);
    anton::String_View const source = ctx.find_source(expr->source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, expr->source_info);
    error.extended_diagnostic +=
      anton::format(" '{}' cannot be assigned to '{}'"_sv, from_string, to_string);
    return error;
  }

  Error err_vector_swizzle_invalid(Context const& ctx, ast::Identifier const& field)
  {
    Source_Info const source_info = field.source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source = ctx.find_source(source_info.source_path)->data;
    anton::String_View const field_code = get_source_bit(source, source_info);
    error.diagnostic = anton::format("error: invalid vector swizzle '{}'"_sv, field_code);
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic +=
      " vector swizzle must contain at most 4 of { x, y, z, w, r, g, b, a, s, t, u, v }"_sv;
    return error;
  }

  Error err_vector_swizzle_overlong(Context const& ctx, ast::Type_Builtin const* type,
                                    ast::Identifier const& field)
  {
    Source_Info const source_info = field.source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source = ctx.find_source(source_info.source_path)->data;
    anton::String_View const field_code = get_source_bit(source, source_info);
    error.diagnostic =
      anton::format("error: vector swizzle '{}' overlong for type '{}'"_sv, field_code, ""_sv);
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    return error;
  }

  Error err_matrix_field_invalid(Context const& ctx, ast::Identifier const* field)
  {
    Error error = error_from_source(ctx.allocator, field->source_info);
    error.diagnostic = anton::format("error: invalid matrix field '{}'"_sv, field->value);
    // TODO: Extended diagnostic.
    return error;
  }

  Error err_type_has_no_field_named(Context const& ctx, ast::Type const* type,
                                    ast::Identifier const& field_identifier)
  {
    Source_Info const field_source_info = field_identifier.source_info;
    Source_Info const type_source_info = type->source_info;
    Error error = error_from_source(ctx.allocator, field_source_info);
    anton::String_View const type_source = ctx.find_source(type_source_info.source_path)->data;
    anton::String_View const type_value = get_source_bit(type_source, type_source_info);
    anton::String_View const field_source = ctx.find_source(field_source_info.source_path)->data;
    anton::String_View const field_value = get_source_bit(field_source, field_source_info);
    error.diagnostic =
      anton::format("error: '{}' does not have field '{}'"_sv, type_value, field_value);
    print_source_snippet(ctx, error.extended_diagnostic, field_source, field_source_info);
    return error;
  }
} // namespace vush
