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
    error.diagnostic =
      anton::format("error: no matching function for call to '{}' with arguments '{}'"_sv,
                    call->identifier->value, arguments);
    anton::String_View const source = ctx.find_source(call->source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, call->identifier->source_info);
    for(ast::Decl_Function const* const fn: overloads) {
      auto result = ctx.find_source(fn->source_info.source_path);
      if(result != nullptr) {
        ANTON_ASSERT(!fn->builtin, "builtin function has source");
        anton::String_View const source = result->data;
        print_source_snippet(ctx, error.extended_diagnostic, source, fn->identifier->source_info);
        error.extended_diagnostic += " candidate function not viable"_sv;
      } else {
        ANTON_ASSERT(fn->builtin, "non-builtin function has no source");
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
                                     call->identifier->value, arguments);
    anton::String_View const source = ctx.find_source(call->source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, call->source_info);
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
} // namespace vush
