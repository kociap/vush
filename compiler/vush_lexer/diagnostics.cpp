#include <vush_lexer/diagnostics.hpp>

#include <anton/format.hpp>

#include <vush_core/context.hpp>
#include <vush_diagnostics/utility.hpp>

namespace vush {
  using namespace anton::literals;

  // Lexer errors occur during lexing a file, hence the sources must always
  // already be available in the context.

  Error err_lexer_newline_in_string_literal(Context const& ctx,
                                            anton::String_View const source_path, i64 const offset,
                                            i64 const line, i64 const column)
  {
    Error error = error_from_source(ctx.allocator, source_path, line, column);
    error.diagnostic =
      anton::String("error: newlines are not allowed in string literals"_sv, ctx.allocator);
    anton::String_View const source = ctx.source_registry->find_source(source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, offset, offset + 1, line);
    error.extended_diagnostic += "newline before string has been terminated"_sv;
    return error;
  }

  Error err_lexer_unrecognised_token(Context const& ctx, anton::String_View const source_path,
                                     i64 const offset, i64 const line, i64 const column)
  {
    Error error = error_from_source(ctx.allocator, source_path, line, column);
    anton::String_View const source = ctx.source_registry->find_source(source_path)->data;
    anton::String_View const token = get_source_bit(source, offset, offset + 1);
    error.diagnostic = anton::format(ctx.allocator, "error: unrecognised token '{}'"_sv, token);
    print_source_snippet(ctx, error.extended_diagnostic, source, offset, offset + 1, line);
    return error;
  }

  Error err_lexer_unexpected_eof(Context const& ctx, anton::String_View const source_path,
                                 i64 const offset, i64 const line, i64 const column)
  {
    Error error = error_from_source(ctx.allocator, source_path, line, column);
    error.diagnostic = anton::String("error: unexpected end of file"_sv, ctx.allocator);
    anton::String_View const source = ctx.source_registry->find_source(source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, offset, offset + 1, line);
    return error;
  }

  Error err_lexer_not_fp_constant(Context const& ctx, anton::String_View const source_path,
                                  i64 const offset, i64 const end_offset, i64 const line,
                                  i64 const column)
  {
    Error error = error_from_source(ctx.allocator, source_path, line, column);
    error.diagnostic =
      anton::format(ctx.allocator, "error: '{}' is not a floating point constant"_sv);
    anton::String_View const source = ctx.source_registry->find_source(source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, offset, end_offset, line);
    return error;
  }

  Error err_lexer_missing_exponent_digits(Context const& ctx, anton::String_View const source_path,
                                          i64 const offset, i64 const line, i64 const column)
  {
    Error error = error_from_source(ctx.allocator, source_path, line, column);
    anton::String_View const source = ctx.source_registry->find_source(source_path)->data;
    error.diagnostic = anton::String("error: exponent has no digits"_sv, ctx.allocator);
    print_source_snippet(ctx, error.extended_diagnostic, source, offset, offset + 1, line);
    error.extended_diagnostic += " exponent must have at least one digit";
    return error;
  }

  Error err_lexer_invalid_digit_in_binary_literal(Context const& ctx,
                                                  anton::String_View const source_path,
                                                  i64 const offset, i64 const line,
                                                  i64 const column)
  {
    Error error = error_from_source(ctx.allocator, source_path, line, column);
    anton::String_View const source = ctx.source_registry->find_source(source_path)->data;
    anton::String_View const digit = get_source_bit(source, offset, offset + 1);
    error.diagnostic =
      anton::format(ctx.allocator, "error: invalid digit '{}' in binary literal"_sv, digit);
    print_source_snippet(ctx, error.extended_diagnostic, source, offset, offset + 1, line);
    error.extended_diagnostic += " allowed digits are '0' and '1'"_sv;
    return error;
  }
} // namespace vush
