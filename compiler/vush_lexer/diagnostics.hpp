#pragma once

#include <vush_core/types.hpp>
#include <vush_diagnostics/error.hpp>

namespace vush {
  struct Context;

  [[nodiscard]] Error
  err_lexer_newline_in_string_literal(Context const& ctx,
                                      anton::String_View source_path,
                                      i64 offset, i64 line, i64 column);
  [[nodiscard]] Error
  err_lexer_unrecognised_token(Context const& ctx,
                               anton::String_View source_path, i64 offset,
                               i64 line, i64 column);
  [[nodiscard]] Error err_lexer_unexpected_eof(Context const& ctx,
                                               anton::String_View source_path,
                                               i64 offset, i64 line,
                                               i64 column);
  [[nodiscard]] Error err_lexer_not_fp_constant(Context const& ctx,
                                                anton::String_View source_path,
                                                i64 offset, i64 end_offset,
                                                i64 line, i64 column);
  [[nodiscard]] Error
  err_lexer_missing_exponent_digits(Context const& ctx,
                                    anton::String_View source_path, i64 offset,
                                    i64 line, i64 column);
  [[nodiscard]] Error
  err_lexer_invalid_digit_in_binary_literal(Context const& ctx,
                                            anton::String_View source_path,
                                            i64 offset, i64 line, i64 column);
} // namespace vush
