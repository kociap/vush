#pragma once

#include <anton/expected.hpp>
#include <anton/iterators/zip.hpp>
#include <anton/string7_view.hpp>

#include <vush_core/types.hpp>
#include <vush_diagnostics/error.hpp>

namespace vush {
  struct Context;

  enum struct Token_Kind {
    identifier,
    comment,
    whitespace,
    // keywords
    kw_if,
    kw_else,
    kw_switch,
    kw_default,
    kw_for,
    kw_while,
    kw_do,
    kw_return,
    kw_break,
    kw_continue,
    kw_discard,
    kw_from,
    kw_struct,
    kw_import,
    kw_var,
    kw_mut,
    kw_settings,
    kw_reinterpret,
    // separators
    tk_lbrace,
    tk_rbrace,
    tk_lbracket,
    tk_rbracket,
    tk_lparen,
    tk_rparen,
    tk_langle,
    tk_rangle,
    tk_semicolon,
    tk_colon,
    tk_comma,
    tk_dot,
    tk_double_quote,
    tk_at,
    tk_plus,
    tk_minus,
    tk_asterisk,
    tk_slash,
    tk_percent,
    tk_amp,
    tk_pipe,
    tk_hat,
    tk_bang,
    tk_tilde,
    tk_equals,
    // literals
    lt_bin_integer,
    lt_dec_integer,
    lt_hex_integer,
    lt_float,
    lt_string,
    lt_bool,
  };

  struct Token {
    Token_Kind kind;
    anton::String7_View value;
    i64 offset;
    i64 line;
    i64 column;
    i64 end_offset;
    i64 end_line;
    i64 end_column;
  };

  [[nodiscard]] anton::Expected<Array<Token>, Error>
  lex_source(Context const& ctx, anton::String_View source_path, anton::String7_View source);
} // namespace vush
