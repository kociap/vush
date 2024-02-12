#include <vush_lexer/lexer.hpp>

#include <anton/optional.hpp>

#include <vush_core/context.hpp>
#include <vush_lexer/diagnostics.hpp>

namespace vush {
  using namespace anton::literals;

  constexpr char8 eof_char8 = (char8)EOF;

  [[nodiscard]] static bool is_whitespace(char32 c)
  {
    return (c <= 32) | (c == 127);
  }

  [[nodiscard]] static bool is_binary_digit(char32 c)
  {
    return (c == 48) | (c == 49);
  }

  [[nodiscard]] static bool is_hexadecimal_digit(char32 c)
  {
    return (c >= 48 && c <= 57) | (c >= 65 && c <= 70) | (c >= 97 && c <= 102);
  }

  [[nodiscard]] static bool is_digit(char32 c)
  {
    return (c >= 48) & (c <= 57);
  }

  [[nodiscard]] static bool is_alpha(char32 c)
  {
    return (c >= 97 && c < 123) | (c >= 65 && c < 91);
  }

  [[nodiscard]] static bool is_first_identifier_character(char32 c)
  {
    return (c == '_') | is_alpha(c);
  }

  [[nodiscard]] static bool is_identifier_character(char32 c)
  {
    return (c == '_') | is_digit(c) | is_alpha(c);
  }

  [[nodiscard]] static bool is_integer_prefix_character(char32 c)
  {
    return (c == 'b') | (c == 'B') | (c == 'x') | (c == 'X');
  }

  [[nodiscard]] static anton::Optional<Token_Kind> is_keyword(anton::String7_View string)
  {
    u64 const h = anton::hash(string);
    switch(h) {
    case anton::hash("if"_sv7):
      return Token_Kind::kw_if;
    case anton::hash("else"_sv7):
      return Token_Kind::kw_else;
    case anton::hash("switch"_sv7):
      return Token_Kind::kw_switch;
    case anton::hash("default"_sv7):
      return Token_Kind::kw_default;
    case anton::hash("for"_sv7):
      return Token_Kind::kw_for;
    case anton::hash("while"_sv7):
      return Token_Kind::kw_while;
    case anton::hash("do"_sv7):
      return Token_Kind::kw_do;
    case anton::hash("return"_sv7):
      return Token_Kind::kw_return;
    case anton::hash("break"_sv7):
      return Token_Kind::kw_break;
    case anton::hash("continue"_sv7):
      return Token_Kind::kw_continue;
    case anton::hash("discard"_sv7):
      return Token_Kind::kw_discard;
    case anton::hash("from"_sv7):
      return Token_Kind::kw_from;
    case anton::hash("struct"_sv7):
      return Token_Kind::kw_struct;
    case anton::hash("import"_sv7):
      return Token_Kind::kw_import;
    case anton::hash("var"_sv7):
      return Token_Kind::kw_var;
    case anton::hash("mut"_sv7):
      return Token_Kind::kw_mut;
    case anton::hash("settings"_sv7):
      return Token_Kind::kw_settings;
    case anton::hash("reinterpret"_sv7):
      return Token_Kind::kw_reinterpret;
    default:
      return anton::null_optional;
    }
  }

  [[nodiscard]] static char8 get_lookahead(char8 const* current, char8 const* const end)
  {
    if(current + 1 != end) {
      return *(current + 1);
    } else {
      return eof_char8;
    }
  }

  struct Source_State {
    i64 offset;
    i64 line;
    i64 column;
  };

  anton::Expected<Array<Token>, Error>
  lex_source(Context const& ctx, anton::String_View const source_path, anton::String7_View source)
  {
    Array<Token> tokens(ctx.allocator, anton::reserve, 4096);
    char8 const* const source_begin = source.begin();
    char8 const* current = source.begin();
    char8 const* const end = source.end();
    i64 line = 1;
    i64 column = 1;
    while(current != end) {
      char8 const c = *current;
      char8 const la = get_lookahead(current, end);
      if(is_whitespace(c)) {
        Source_State const state{current - source_begin, line, column};
        // Handle whitespace.
        char8 const* const begin = current;
        do {
          if(*current == '\n') {
            line += 1;
            column = 1;
          } else {
            ++column;
          }
          ++current;
        } while(current != end && is_whitespace(*current));
        tokens.push_back(Token{Token_Kind::whitespace, anton::String7_View{begin, current},
                               state.offset, state.line, state.column, current - source_begin, line,
                               column});
      } else if(c == '/' && (la == '/' || la == '*')) {
        // Handle line and block comments.
        Source_State const state{current - source_begin, line, column};
        char8 const* const begin = current;
        if(la == U'/') {
          for(; current != end && *current != '\n'; ++current) {}
          // The loop stops at the newline or the eof. Skip the newline.
          if(current != end) {
            current += 1;
          }
          line += 1;
          column = 1;
        } else {
          current += 2;
          column += 2;
          while(true) {
            if(current == end) {
              return {
                anton::expected_error,
                err_lexer_unexpected_eof(ctx, source_path, current - source_begin, line, column)};
            }

            char8 const c1 = *current;
            char8 const c2 = get_lookahead(current, end);
            if(c1 == '*' && c2 == '/') {
              break;
            }

            if(*current == '\n') {
              line += 1;
              column = 1;
            } else {
              ++column;
            }
            ++current;
          }

          column += 2;
          current += 2;
        }

        tokens.push_back(Token{Token_Kind::comment, anton::String7_View{begin, current},
                               state.offset, state.line, state.column, current - source_begin, line,
                               column});
      } else if(is_first_identifier_character(c)) {
        // Handle identifier.
        Source_State const state{current - source_begin, line, column};
        char8 const* const begin = current;
        while(current != end && is_identifier_character(*current)) {
          ++current;
        }

        anton::String7_View const identifier{begin, current};
        column += current - begin;

        // Handle bool literals separately.
        if(identifier == "true"_sv7 || identifier == "false"_sv7) {
          tokens.push_back(Token{Token_Kind::lt_bool, identifier});
        } else {
          anton::Optional<Token_Kind> keyword = is_keyword(identifier);
          if(keyword) {
            tokens.push_back(Token{keyword.value(), identifier, state.offset, state.line,
                                   state.column, current - source_begin, line, column});
          } else {
            tokens.push_back(Token{Token_Kind::identifier, identifier, state.offset, state.line,
                                   state.column, current - source_begin, line, column});
          }
        }
      } else if(is_digit(c) || (c == '.' && is_digit(la))) {
        // Handle integer and float literals. We begin by matching the prefix. If we're unable to
        // find one, then we match the integral part. If a period or exponent follows, we match the
        // fractional part and exponent.
        //
        // The plus and minus signs are not a part of the literals.

        Source_State const state{current - source_begin, line, column};
        if(c == '0' && is_integer_prefix_character(la)) {
          // We're matching a prefixed integer literal.
          switch(la) {
          case 'b':
          case 'B': {
            char8 const* const begin = ++current;
            while(current != end && is_binary_digit(*current)) {
              ++current;
              ++column;
            }

            // We have to verify that no digits follow a binary literal because otherwise those
            // would be tokenised as a separate integer literal.
            if(is_digit(*current)) {
              return {anton::expected_error,
                      err_lexer_invalid_digit_in_binary_literal(
                        ctx, source_path, current - source_begin, line, column)};
            }

            tokens.push_back(Token{Token_Kind::lt_bin_integer, anton::String7_View{begin, current},
                                   state.offset, state.line, state.column, current - source_begin,
                                   line, column});
          } break;

          case 'x':
          case 'X': {
            char8 const* const begin = ++current;
            while(current != end && is_hexadecimal_digit(*current)) {
              ++current;
              ++column;
            }

            // We do not do any verification here of what follows
            // a hexadecimal literal because it might be a suffix.

            tokens.push_back(Token{Token_Kind::lt_hex_integer, anton::String7_View{begin, current},
                                   state.offset, state.line, state.column, current - source_begin,
                                   line, column});
          } break;

          default:
            ANTON_UNREACHABLE("unreachable");
          }
        } else {
          // We haven't found a prefixed integer. Match integer or float.
          char8 const* const integer_begin = current;
          char8 const* const float_begin = current;
          while(current != end && is_digit(*current)) {
            ++current;
            ++column;
          }
          anton::String7_View const integer{integer_begin, current};
          // Check whether the integer part is not followed by a period or exponent in which case
          // we've found an integer. Otherwise we have encountered a float.
          bool const end_or_not_float =
            current == end || (*current != '.' && *current != 'e' && *current != 'E');
          if(end_or_not_float) {
            tokens.push_back(Token{Token_Kind::lt_dec_integer, integer, state.offset, state.line,
                                   state.column, current - source_begin, line, column});
          } else {
            // Match float literal.
            bool has_period = false;
            char8 const* const fraction_begin = current;
            if(current != end && *current == '.') {
              has_period = true;
              ++current;
              ++column;
              while(current != end && is_digit(*current)) {
                ++current;
                ++column;
              }
            }
            anton::String7_View const fraction{fraction_begin, current};
            // The grammar requires that integer, fraction or both contain at least one digit.
            if(integer.size() == 0 && fraction.size() == 0) {
              return {anton::expected_error,
                      err_lexer_not_fp_constant(ctx, source_path, float_begin - source_begin,
                                                current - source_begin, line, column)};
            }

            bool has_e = false;
            if(current != end && (*current == 'e' || *current == 'E')) {
              has_e = true;
              ++current;
              ++column;
              if(current != end && (*current == '-' || *current == U'+')) {
                ++current;
                ++column;
              }

              char8 const* const exponent = current;
              while(current != end && is_digit(*current)) {
                ++current;
                ++column;
              }

              if(current - exponent == 0) {
                return {anton::expected_error,
                        err_lexer_missing_exponent_digits(ctx, source_path, current - source_begin,
                                                          line, column)};
              }
            }

            if(!has_e && !has_period) {
              return {anton::expected_error,
                      err_lexer_not_fp_constant(ctx, source_path, float_begin - source_begin,
                                                current - source_begin, line, column)};
            }

            anton::String7_View const float_literal{float_begin, current};
            tokens.push_back(Token{Token_Kind::lt_float, float_literal, state.offset, state.line,
                                   state.column, current - source_begin, line, column});
          }
        }
      } else if(c == '\"') {
        // Handle string literals.
        Source_State const state{current - source_begin, line, column};
        char8 const* const begin = current;
        ++current;
        ++column;
        // Whether the string contains a newline.
        bool newline = false;
        // Whether we have found a closing '"'.
        bool complete = false;
        // Transient state to detect escaped characters.
        bool escaped = false;
        while(current != end) {
          newline |= *current == '\n';
          if(*current == U'\"' && !escaped) {
            complete = true;
            break;
          }
          escaped = *current == '\\';
          ++current;
          ++column;
        }
        ++current;
        ++column;

        if(!complete) {
          return {anton::expected_error,
                  err_lexer_unexpected_eof(ctx, source_path, current - source_begin, line, column)};
        }

        if(newline) {
          // We disallow newlines inside string literals.
          return {anton::expected_error, err_lexer_newline_in_string_literal(
                                           ctx, source_path, current - source_begin, line, column)};
        }

        tokens.push_back(Token{Token_Kind::lt_string, anton::String7_View{begin, current},
                               state.offset, state.line, state.column, current - source_begin, line,
                               column});
      } else {
        // Handle tokens.
        Source_State const state{current - source_begin, line, column};
        char8 const* const begin = current;
        Token_Kind token_kind;
        switch(c) {
        case '{':
          token_kind = Token_Kind::tk_lbrace;
          break;
        case '}':
          token_kind = Token_Kind::tk_rbrace;
          break;
        case '[':
          token_kind = Token_Kind::tk_lbracket;
          break;
        case ']':
          token_kind = Token_Kind::tk_rbracket;
          break;
        case '(':
          token_kind = Token_Kind::tk_lparen;
          break;
        case ')':
          token_kind = Token_Kind::tk_rparen;
          break;
        case '<':
          token_kind = Token_Kind::tk_langle;
          break;
        case '>':
          token_kind = Token_Kind::tk_rangle;
          break;
        case ';':
          token_kind = Token_Kind::tk_semicolon;
          break;
        case ':':
          token_kind = Token_Kind::tk_colon;
          break;
        case ',':
          token_kind = Token_Kind::tk_comma;
          break;
        case '.':
          token_kind = Token_Kind::tk_dot;
          break;
        case '\"':
          token_kind = Token_Kind::tk_double_quote;
          break;
        case '@':
          token_kind = Token_Kind::tk_at;
          break;
        case '+':
          token_kind = Token_Kind::tk_plus;
          break;
        case '-':
          token_kind = Token_Kind::tk_minus;
          break;
        case '*':
          token_kind = Token_Kind::tk_asterisk;
          break;
        case '/':
          token_kind = Token_Kind::tk_slash;
          break;
        case '%':
          token_kind = Token_Kind::tk_percent;
          break;
        case '&':
          token_kind = Token_Kind::tk_amp;
          break;
        case '|':
          token_kind = Token_Kind::tk_pipe;
          break;
        case '^':
          token_kind = Token_Kind::tk_hat;
          break;
        case '!':
          token_kind = Token_Kind::tk_bang;
          break;
        case '~':
          token_kind = Token_Kind::tk_tilde;
          break;
        case '=':
          token_kind = Token_Kind::tk_equals;
          break;
        default:
          return {anton::expected_error, err_lexer_unrecognised_token(
                                           ctx, source_path, current - source_begin, line, column)};
        }
        ++current;
        ++column;
        tokens.push_back(Token{token_kind, anton::String7_View{begin, current}, state.offset,
                               state.line, state.column, current - source_begin, line, column});
      }
    }
    return {anton::expected_value, ANTON_MOV(tokens)};
  }
} // namespace vush
