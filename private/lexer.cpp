#include <lexer.hpp>

#include <anton/optional.hpp>

namespace vush {
    using namespace anton::literals;

    Lexed_Source::Lexed_Source(Array<Token> tokens, Array<Token_Source_Info> token_sources)
        : tokens(ANTON_MOV(tokens)), token_sources(ANTON_MOV(token_sources)) {}

    auto Lexed_Source::begin() -> token_iterator {
        return token_iterator{tokens.begin(), token_sources.begin()};
    }

    auto Lexed_Source::end() -> token_iterator {
        return token_iterator{tokens.end(), token_sources.end()};
    }

    auto Lexed_Source::begin() const -> const_token_iterator {
        return const_token_iterator{tokens.begin(), token_sources.begin()};
    }

    auto Lexed_Source::end() const -> const_token_iterator {
        return const_token_iterator{tokens.end(), token_sources.end()};
    }

    auto Lexed_Source::cbegin() const -> const_token_iterator {
        return const_token_iterator{tokens.begin(), token_sources.begin()};
    }

    auto Lexed_Source::cend() const -> const_token_iterator {
        return const_token_iterator{tokens.end(), token_sources.end()};
    }

    constexpr char8 eof_char8 = (char8)EOF;

    [[nodiscard]] static bool is_whitespace(char32 c) {
        return (c <= 32) | (c == 127);
    }

    [[nodiscard]] static bool is_binary_digit(char32 c) {
        return (c == 48) | (c == 49);
    }

    [[nodiscard]] static bool is_hexadecimal_digit(char32 c) {
        return (c >= 48 && c <= 57) | (c >= 65 && c <= 70) | (c >= 97 && c <= 102);
    }

    [[nodiscard]] static bool is_digit(char32 c) {
        return (c >= 48) & (c <= 57);
    }

    [[nodiscard]] static bool is_alpha(char32 c) {
        return (c >= 97 && c < 123) | (c >= 65 && c < 91);
    }

    [[nodiscard]] static bool is_first_identifier_character(char32 c) {
        return (c == '_') | is_alpha(c);
    }

    [[nodiscard]] static bool is_identifier_character(char32 c) {
        return (c == '_') | is_digit(c) | is_alpha(c);
    }

    [[nodiscard]] static bool is_integer_prefix_character(char32 c) {
        return (c == 'b') | (c == 'B') | (c == 'x') | (c == 'X');
    }

    [[nodiscard]] static anton::Optional<Token_Kind> is_keyword(anton::String7_View string) {
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

    [[nodiscard]] static char8 get_lookahead(char8 const* current, char8 const* const end) {
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

    anton::Expected<Lexed_Source, Error> lex_source(Allocator* allocator, anton::String7_View source) {
        Array<Token> tokens(anton::reserve, 4096, allocator);
        Array<Token_Source_Info> token_sources(anton::reserve, 4096, allocator);
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
                tokens.push_back(Token{Token_Kind::whitespace, anton::String7_View{begin, current}});
                token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
            } else if(c == '/' && (la == '/' || la == '*')) {
                // Handle line and block comments.
                // TODO: Report EOF in block comment.
                Source_State const state{current - source_begin, line, column};
                char8 const* const begin = current;
                if(la == U'/') {
                    for(; current != end && *current != '\n'; ++current) {}
                    // The loop stops at the newline. Skip the newline.
                    current += 1;
                    line += 1;
                    column = 1;
                } else {
                    current += 2;
                    column += 2;
                    while(current != end) {
                        char8 const c1 = *current;
                        char8 const c2 = get_lookahead(current, end);
                        if(c1 == '*' && c2 == '/') {
                            break;
                        }
                        // TODO: EOF error.
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

                tokens.push_back(Token{Token_Kind::comment, anton::String7_View{begin, current}});
                token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
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
                        tokens.push_back(Token{keyword.value(), identifier});
                    } else {
                        tokens.push_back(Token{Token_Kind::identifier, identifier});
                    }
                }
                token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
            } else if(is_digit(c) || (c == '.' && is_digit(la))) {
                // Handle integer and float literals. We begin by matching the prefix. If we're unable to find one,
                // then we match the integral part. If a period or exponent follows, we match the fractional part and exponent.
                // Section 4.1.3 and 4.1.4 of The OpenGL Shading Language 4.60.7 state that the leading '-' on integer
                // and floating point literals is always interpreted as a unary minus operator. We follow the same convention.

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

                            if(is_digit(*current)) {
                                // TODO: Error.
                                // TODO: Provide an overload with allocator parameter for String::from_utf32
                                // anton::concat(_allocator, u8"invalid digit '"_sv, anton::String::from_utf32(&next, 4), u8"' in binary integer literal"_sv);
                                return {anton::expected_error, Error{}};
                            }

                            tokens.push_back(Token{Token_Kind::lt_bin_integer, anton::String7_View{begin, current}});
                            token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
                        } break;

                        case 'x':
                        case 'X': {
                            char8 const* const begin = ++current;
                            while(current != end && is_hexadecimal_digit(*current)) {
                                ++current;
                                ++column;
                            }

                            if(is_digit(*current)) {
                                // TODO: Error.
                                // TODO: Provide an overload with allocator parameter for String::from_utf32
                                // anton::concat(_allocator, u8"invalid digit '"_sv, anton::String::from_utf32(&next, 4), u8"' in binary integer literal"_sv);
                                return {anton::expected_error, Error{}};
                            }

                            tokens.push_back(Token{Token_Kind::lt_bin_integer, anton::String7_View{begin, current}});
                            token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
                        } break;

                        default:
                            ANTON_UNREACHABLE();
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
                    // Check whether the integer part is not followed by a period or exponent in which case we've found an integer.
                    // Otherwise we have encountered a float.
                    bool const end_or_not_float = current == end || (*current != '.' && *current != 'e' && *current != 'E');
                    if(end_or_not_float) {
                        tokens.push_back(Token{Token_Kind::lt_dec_integer, integer});
                        token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
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
                        // The grammar requires that either integer, fraction or both contain at least one digit.
                        if(integer.size() == 0 && fraction.size() == 0) {
                            // TODO: Error ("not a float constant").
                            return {anton::expected_error, Error{}};
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
                                // TODO: Error ("exponent has no digits").
                                return {anton::expected_error, Error{}};
                            }
                        }

                        if(!has_e && !has_period) {
                            // TODO: Error ("not a floating point constant").
                            return {anton::expected_error, Error{}};
                        }

                        anton::String7_View const float_literal{float_begin, current};
                        tokens.push_back(Token{Token_Kind::lt_float, float_literal});
                        token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
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
                    // TODO: Error.
                    return {anton::expected_error, Error{}}; // u8"unexpected end of file"_sv
                }

                if(newline) {
                    // TODO: Error.
                    // We disallow newlines inside string literals.
                    return {anton::expected_error, Error{}}; // u8"newlines are not allowed in string literals"_sv
                }

                tokens.push_back(Token{Token_Kind::lt_string, anton::String7_View{begin, current}});
                token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
            } else {
                // Handle tokens.
                Source_State const state{current - source_begin, line, column};
                char8 const* const begin = current;
                Token_Kind token_kind;
                switch(c) {
                    case '{':
                        token_kind = Token_Kind::tk_brace_open;
                        break;
                    case '}':
                        token_kind = Token_Kind::tk_brace_close;
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
                        // TODO: Error.
                        return {anton::expected_error, Error{}};
                }
                ++current;
                ++column;
                tokens.push_back(Token{token_kind, anton::String7_View{begin, current}});
                token_sources.push_back(Token_Source_Info{state.offset, state.line, state.column, current - source_begin, line, column});
            }
        }
        return {anton::expected_value, ANTON_MOV(tokens), ANTON_MOV(token_sources)};
    }
} // namespace vush
