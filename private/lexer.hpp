#pragma once

#include <anton/expected.hpp>
#include <anton/iterators/zip.hpp>
#include <anton/string7_view.hpp>
#include <diagnostics.hpp>
#include <vush/types.hpp>

namespace vush {
    enum struct Token_Type {
        identifier,
        comment,
        whitespace,
        // keywords
        kw_if,
        kw_else,
        kw_switch,
        kw_case,
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
        kw_const,
        kw_settings,
        kw_reinterpret,
        kw_invariant,
        kw_smooth,
        kw_flat,
        kw_noperspective,
        // separators
        tk_brace_open,
        tk_brace_close,
        tk_bracket_open,
        tk_bracket_close,
        tk_paren_open,
        tk_paren_close,
        tk_angle_open,
        tk_angle_close,
        tk_semicolon,
        tk_colon,
        tk_comma,
        tk_dot,
        tk_double_quote,
        tk_plus,
        tk_minus,
        tk_asterisk,
        tk_slash,
        tk_percent,
        tk_ampresand,
        tk_pipe,
        tk_hat,
        tk_exclamation,
        tk_tilde,
        tk_equals,
        // literals
        lt_bin_integer,
        lt_oct_integer,
        lt_dec_integer,
        lt_hex_integer,
        lt_float,
        lt_string,
        lt_bool,
    };

    struct Token {
        Token_Type type;
        anton::String7_View value;
    };

    struct Token_Source_Info {
        i64 offset;
        i64 line;
        i64 column;
        i64 end_offset;
        i64 end_line;
        i64 end_column;
    };

    struct Lexed_Source {
    public:
        Array<Token> tokens;
        Array<Token_Source_Info> token_sources;

    public:
        Lexed_Source(Array<Token> tokens, Array<Token_Source_Info> token_sources);

        using token_iterator = anton::Zip_Iterator<Token*, Token_Source_Info*>;
        using const_token_iterator = anton::Zip_Iterator<Token const*, Token_Source_Info const*>;

        token_iterator begin();
        token_iterator end();
        const_token_iterator begin() const;
        const_token_iterator end() const;
        const_token_iterator cbegin() const;
        const_token_iterator cend() const;
    };

    [[nodiscard]] anton::Expected<Lexed_Source, Error> lex_source(Allocator* allocator, anton::String7_View source);
} // namespace vush
