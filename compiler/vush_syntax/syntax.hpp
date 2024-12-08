#pragma once

#include <anton/ilist.hpp>
#include <anton/string.hpp>

#include <vush_core/source_info.hpp>
#include <vush_core/types.hpp>

namespace vush {
  // Syntax Node Or Token Kind (SNOT_Kind)
  //
  // Overlaps with lexer's Token_Kind.
  //
  enum struct SNOT_Kind {
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
    kw_buffer,
    kw_fn,
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

    // compound tokens
    tk_amp2, // &&
    tk_pipe2, // ||
    tk_hat2, // ^^
    tk_shl, // <<
    tk_shr, // >>
    tk_eq2, // ==
    tk_neq, // !=
    tk_lteq, // <=
    tk_gteq, // >=
    tk_pluseq, // +=
    tk_minuseq, // -=
    tk_asteriskeq, // *=
    tk_slasheq, // /=
    tk_percenteq, // %=
    tk_ampeq, // &=
    tk_pipeeq, // |=
    tk_hateq, // ^=
    tk_shleq, // <<=
    tk_shreq, // >>=
    tk_thick_arrow, // =>
    tk_thin_arrow, // ->
    tk_colon2, // ::

    // tk_setting_string
    //
    // A special type of string only used by setting key and values. This token contains anything
    // other than comments, whitespace, colons, left braces or right braces.
    //
    tk_setting_string,

    type_named,
    type_array,

    // The base type of array.
    type_array_base,
    type_array_size,

    variable,

    struct_field,
    struct_field_block,

    buffer_field,
    buffer_field_block,

    fn_parameter_if,
    fn_parameter,
    fn_parameter_list,

    attribute,
    attribute_list,
    attribute_parameter_keyed,
    attribute_parameter_positional,
    attribute_parameter_list,

    decl_block,
    decl_if,
    decl_import,
    decl_struct,
    decl_buffer,
    decl_settings,
    decl_function,
    decl_stage_function,

    setting_block,
    setting_keyval,

    field_initializer,
    index_initializer,
    basic_initializer,

    init_initializer_list,
    call_arg_list,

    expr_block,
    expr_if,
    expr_identifier,
    expr_binary,
    expr_prefix,
    expr_field,
    expr_index,
    expr_parentheses,
    expr_reinterpret,
    expr_init,
    expr_call,
    expr_lt_bool,
    expr_lt_integer,
    expr_lt_float,
    expr_lt_string,
    // expr_default
    //
    // The 'default' label in stmt_case.
    //
    expr_default,

    stmt_block,
    stmt_if,
    stmt_switch,
    stmt_for,
    stmt_while,
    stmt_do_while,
    stmt_return,
    stmt_break,
    stmt_continue,
    stmt_discard,
    stmt_expression,
    stmt_assignment,
    stmt_empty,

    switch_arm_list,
    switch_arm,
    switch_arm_label,
    return_expression,
    for_variable,
    for_condition,
    for_expression,
  };

  // Syntax Node Or Token (SNOT)
  //
  struct SNOT: public anton::IList_DNode {
    SNOT_Kind kind;
    Source_Info source_info;
    union {
      char8 const* source;
      SNOT* children;
    };

    SNOT(SNOT_Kind kind, Source_Info source_info, char8 const* source)
      : kind(kind), source_info(source_info), source(source)
    {
    }

    SNOT(SNOT_Kind kind, Source_Info source_info, SNOT* children)
      : kind(kind), source_info(source_info), children(children)
    {
    }

    [[nodiscard]] bool is_token() const
    {
      return static_cast<u32>(kind) <=
             static_cast<u32>(SNOT_Kind::tk_setting_string);
    }

    [[nodiscard]] bool is_node() const
    {
      return !is_token();
    }

    [[nodiscard]] anton::String_View get_value() const
    {
      return {source + source_info.offset, source + source_info.end_offset};
    }
  };
} // namespace vush
