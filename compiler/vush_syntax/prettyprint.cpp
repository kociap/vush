#include <vush_syntax/syntax.hpp>

namespace vush {
  using namespace anton::literals;
  namespace {
    struct Printer {
    private:
      anton::Output_Stream* stream;
      i32 indent_width = 0;
      i32 current_indent = 0;

    public:
      Printer(anton::Output_Stream& stream, i32 indent_width)
        : stream(&stream), indent_width(indent_width)
      {
      }

      void write(anton::String_View const string)
      {
        stream->write(string);
      }

      void indent()
      {
        for(i32 i = 0; i < current_indent; i += 1) {
          stream->write(" "_sv);
        }
      }

      void inc_indent()
      {
        current_indent += indent_width;
      }

      void dec_indent()
      {
        current_indent =
          anton::math::max(current_indent - indent_width, (i32)0);
      }

      void set_indent_level(i32 const level)
      {
        current_indent = anton::math::max(level * indent_width, (i32)0);
      }
    };
  } // namespace

  [[nodiscard]] static anton::String_View stringify(SNOT_Kind const kind)
  {
    switch(kind) {
    case SNOT_Kind::identifier:
      return "identifier"_sv;
    case SNOT_Kind::comment:
      return "comment"_sv;
    case SNOT_Kind::whitespace:
      return "whitespace"_sv;
    case SNOT_Kind::kw_if:
      return "kw_if"_sv;
    case SNOT_Kind::kw_else:
      return "kw_else"_sv;
    case SNOT_Kind::kw_switch:
      return "kw_switch"_sv;
    case SNOT_Kind::kw_default:
      return "kw_default"_sv;
    case SNOT_Kind::kw_for:
      return "kw_for"_sv;
    case SNOT_Kind::kw_while:
      return "kw_while"_sv;
    case SNOT_Kind::kw_do:
      return "kw_do"_sv;
    case SNOT_Kind::kw_return:
      return "kw_return"_sv;
    case SNOT_Kind::kw_break:
      return "kw_break"_sv;
    case SNOT_Kind::kw_continue:
      return "kw_continue"_sv;
    case SNOT_Kind::kw_discard:
      return "kw_discard"_sv;
    case SNOT_Kind::kw_from:
      return "kw_from"_sv;
    case SNOT_Kind::kw_struct:
      return "kw_struct"_sv;
    case SNOT_Kind::kw_import:
      return "kw_import"_sv;
    case SNOT_Kind::kw_var:
      return "kw_var"_sv;
    case SNOT_Kind::kw_mut:
      return "kw_mut"_sv;
    case SNOT_Kind::kw_settings:
      return "kw_settings"_sv;
    case SNOT_Kind::kw_reinterpret:
      return "kw_reinterpret"_sv;
    case SNOT_Kind::kw_buffer:
      return "kw_buffer"_sv;
    case SNOT_Kind::kw_fn:
      return "kw_fn"_sv;
    case SNOT_Kind::tk_lbrace:
      return "tk_lbrace"_sv;
    case SNOT_Kind::tk_rbrace:
      return "tk_rbrace"_sv;
    case SNOT_Kind::tk_lbracket:
      return "tk_lbracket"_sv;
    case SNOT_Kind::tk_rbracket:
      return "tk_rbracket"_sv;
    case SNOT_Kind::tk_lparen:
      return "tk_lparen"_sv;
    case SNOT_Kind::tk_rparen:
      return "tk_rparen"_sv;
    case SNOT_Kind::tk_langle:
      return "tk_langle"_sv;
    case SNOT_Kind::tk_rangle:
      return "tk_rangle"_sv;
    case SNOT_Kind::tk_semicolon:
      return "tk_semicolon"_sv;
    case SNOT_Kind::tk_colon:
      return "tk_colon"_sv;
    case SNOT_Kind::tk_comma:
      return "tk_comma"_sv;
    case SNOT_Kind::tk_dot:
      return "tk_dot"_sv;
    case SNOT_Kind::tk_double_quote:
      return "tk_double_quote"_sv;
    case SNOT_Kind::tk_at:
      return "tk_at"_sv;
    case SNOT_Kind::tk_plus:
      return "tk_plus"_sv;
    case SNOT_Kind::tk_minus:
      return "tk_minus"_sv;
    case SNOT_Kind::tk_asterisk:
      return "tk_asterisk"_sv;
    case SNOT_Kind::tk_slash:
      return "tk_slash"_sv;
    case SNOT_Kind::tk_percent:
      return "tk_percent"_sv;
    case SNOT_Kind::tk_amp:
      return "tk_amp"_sv;
    case SNOT_Kind::tk_pipe:
      return "tk_pipe"_sv;
    case SNOT_Kind::tk_hat:
      return "tk_hat"_sv;
    case SNOT_Kind::tk_bang:
      return "tk_bang"_sv;
    case SNOT_Kind::tk_tilde:
      return "tk_tilde"_sv;
    case SNOT_Kind::tk_equals:
      return "tk_equals"_sv;
    case SNOT_Kind::lt_bin_integer:
      return "lt_bin_integer"_sv;
    case SNOT_Kind::lt_dec_integer:
      return "lt_dec_integer"_sv;
    case SNOT_Kind::lt_hex_integer:
      return "lt_hex_integer"_sv;
    case SNOT_Kind::lt_float:
      return "lt_float"_sv;
    case SNOT_Kind::lt_string:
      return "lt_string"_sv;
    case SNOT_Kind::lt_bool:
      return "lt_bool"_sv;
    case SNOT_Kind::tk_amp2:
      return "tk_amp2"_sv;
    case SNOT_Kind::tk_pipe2:
      return "tk_pipe2"_sv;
    case SNOT_Kind::tk_hat2:
      return "tk_hat2"_sv;
    case SNOT_Kind::tk_shl:
      return "tk_shl"_sv;
    case SNOT_Kind::tk_shr:
      return "tk_shr"_sv;
    case SNOT_Kind::tk_eq2:
      return "tk_eq2"_sv;
    case SNOT_Kind::tk_neq:
      return "tk_neq"_sv;
    case SNOT_Kind::tk_lteq:
      return "tk_lteq"_sv;
    case SNOT_Kind::tk_gteq:
      return "tk_gteq"_sv;
    case SNOT_Kind::tk_pluseq:
      return "tk_pluseq"_sv;
    case SNOT_Kind::tk_minuseq:
      return "tk_minuseq"_sv;
    case SNOT_Kind::tk_asteriskeq:
      return "tk_asteriskeq"_sv;
    case SNOT_Kind::tk_slasheq:
      return "tk_slasheq"_sv;
    case SNOT_Kind::tk_percenteq:
      return "tk_percenteq"_sv;
    case SNOT_Kind::tk_ampeq:
      return "tk_ampeq"_sv;
    case SNOT_Kind::tk_pipeeq:
      return "tk_pipeeq"_sv;
    case SNOT_Kind::tk_hateq:
      return "tk_hateq"_sv;
    case SNOT_Kind::tk_shleq:
      return "tk_shleq"_sv;
    case SNOT_Kind::tk_shreq:
      return "tk_shreq"_sv;
    case SNOT_Kind::tk_thick_arrow:
      return "tk_thick_arrow"_sv;
    case SNOT_Kind::tk_thin_arrow:
      return "tk_thin_arrow"_sv;
    case SNOT_Kind::tk_colon2:
      return "tk_colon2"_sv;
    case SNOT_Kind::tk_setting_string:
      return "tk_setting_string"_sv;
    case SNOT_Kind::type_named:
      return "type_named"_sv;
    case SNOT_Kind::type_array:
      return "type_array"_sv;
    case SNOT_Kind::type_array_base:
      return "type_array_base"_sv;
    case SNOT_Kind::type_array_size:
      return "type_array_size"_sv;
    case SNOT_Kind::variable:
      return "variable"_sv;
    case SNOT_Kind::struct_field:
      return "struct_field"_sv;
    case SNOT_Kind::struct_field_block:
      return "struct_field_block"_sv;
    case SNOT_Kind::buffer_field:
      return "buffer_field"_sv;
    case SNOT_Kind::buffer_field_block:
      return "buffer_field_block"_sv;
    case SNOT_Kind::fn_parameter_if:
      return "fn_parameter_if"_sv;
    case SNOT_Kind::fn_parameter:
      return "fn_parameter"_sv;
    case SNOT_Kind::fn_parameter_list:
      return "fn_parameter_list"_sv;
    case SNOT_Kind::attribute:
      return "attribute"_sv;
    case SNOT_Kind::attribute_list:
      return "attribute_list"_sv;
    case SNOT_Kind::attribute_parameter_keyed:
      return "attribute_parameter_keyed"_sv;
    case SNOT_Kind::attribute_parameter_positional:
      return "attribute_parameter_positional"_sv;
    case SNOT_Kind::attribute_parameter_list:
      return "attribute_parameter_list"_sv;
    case SNOT_Kind::decl_block:
      return "decl_block"_sv;
    case SNOT_Kind::decl_if:
      return "decl_if"_sv;
    case SNOT_Kind::decl_import:
      return "decl_import"_sv;
    case SNOT_Kind::decl_struct:
      return "decl_struct"_sv;
    case SNOT_Kind::decl_buffer:
      return "decl_buffer"_sv;
    case SNOT_Kind::decl_settings:
      return "decl_settings"_sv;
    case SNOT_Kind::decl_function:
      return "decl_function"_sv;
    case SNOT_Kind::decl_stage_function:
      return "decl_stage_function"_sv;
    case SNOT_Kind::setting_block:
      return "setting_block"_sv;
    case SNOT_Kind::setting_keyval:
      return "setting_keyval"_sv;
    case SNOT_Kind::field_initializer:
      return "field_initializer"_sv;
    case SNOT_Kind::index_initializer:
      return "index_initializer"_sv;
    case SNOT_Kind::basic_initializer:
      return "basic_initializer"_sv;
    case SNOT_Kind::init_initializer_list:
      return "init_initializer_list"_sv;
    case SNOT_Kind::call_arg_list:
      return "call_arg_list"_sv;
    case SNOT_Kind::expr_block:
      return "expr_block"_sv;
    case SNOT_Kind::expr_if:
      return "expr_if"_sv;
    case SNOT_Kind::expr_identifier:
      return "expr_identifier"_sv;
    case SNOT_Kind::expr_binary:
      return "expr_binary"_sv;
    case SNOT_Kind::expr_prefix:
      return "expr_prefix"_sv;
    case SNOT_Kind::expr_field:
      return "expr_field"_sv;
    case SNOT_Kind::expr_index:
      return "expr_index"_sv;
    case SNOT_Kind::expr_parentheses:
      return "expr_parentheses"_sv;
    case SNOT_Kind::expr_reinterpret:
      return "expr_reinterpret"_sv;
    case SNOT_Kind::expr_init:
      return "expr_init"_sv;
    case SNOT_Kind::expr_call:
      return "expr_call"_sv;
    case SNOT_Kind::expr_lt_bool:
      return "expr_lt_bool"_sv;
    case SNOT_Kind::expr_lt_integer:
      return "expr_lt_integer"_sv;
    case SNOT_Kind::expr_lt_float:
      return "expr_lt_float"_sv;
    case SNOT_Kind::expr_lt_string:
      return "expr_lt_string"_sv;
    case SNOT_Kind::expr_default:
      return "expr_default"_sv;
    case SNOT_Kind::stmt_block:
      return "stmt_block"_sv;
    case SNOT_Kind::stmt_if:
      return "stmt_if"_sv;
    case SNOT_Kind::stmt_switch:
      return "stmt_switch"_sv;
    case SNOT_Kind::stmt_for:
      return "stmt_for"_sv;
    case SNOT_Kind::stmt_while:
      return "stmt_while"_sv;
    case SNOT_Kind::stmt_do_while:
      return "stmt_do_while"_sv;
    case SNOT_Kind::stmt_return:
      return "stmt_return"_sv;
    case SNOT_Kind::stmt_break:
      return "stmt_break"_sv;
    case SNOT_Kind::stmt_continue:
      return "stmt_continue"_sv;
    case SNOT_Kind::stmt_discard:
      return "stmt_discard"_sv;
    case SNOT_Kind::stmt_expression:
      return "stmt_expression"_sv;
    case SNOT_Kind::stmt_assignment:
      return "stmt_assignment"_sv;
    case SNOT_Kind::stmt_empty:
      return "stmt_empty"_sv;
    case SNOT_Kind::switch_arm_list:
      return "switch_arm_list"_sv;
    case SNOT_Kind::switch_arm:
      return "switch_arm"_sv;
    case SNOT_Kind::switch_arm_label:
      return "switch_arm_label"_sv;
    case SNOT_Kind::return_expression:
      return "return_expression"_sv;
    case SNOT_Kind::for_variable:
      return "for_variable"_sv;
    case SNOT_Kind::for_condition:
      return "for_condition"_sv;
    case SNOT_Kind::for_expression:
      return "for_expression"_sv;
    }
  }

  static void print_node(Allocator* allocator, Printer& printer,
                         Syntax_Prettyprint_Options const& options,
                         SNOT const* node)
  {
    printer.indent();
    printer.write("[");
    printer.write(stringify(node->kind));
    if(options.print_source || options.print_location) {
      printer.write(" @");
      if(options.print_location) {
        printer.write(" ");
        printer.write(anton::to_string(allocator, node->source_info.line));
        printer.write(":");
        printer.write(anton::to_string(allocator, node->source_info.column));
      }

      if(options.print_source) {
        printer.write(" ");
        printer.write(node->source_info.source->path);
      }
    }
    printer.write("]\n");
  }

  static void print_list(Allocator* allocator, Printer& printer,
                         Syntax_Prettyprint_Options const& options,
                         SNOT const* snots)
  {
    for(; snots != nullptr; snots = anton::ilist_next(snots)) {
      print_node(allocator, printer, options, snots);
      printer.inc_indent();
      print_list(allocator, printer, options, snots->children);
      printer.dec_indent();
    }
  }

  void prettyprint_syntax(Allocator* allocator, anton::Output_Stream& stream,
                          Syntax_Prettyprint_Options const& options,
                          SNOT const* snots)
  {
    Printer printer(stream, options.indent_width);
    print_list(allocator, printer, options, snots);
  }
} // namespace vush
