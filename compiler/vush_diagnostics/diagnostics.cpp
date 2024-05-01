#include <vush_diagnostics/diagnostics.hpp>

#include <anton/format.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/context.hpp>
#include <vush_diagnostics/utility.hpp>

namespace vush {
  using namespace anton::literals;

  anton::String format_undefined_symbol(Context const& ctx,
                                        Source_Info const& symbol)
  {
    anton::String_View const source =
      ctx.source_registry->find_source(symbol.source_path)->data;
    anton::String message = format_diagnostic_location(ctx.allocator, symbol);
    message += u8"error: undefined symbol '"_sv;
    message += get_source_bit(source, symbol);
    message += u8"'\n"_sv;
    if(ctx.diagnostics.extended) {
      print_source_snippet(ctx, message, source, symbol);
      message += '\n';
    }
    return message;
  }

  // anton::String format_called_symbol_does_not_name_function(Context const& ctx, Source_Info const& symbol) {
  //     anton::String_View const source = ctx.source_registry->find_source(symbol.source_path)->data;
  //     anton::String message = format_diagnostic_location(ctx.allocator, symbol);
  //     message += u8"error: called symbol '"_sv;
  //     message += get_source_bit(source, symbol);
  //     message += u8"' does not name a function\n"_sv;
  //     if(ctx.diagnostics.extended) {
  //         print_source_snippet(ctx, message, source, symbol);
  //         message += '\n';
  //     }
  //     return message;
  // }

  Error err_invalid_integer_suffix(Context const& ctx,
                                   Source_Info const& suffix)
  {
    Error error = error_from_source(ctx.allocator, suffix);
    anton::String_View const source =
      ctx.source_registry->find_source(suffix.source_path)->data;
    error.diagnostic =
      anton::format(ctx.allocator, "error: invalid integer suffix '{}'"_sv,
                    get_source_bit(source, suffix));
    print_source_snippet(ctx, error.extended_diagnostic, source, suffix);
    error.extended_diagnostic += " valid suffixes are 'u' and 'U'"_sv;
    return error;
  }

  Error err_invalid_float_suffix(Context const& ctx, Source_Info const& suffix)
  {
    Error error = error_from_source(ctx.allocator, suffix);
    anton::String_View const source =
      ctx.source_registry->find_source(suffix.source_path)->data;
    error.diagnostic =
      anton::format(ctx.allocator, "error: invalid float suffix '{}'"_sv,
                    get_source_bit(source, suffix));
    print_source_snippet(ctx, error.extended_diagnostic, source, suffix);
    error.extended_diagnostic += " valid suffixes are 'd' and 'D'"_sv;
    return error;
  }

  anton::String format_integer_literal_overflow(Context const& ctx,
                                                Source_Info const& integer)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, integer);
    message += u8"error: integer literal requires more than 32 bits\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const source =
        ctx.source_registry->find_source(integer.source_path)->data;
      print_source_snippet(ctx, message, source, integer);
      message += '\n';
    }
    return message;
  }

  anton::String format_integer_literal_leading_zeros(Context const& ctx,
                                                     Source_Info const& integer)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, integer);
    message +=
      u8"error: leading zeros in decimal integer literals are not allowed\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const source =
        ctx.source_registry->find_source(integer.source_path)->data;
      print_source_snippet(ctx, message, source, integer);
      message += '\n';
    }
    return message;
  }

  Error err_overload_on_return_type(Context const& ctx,
                                    Source_Info const& identifier1,
                                    Source_Info const& return1,
                                    Source_Info const& identifier2,
                                    Source_Info const& return2)
  {
    // TODO: Separate error function for user-builtin functions.
    Error error = error_from_source(ctx.allocator, identifier2);
    anton::String_View const source1 =
      ctx.source_registry->find_source(identifier1.source_path)->data;
    error.diagnostic = anton::String(
      "error: functions may not be overloaded on their return type alone"_sv,
      ctx.allocator);
    error.extended_diagnostic =
      format_diagnostic_location(ctx.allocator, identifier1);
    error.extended_diagnostic += '\n';
    print_source_snippet(ctx, error.extended_diagnostic, source1, identifier1);
    error.extended_diagnostic += anton::format(
      ctx.allocator, u8"overload with return type '{}' defined here\n"_sv,
      get_source_bit(source1, return1));
    anton::String_View const source2 =
      ctx.source_registry->find_source(identifier2.source_path)->data;
    error.extended_diagnostic +=
      format_diagnostic_location(ctx.allocator, identifier2);
    error.extended_diagnostic += '\n';
    print_source_snippet(ctx, error.extended_diagnostic, source2, identifier2);
    error.extended_diagnostic += anton::format(
      ctx.allocator,
      u8"overload with a different return type '{}', but identical parameters defined here\n"_sv,
      get_source_bit(source2, return2));
    return error;
  }

  Error err_immutable_variable_missing_initializer(Context const& ctx,
                                                   Source_Info const& constant)
  {
    Error error = error_from_source(ctx.allocator, constant);
    anton::String_View const source =
      ctx.source_registry->find_source(constant.source_path)->data;
    error.diagnostic = "error: immutable variable is missing an initializer"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, constant);
    error.extended_diagnostic +=
      " an immutable variable must be initialized"_sv;
    return error;
  }

  anton::String
  format_variable_declaration_in_global_scope(Context const& ctx,
                                              Source_Info const& declaration)
  {
    anton::String message =
      format_diagnostic_location(ctx.allocator, declaration);
    message +=
      u8"error: illegal declaration of a variable in global scope\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const& source =
        ctx.source_registry->find_source(declaration.source_path)->data;
      print_source_snippet(ctx, message, source, declaration);
      message += '\n';
    }
    return message;
  }

  // Error err_immutable_variable_missing_initializer(Context const& ctx, Source_Info const& constant) {
  //     anton::String message = format_diagnostic_location(ctx.allocator, constant);
  //     message += u8"error: missing constant initializer\n"_sv;
  //     if(ctx.diagnostics.extended) {
  //         anton::String_View const source = ctx.source_registry->find_source(constant.source_path)->data;
  //         print_source_snippet(ctx, message, source, constant);
  //         message += '\n';
  //     }
  //     return message;
  // }

  anton::String format_expression_not_implicitly_convertible_to_bool(
    Context const& ctx, Source_Info const& expression)
  {
    anton::String message =
      format_diagnostic_location(ctx.allocator, expression);
    message += u8"error: expression is not implicitly convertible to bool\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const source =
        ctx.source_registry->find_source(expression.source_path)->data;
      print_source_snippet(ctx, message, source, expression);
      message += '\n';
    }
    return message;
  }

  anton::String format_illegal_image_layout_qualifier_on_non_sourced_parameter(
    Context const& ctx, Source_Info const& qualifier,
    Source_Info const& parameter_identifier)
  {
    anton::String message =
      format_diagnostic_location(ctx.allocator, qualifier);
    anton::String_View const source =
      ctx.source_registry->find_source(qualifier.source_path)->data;
    message += u8"illegal image layout qualifier '"_sv;
    message += get_source_bit(source, qualifier);
    message += u8"' on non-sourced parameter '"_sv;
    message += get_source_bit(source, parameter_identifier);
    message += u8"'\n"_sv;
    if(ctx.diagnostics.extended) {
      // TODO: Underline the qualifier and the parameter?
      print_source_snippet(ctx, message, source, qualifier);
      message += '\n';
    }
    return message;
  }

  anton::String format_illegal_image_layout_qualifier_on_non_image_type(
    Context const& ctx, Source_Info const& qualifier, Source_Info const& type)
  {
    anton::String message =
      format_diagnostic_location(ctx.allocator, qualifier);
    anton::String_View const source =
      ctx.source_registry->find_source(qualifier.source_path)->data;
    message += u8"illegal image layout qualifier '"_sv;
    message += get_source_bit(source, qualifier);
    message += u8"' on non-image type '"_sv;
    message += get_source_bit(source, type);
    message += u8"'\n"_sv;
    if(ctx.diagnostics.extended) {
      // TODO: Underline the qualifier and the type?
      print_source_snippet(ctx, message, source, qualifier);
      message += '\n';
    }
    return message;
  }

  anton::String
  format_missing_vertex_stage_error([[maybe_unused]] Context const& ctx,
                                    anton::String const& pass_name)
  {
    return anton::format(ctx.allocator,
                         u8"error: missing vertex stage in pass '{}'\n"_sv,
                         pass_name);
  }

  anton::String
  format_graphics_and_compute_stages([[maybe_unused]] Context const& ctx,
                                     anton::String const& pass_name)
  {
    return anton::format(
      ctx.allocator,
      u8"error: pass must have either compute or graphics (vertex, fragment) stages. '{}' has both\n"_sv,
      pass_name);
  }

  Error err_stage_return_must_be_builtin_or_struct(
    Context const& ctx, anton::String_View const pass_name,
    Source_Info const& stage, Source_Info const& return_type)
  {
    Error error = error_from_source(ctx.allocator, return_type);
    anton::String_View const source =
      ctx.source_registry->find_source(return_type.source_path)->data;
    anton::String_View const stage_str = get_source_bit(source, stage);
    error.diagnostic = anton::format(
      ctx.allocator,
      "error: the return type of the {} stage of '{}' is not a builtin or struct"_sv,
      stage_str, pass_name);
    print_source_snippet(ctx, error.extended_diagnostic, source, return_type);
    error.extended_diagnostic += " return type must be a builtin or struct"_sv;
    return error;
  }

  Error err_compute_return_must_be_void(Context const& ctx,
                                        anton::String_View const pass_name,
                                        Source_Info const& return_type)
  {
    Error error = error_from_source(ctx.allocator, return_type);
    error.diagnostic += anton::format(
      ctx.allocator,
      "error: the return type of the compute stage of '{}' must be void\n"_sv,
      pass_name);
    anton::String_View const source =
      ctx.source_registry->find_source(return_type.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, return_type);
    error.extended_diagnostic += " return type must be void"_sv;
    return error;
  }

  Error err_duplicate_attribute(Context const& ctx, Source_Info const& old_attr,
                                Source_Info const& new_attr)
  {
    Error error = error_from_source(ctx.allocator, new_attr);
    anton::String_View const source =
      ctx.source_registry->find_source(new_attr.source_path)->data;
    error.diagnostic =
      anton::format(ctx.allocator, "error: duplicate attribute '{}'"_sv,
                    get_source_bit(source, new_attr));
    print_source_snippet(ctx, error.extended_diagnostic, source, old_attr);
    error.extended_diagnostic += " attribute first appeared here\n"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, new_attr);
    error.extended_diagnostic += " duplicated here"_sv;
    return error;
  }

  Error err_illegal_attribute(Context const& ctx, Source_Info const& attr)
  {
    Error error = error_from_source(ctx.allocator, attr);
    anton::String_View const source =
      ctx.source_registry->find_source(attr.source_path)->data;
    error.diagnostic =
      anton::format(ctx.allocator, "error: illegal attribute '{}'"_sv,
                    get_source_bit(source, attr));
    print_source_snippet(ctx, error.extended_diagnostic, source, attr);
    error.extended_diagnostic += " attribute not allowed"_sv;
    return error;
  }

  Error err_empty_struct(Context const& ctx, Source_Info const& struct_name)
  {
    Error error = error_from_source(ctx.allocator, struct_name);
    anton::String_View const source =
      ctx.source_registry->find_source(struct_name.source_path)->data;
    anton::String_View const name = get_source_bit(source, struct_name);
    error.diagnostic = anton::format(
      ctx.allocator,
      "error: structs must have at least one member, but '{}' is empty"_sv,
      name);
    print_source_snippet(ctx, error.extended_diagnostic, source, struct_name);
    error.extended_diagnostic += " defined here with an empty body"_sv;
    return error;
  }

  Error err_duplicate_struct_field(Context const& ctx,
                                   Source_Info const& first_member_name,
                                   Source_Info const& second_member_name)
  {
    Error error = error_from_source(ctx.allocator, second_member_name);
    anton::String_View const source =
      ctx.source_registry->find_source(second_member_name.source_path)->data;
    anton::String_View const name = get_source_bit(source, second_member_name);
    error.diagnostic =
      anton::format(ctx.allocator, "error: duplicate member '{}'"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source,
                         first_member_name);
    error.extended_diagnostic += " defined here\n"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source,
                         second_member_name);
    error.extended_diagnostic += " duplicated here"_sv;
    return error;
  }

  Error err_opaque_type_in_struct(Context const& ctx, Source_Info const& type)
  {
    Error error = error_from_source(ctx.allocator, type);
    anton::String_View const source =
      ctx.source_registry->find_source(type.source_path)->data;
    anton::String_View const name = get_source_bit(source, type);
    error.diagnostic = anton::format(
      ctx.allocator, "error: opaque type '{}' may not be used inside struct"_sv,
      name);
    print_source_snippet(ctx, error.extended_diagnostic, source, type);
    error.extended_diagnostic += " opaque type used\n"_sv;
    return error;
  }

  Error err_recursive_type_definition(Context const& ctx,
                                      Source_Info const& struct_name,
                                      Source_Info const& type)
  {
    Error error = error_from_source(ctx.allocator, type);
    anton::String_View const source =
      ctx.source_registry->find_source(struct_name.source_path)->data;
    anton::String_View const name = get_source_bit(source, struct_name);
    error.diagnostic = anton::format(
      ctx.allocator, "error: recursively defined type '{}'"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, type);
    error.extended_diagnostic += anton::format(
      ctx.allocator, " '{}' used within its own definition\n"_sv, name);
    return error;
  }

  Error
  err_source_import_failed(Context const& ctx, Source_Info const& import_info,
                           anton::String_View const source_callback_message)
  {
    Error error = error_from_source(ctx.allocator, import_info);
    error.diagnostic =
      anton::String("error: source import failed with the following error: "_sv,
                    ctx.allocator);
    error.diagnostic += source_callback_message;
    anton::String_View const source =
      ctx.source_registry->find_source(import_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, import_info);
    return error;
  }

  Error err_source_import_failed_no_location(
    Context const& ctx, anton::String_View const source_callback_message)
  {
    Error error;
    error.line = 1;
    error.column = 1;
    error.end_line = 1;
    error.end_column = 1;
    error.source = anton::String("<vush>"_sv, ctx.allocator);
    error.diagnostic =
      anton::String("error: source import failed with the following error: "_sv,
                    ctx.allocator);
    error.diagnostic += source_callback_message;
    error.extended_diagnostic = anton::String(ctx.allocator);
    return error;
  }

  Error err_duplicate_label(Context const& ctx, Source_Info const& first,
                            Source_Info const& second)
  {
    Error error = error_from_source(ctx.allocator, second);
    anton::String_View const source =
      ctx.source_registry->find_source(first.source_path)->data;
    anton::String_View const label = get_source_bit(source, first);
    error.diagnostic =
      anton::format(ctx.allocator, "error: duplicate label '{}'"_sv, label);
    print_source_snippet(ctx, error.extended_diagnostic, source, first);
    error.extended_diagnostic += " label first appeared here...\n"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, second);
    error.extended_diagnostic += " ...and then the second time here"_sv;
    return error;
  }

  anton::String format_duplicate_sourced_parameter(
    Context const& ctx, Source_Info const& first, Source_Info const& first_type,
    Source_Info const& second, Source_Info const& second_type)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, second);
    message +=
      u8"error: duplicate sourced parameter name with a different type\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const first_source =
        ctx.source_registry->find_source(first.source_path)->data;
      message += format_diagnostic_location(ctx.allocator, first);
      message += u8"first definition with type '"_sv;
      message += get_source_bit(first_source, first_type);
      message += u8"' found here\n"_sv;
      print_source_snippet(ctx, message, first_source, first);
      message += '\n';
      anton::String_View const second_source =
        ctx.source_registry->find_source(second.source_path)->data;
      message += format_diagnostic_location(ctx.allocator, second);
      message += u8"second definition with type '"_sv;
      message += get_source_bit(second_source, second_type);
      message += u8"' found here\n"_sv;
      print_source_snippet(ctx, message, second_source, second);
      message += '\n';
    }
    return message;
  }

  // Error err_duplicate_default_label(Context const& ctx, Source_Info const& first, Source_Info const& second) {
  //     anton::String message = format_diagnostic_location(ctx.allocator, second);
  //     message += u8"error: duplicate 'default' label in switch statement\n"_sv;
  //     if(ctx.diagnostics.extended) {
  //         anton::String_View const first_source = ctx.source_registry->find_source(first.source_path)->data;
  //         message += format_diagnostic_location(ctx.allocator, first);
  //         message += u8"first occurence of 'default' found here\n"_sv;
  //         print_source_snippet(ctx, message, first_source, first);
  //         message += '\n';
  //         anton::String_View const second_source = ctx.source_registry->find_source(second.source_path)->data;
  //         message += format_diagnostic_location(ctx.allocator, second);
  //         message += u8"second occurence of 'default' found here\n"_sv;
  //         print_source_snippet(ctx, message, second_source, second);
  //         message += '\n';
  //     }
  //     return message;
  // }

  // Error err_duplicate_label(Context const& ctx, Source_Info const& first, Source_Info const& second) {
  //     anton::String_View const second_source = ctx.source_registry->find_source(second.source_path)->data;
  //     an*ton::String message = format_diagnostic_location(ctx.allocator, second);
  //     message += u8"error: duplicate '"_sv;
  //     message += get_source_bit(second_source, second);
  //     message += u8"' label in switch statement\n"_sv;
  //     if(ctx.diagnostics.extended) {
  //         anton::String_View const first_source = ctx.source_registry->find_source(first.source_path)->data;
  //         message += format_diagnostic_location(ctx.allocator, first);
  //         message += u8"first occurence of '"_sv;
  //         message += get_source_bit(first_source, first);
  //         message += u8"' found here\n"_sv;
  //         print_source_snippet(ctx, message, first_source, first);
  //         message += '\n';
  //         message += format_diagnostic_location(ctx.allocator, second);
  //         message += u8"second occurence of '"_sv;
  //         message += get_source_bit(second_source, second);
  //         message += u8"' found here\n"_sv;
  //         print_source_snippet(ctx, message, second_source, second);
  //         message += '\n';
  //     }
  //     return message;
  // }

  // Error err_invalid_switch_arm_expression(Context const& ctx, Source_Info const& expression) {}

  Error err_identifier_is_not_a_constant(Context const& ctx,
                                         Source_Info const& identifier)
  {
    Error error = error_from_source(ctx.allocator, identifier);
    anton::String_View const source =
      ctx.source_registry->find_source(identifier.source_path)->data;
    anton::String_View const name = get_source_bit(source, identifier);
    error.diagnostic =
      anton::format(ctx.allocator, "error: '{}' is not a constant"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, identifier);
    error.extended_diagnostic +=
      anton::format(ctx.allocator, " '{}' is not a constant"_sv, name);
    return error;
  }

  Error err_expression_is_not_constant_evaluable(Context const& ctx,
                                                 Source_Info const& expression)
  {
    Error error = error_from_source(ctx.allocator, expression);
    anton::String_View const source =
      ctx.source_registry->find_source(expression.source_path)->data;
    error.diagnostic = "error: expression is not constant evaluable'"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, expression);
    error.extended_diagnostic += " not constant evaluable"_sv;
    return error;
  }

  Error err_unimplemented(Context const& ctx, Source_Info const& source_info,
                          anton::String_View const file, i64 const line)
  {
    Error error = error_from_source(ctx.allocator, source_info);
    // Trim the base of the path up to 'compiler'.
    i64 const offset = anton::find_substring(file, "compiler"_sv);
    anton::String_View trimmed_file = anton::shrink_front_bytes(file, offset);
    error.diagnostic =
      anton::format("error: unimplemented at {}:{}"_sv, trimmed_file, line);
    anton::String_View const source =
      ctx.source_registry->find_source(source_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic +=
      " resulted in the compiler reaching an unimplemented path"_sv;
    return error;
  }
} // namespace vush
