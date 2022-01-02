#include <diagnostics.hpp>

#include <anton/format.hpp>
#include <ast.hpp>

namespace vush {
    using namespace anton::literals;

    static void print_underline(anton::String& out, i64 const padding, i64 const underline_length) {
        for(i64 i = 0; i < padding; ++i) {
            out += U' ';
        }

        for(i64 i = 0; i < underline_length; ++i) {
            out += U'~';
        }
    }

    struct Line_Limits {
        i64 start;
        i64 end;
    };

    [[nodiscard]] static Line_Limits find_line_limits(anton::String_View const source, i64 const start_pos) {
        Line_Limits limits{start_pos, start_pos};
        char8 const* const begin = source.bytes_begin() - 1;
        char8 const* const end = source.bytes_end();
        // Search backwards
        for(char8 const* data = source.data() + start_pos - 1; data != begin; --data) {
            if(*data == '\n') {
                break;
            }

            limits.start -= 1;
        }
        // Search forward
        for(char8 const* data = source.data() + start_pos + 1; data != end; ++data) {
            if(*data == '\n') {
                break;
            }

            limits.end += 1;
        }

        return limits;
    }

    [[nodiscard]] static i64 calculate_integer_length(i64 integer) {
        i64 length = 0;
        // Account for '-' sign
        if(integer < 0) {
            length = 1;
            integer = -integer;
        }

        do {
            length += 1;
            integer /= 10;
        } while(integer != 0);
        return length;
    }

    static void print_line_number(anton::String& out, i64 const width, anton::Optional<i64> const number) {
        if(number) {
            out += ' ';
            out += anton::to_string(number.value());
            out += u8" | "_sv;
        } else {
            for(i64 i = 0; i < width + 1; ++i) {
                out += ' ';
            }
            out += u8" | "_sv;
        }
    }

    static void print_source_snippet(Context const& ctx, anton::String& out, anton::String_View const source, Source_Info const& src_info) {
        Line_Limits const line = find_line_limits(source, src_info.start_offset);
        anton::String_View const source_bit{source.data() + line.start, source.data() + line.end};
        if(ctx.diagnostics.display_line_numbers) {
            i64 const line_number = src_info.line;
            i64 const line_number_width = calculate_integer_length(line_number);
            print_line_number(out, line_number_width, anton::null_optional);
            out += '\n';
            print_line_number(out, line_number_width, line_number);
            out += source_bit;
            out += '\n';
            print_line_number(out, line_number_width, anton::null_optional);
            i64 const padding = src_info.start_offset - line.start;
            i64 const underline = src_info.end_offset - src_info.start_offset;
            print_underline(out, padding, underline);
        } else {
            out += source_bit;
            out += U'\n';
            i64 const padding = src_info.start_offset - line.start;
            i64 const underline = src_info.end_offset - src_info.start_offset;
            print_underline(out, padding, underline);
        }
    }

    static anton::String_View get_source_bit(anton::String_View const source, Source_Info const& src_info) {
        anton::String_View const source_bit{source.data() + src_info.start_offset, source.data() + src_info.end_offset};
        return source_bit;
    }

    [[nodiscard]] static anton::String format_diagnostic_location(Source_Info const& info) {
        return anton::String{info.source_path} + u8":" + anton::to_string(info.line) + u8":" + anton::to_string(info.column) + u8": ";
    }

    anton::String format_diagnostic_location(anton::String_View const path, i64 const line, i64 const column) {
        return anton::String{path} + u8":" + anton::to_string(line) + u8":" + anton::to_string(column) + u8": ";
    }

    [[nodiscard]] static anton::String_View readable_type_node_type(AST_Node_Type const type) {
        ANTON_ASSERT(type == AST_Node_Type::builtin_type || type == AST_Node_Type::builtin_type || type == AST_Node_Type::array_type, "invalid AST_Node_Type");
        switch(type) {
            case AST_Node_Type::builtin_type:
                return "builtin"_sv;

            case AST_Node_Type::user_defined_type:
                return "user defined"_sv;

            case AST_Node_Type::array_type:
                return "array"_sv;

            default:
                ANTON_UNREACHABLE();
        }
    }

    anton::String format_undefined_symbol(Context const& ctx, Source_Info const& symbol) {
        anton::String_View const source = ctx.source_registry.find(symbol.source_path)->value.data;
        anton::String message = format_diagnostic_location(symbol);
        message += u8"error: undefined symbol '";
        message += get_source_bit(source, symbol);
        message += u8"'\n";
        if(ctx.diagnostics.extended) {
            print_source_snippet(ctx, message, source, symbol);
            message += '\n';
        }
        return message;
    }

    anton::String format_called_symbol_does_not_name_function(Context const& ctx, Source_Info const& symbol) {
        anton::String_View const source = ctx.source_registry.find(symbol.source_path)->value.data;
        anton::String message = format_diagnostic_location(symbol);
        message += u8"error: called symbol '";
        message += get_source_bit(source, symbol);
        message += u8"' does not name a function\n";
        if(ctx.diagnostics.extended) {
            print_source_snippet(ctx, message, source, symbol);
            message += '\n';
        }
        return message;
    }

    anton::String format_symbol_redefinition(Context const& ctx, Source_Info const& first, Source_Info const& second) {
        anton::String message = format_diagnostic_location(second);
        anton::String_View const first_source = ctx.source_registry.find(first.source_path)->value.data;
        anton::String_View const name = get_source_bit(first_source, first);
        message += anton::format(u8"error: redefinition of the symbol '{}'\n"_sv, name);
        if(ctx.diagnostics.extended) {
            message += format_diagnostic_location(first);
            message += anton::format(u8"definition of the symbol '{}' found here\n"_sv, name);
            print_source_snippet(ctx, message, first_source, first);
            message += '\n';
            message += format_diagnostic_location(second);
            message += u8"redefined here\n"_sv;
            anton::String_View const second_source = ctx.source_registry.find(second.source_path)->value.data;
            print_source_snippet(ctx, message, second_source, second);
            message += '\n';
        }
        return message;
    }

    anton::String format_integer_literal_overflow(Context const& ctx, Source_Info const& integer) {
        anton::String message = format_diagnostic_location(integer);
        message += u8"error: integer literal requires more than 32 bits\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.source_registry.find(integer.source_path)->value.data;
            print_source_snippet(ctx, message, source, integer);
            message += '\n';
        }
        return message;
    }

    anton::String format_integer_literal_leading_zeros(Context const& ctx, Source_Info const& integer) {
        anton::String message = format_diagnostic_location(integer);
        message += u8"error: leading zeros in decimal integer literals are not allowed\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.source_registry.find(integer.source_path)->value.data;
            print_source_snippet(ctx, message, source, integer);
            message += '\n';
        }
        return message;
    }

    anton::String format_overload_identical_parameters_different_return_types(Context const& ctx, Function_Declaration const& overload1,
                                                                              Function_Declaration const& overload2) {
        anton::String message = format_diagnostic_location(overload2.source_info);
        message += u8"error: functions may not be overloaded on their return type alone\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const first_source = ctx.source_registry.find(overload1.source_info.source_path)->value.data;
            message += format_diagnostic_location(overload1.source_info);
            message += anton::format(u8"overload with return type '{}' defined here\n", stringify_type(*overload1.return_type));
            print_source_snippet(ctx, message, first_source, overload1.return_type->source_info);
            message += '\n';
            anton::String_View const second_source = ctx.source_registry.find(overload2.source_info.source_path)->value.data;
            message += format_diagnostic_location(overload2.source_info);
            message +=
                anton::format(u8"overload with a different return type '{}', but identical parameters defined here\n", stringify_type(*overload2.return_type));
            print_source_snippet(ctx, message, second_source, overload2.return_type->source_info);
            message += '\n';
        }
        return message;
    }

    anton::String format_overload_identical_parameters(Context const& ctx, Function_Declaration const& overload1, Function_Declaration const& overload2) {
        anton::String message = format_diagnostic_location(overload2.source_info);
        message += anton::format(u8"error: redefinition of function '{}'\n", overload1.identifier->value);
        if(ctx.diagnostics.extended) {
            anton::String_View const first_source = ctx.source_registry.find(overload1.source_info.source_path)->value.data;
            message += format_diagnostic_location(overload1.source_info);
            message += u8"first definition found here\n";
            print_source_snippet(ctx, message, first_source, overload1.source_info);
            message += '\n';
            anton::String_View const second_source = ctx.source_registry.find(overload2.source_info.source_path)->value.data;
            message += format_diagnostic_location(overload2.source_info);
            message += u8"second definition found here\n";
            print_source_snippet(ctx, message, second_source, overload2.source_info);
            message += '\n';
        }
        return message;
    }

    anton::String format_variable_declaration_in_global_scope(Context const& ctx, Source_Info const& declaration) {
        anton::String message = format_diagnostic_location(declaration);
        message += u8"error: illegal declaration of a variable in global scope\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const& source = ctx.source_registry.find(declaration.source_path)->value.data;
            print_source_snippet(ctx, message, source, declaration);
            message += '\n';
        }
        return message;
    }

    anton::String format_constant_missing_initializer(Context const& ctx, Source_Info const& constant) {
        anton::String message = format_diagnostic_location(constant);
        message += u8"error: missing constant initializer\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.source_registry.find(constant.source_path)->value.data;
            print_source_snippet(ctx, message, source, constant);
            message += '\n';
        }
        return message;
    }

    anton::String format_expression_not_implicitly_convertible_to_bool(Context const& ctx, Source_Info const& expression) {
        anton::String message = format_diagnostic_location(expression);
        message += u8"error: expression is not implicitly convertible to bool\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.source_registry.find(expression.source_path)->value.data;
            print_source_snippet(ctx, message, source, expression);
            message += '\n';
        }
        return message;
    }

    anton::String format_ordinary_parameter_not_allowed_on_stage(Context const& ctx, Source_Info const& src, Stage_Type const stage) {
        anton::String message = format_diagnostic_location(src);
        message += u8"ordinary parameters are not allowed on ";
        message += stringify(stage);
        message += u8" stage\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.source_registry.find(src.source_path)->value.data;
            print_source_snippet(ctx, message, source, src);
            message += '\n';
        }
        return message;
    }

    anton::String format_vertex_input_not_allowed_on_stage(Context const& ctx, Source_Info const& src, Stage_Type const stage) {
        anton::String message = format_diagnostic_location(src);
        message += u8"vertex input parameters are not allowed on ";
        message += stringify(stage);
        message += u8" stage\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.source_registry.find(src.source_path)->value.data;
            print_source_snippet(ctx, message, source, src);
            message += '\n';
        }
        return message;
    }

    anton::String format_illegal_image_layout_qualifier_on_non_sourced_parameter(Context const& ctx, Source_Info const& qualifier,
                                                                                 Source_Info const& parameter_identifier) {
        anton::String message = format_diagnostic_location(qualifier);
        anton::String_View const source = ctx.source_registry.find(qualifier.source_path)->value.data;
        message += u8"illegal image layout qualifier '";
        message += get_source_bit(source, qualifier);
        message += u8"' on non-sourced parameter '";
        message += get_source_bit(source, parameter_identifier);
        message += u8"'\n";
        if(ctx.diagnostics.extended) {
            // TODO: Underline the qualifier and the parameter?
            print_source_snippet(ctx, message, source, qualifier);
            message += '\n';
        }
        return message;
    }

    anton::String format_illegal_image_layout_qualifier_on_non_image_type(Context const& ctx, Source_Info const& qualifier, Source_Info const& type) {
        anton::String message = format_diagnostic_location(qualifier);
        anton::String_View const source = ctx.source_registry.find(qualifier.source_path)->value.data;
        message += u8"illegal image layout qualifier '";
        message += get_source_bit(source, qualifier);
        message += u8"' on non-image type '";
        message += get_source_bit(source, type);
        message += u8"'\n";
        if(ctx.diagnostics.extended) {
            // TODO: Underline the qualifier and the type?
            print_source_snippet(ctx, message, source, qualifier);
            message += '\n';
        }
        return message;
    }

    anton::String format_duplicate_pass_stage_error(Context const& ctx, Source_Info const& first, Source_Info const& second, anton::String const& pass_name,
                                                    Stage_Type const& stage) {
        anton::String message = format_diagnostic_location(second) + u8"error: duplicate " + stringify(stage) + u8" stage in pass '" + pass_name + "'\n";
        if(ctx.diagnostics.extended) {
            message += format_diagnostic_location(first);
            message += u8"first definition found here:\n";
            message += format_diagnostic_location(second);
            message += u8"second definition found here:\n";
        }
        return message;
    }

    anton::String format_missing_vertex_stage_error([[maybe_unused]] Context const& ctx, anton::String const& pass_name) {
        return u8"error: missing vertex stage in pass '" + pass_name + u8"'\n";
    }

    anton::String format_graphics_and_compute_stages([[maybe_unused]] Context const& ctx, anton::String const& pass_name) {
        return u8"error: pass must have either compute or graphics (vertex, fragment) stages. '" + pass_name + u8"' has both\n";
    }

    anton::String format_stage_return_type_must_be_void_or_udt(Context const& ctx, anton::String_View const pass_name, Stage_Type const stage,
                                                               Type const& return_type) {
        anton::String_View const source = ctx.source_registry.find(return_type.source_info.source_path)->value.data;
        anton::String_View const stage_string = stringify(stage);
        anton::String_View const type = get_source_bit(source, return_type.source_info);
        anton::String_View const readable_type = readable_type_node_type(return_type.node_type);
        anton::String message = format_diagnostic_location(return_type.source_info);
        message += anton::format(
            "error: the {} return type '{}' of the {} stage in the pass {} is not allowed. the return type of a {} stage must be 'void' or a user defined type\n"_sv,
            readable_type, type, stage_string, pass_name, stage_string);
        if(ctx.diagnostics.extended) {
            print_source_snippet(ctx, message, source, return_type.source_info);
            message += "\n"_sv;
        }
        return message;
    }

    anton::String format_stage_input_parameter_must_be_udt(Context const& ctx, anton::String_View const pass_name, Stage_Type const stage,
                                                           Function_Parameter const& parameter) {
        anton::String_View const source = ctx.source_registry.find(parameter.source_info.source_path)->value.data;
        anton::String_View const stage_string = stringify(stage);
        anton::String_View const parameter_name = parameter.identifier->value;
        anton::String const type = stringify_type(*parameter.type);
        anton::String_View const readable_type = readable_type_node_type(parameter.type->node_type);
        anton::String message = format_diagnostic_location(parameter.type->source_info);
        message += anton::format("error: the stage input parameter '{}' in the {} stage in the pass {} has illegal {} type '{}'."_sv, parameter_name,
                                 stage_string, pass_name, readable_type, type);
        message += "the stage input parameter must be a user defined type\n"_sv;
        if(ctx.diagnostics.extended) {
            print_source_snippet(ctx, message, source, parameter.type->source_info);
            message += '\n';
        }
        return message;
    }

    anton::String format_empty_struct([[maybe_unused]] Context const& ctx, Source_Info const& struct_name) {
        anton::String_View const source = ctx.source_registry.find(struct_name.source_path)->value.data;
        anton::String message = format_diagnostic_location(struct_name);
        anton::String_View const name = get_source_bit(source, struct_name);
        message += anton::format(u8"error: structs must not be empty, i.e. they must contain at least one member, but '{}' is an empty struct\n"_sv, name);
        if(ctx.diagnostics.extended) {
            message += format_diagnostic_location(struct_name);
            message += anton::format(u8"'{}' is defined here with an empty body\n"_sv, name);
            print_source_snippet(ctx, message, source, struct_name);
            message += '\n';
        }
        return message;
    }

    anton::String format_compute_return_type_must_be_void(Context const& ctx, Source_Info const& return_type) {
        anton::String message = format_diagnostic_location(return_type);
        message += u8"error: the return type of the compute stage must be void\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.source_registry.find(return_type.source_path)->value.data;
            print_source_snippet(ctx, message, source, return_type);
            message += '\n';
        }
        return message;
    }

    anton::String format_source_import_failed(Context const& ctx, Source_Info const& import_info, anton::String_View const source_callback_message) {
        anton::String message = format_diagnostic_location(import_info);
        message += u8"error: source import failed with the following error: ";
        message += source_callback_message;
        message += '\n';
        if(ctx.diagnostics.extended) {
            anton::String_View const source = ctx.source_registry.find(import_info.source_path)->value.data;
            print_source_snippet(ctx, message, source, import_info);
        }
        return message;
    }

    anton::String format_duplicate_sourced_parameter(Context const& ctx, Source_Info const& first, Source_Info const& first_type, Source_Info const& second,
                                                     Source_Info const& second_type) {
        anton::String message = format_diagnostic_location(second);
        message += u8"error: duplicate sourced parameter name with a different type\n";
        if(ctx.diagnostics.extended) {
            anton::String_View const first_source = ctx.source_registry.find(first.source_path)->value.data;
            message += format_diagnostic_location(first);
            message += u8"first definition with type '";
            message += get_source_bit(first_source, first_type);
            message += u8"' found here\n";
            print_source_snippet(ctx, message, first_source, first);
            message += '\n';
            anton::String_View const second_source = ctx.source_registry.find(second.source_path)->value.data;
            message += format_diagnostic_location(second);
            message += u8"second definition with type '";
            message += get_source_bit(second_source, second_type);
            message += u8"' found here\n";
            print_source_snippet(ctx, message, second_source, second);
            message += '\n';
        }
        return message;
    }

    anton::String format_duplicate_default_label(Context const& ctx, Source_Info const& first, Source_Info const& second) {
        anton::String message = format_diagnostic_location(second);
        message += u8"error: duplicate 'default' label in switch statement\n"_sv;
        if(ctx.diagnostics.extended) {
            anton::String_View const first_source = ctx.source_registry.find(first.source_path)->value.data;
            message += format_diagnostic_location(first);
            message += u8"first occurence of 'default' found here\n"_sv;
            print_source_snippet(ctx, message, first_source, first);
            message += '\n';
            anton::String_View const second_source = ctx.source_registry.find(second.source_path)->value.data;
            message += format_diagnostic_location(second);
            message += u8"second occurence of 'default' found here\n"_sv;
            print_source_snippet(ctx, message, second_source, second);
            message += '\n';
        }
        return message;
    }

    anton::String format_duplicate_label(Context const& ctx, Source_Info const& first, Source_Info const& second) {
        anton::String_View const second_source = ctx.source_registry.find(second.source_path)->value.data;
        anton::String message = format_diagnostic_location(second);
        message += u8"error: duplicate '"_sv;
        message += get_source_bit(second_source, second);
        message += u8"' label in switch statement\n"_sv;
        if(ctx.diagnostics.extended) {
            anton::String_View const first_source = ctx.source_registry.find(first.source_path)->value.data;
            message += format_diagnostic_location(first);
            message += u8"first occurence of '"_sv;
            message += get_source_bit(first_source, first);
            message += u8"' found here\n"_sv;
            print_source_snippet(ctx, message, first_source, first);
            message += '\n';
            message += format_diagnostic_location(second);
            message += u8"second occurence of '"_sv;
            message += get_source_bit(second_source, second);
            message += u8"' found here\n"_sv;
            print_source_snippet(ctx, message, second_source, second);
            message += '\n';
        }
        return message;
    }
} // namespace vush
