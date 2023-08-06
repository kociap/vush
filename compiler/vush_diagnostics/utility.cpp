#include <vush_diagnostics/utility.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/context.hpp>

namespace vush {
  using namespace anton::literals;

  Error error_from_source(Allocator* const allocator, Source_Info const& info)
  {
    return Error{.line = info.line,
                 .column = info.column,
                 .end_line = info.end_line,
                 .end_column = info.end_column,
                 .source = anton::String(info.source_path, allocator),
                 .diagnostic = anton::String(allocator),
                 .extended_diagnostic = anton::String(allocator)};
  }

  Error error_from_source(Allocator* const allocator, anton::String_View const source_path,
                          i64 const line, i64 const column)
  {
    return Error{.line = line,
                 .column = column,
                 .end_line = line,
                 .end_column = column + 1,
                 .source = anton::String(source_path, allocator),
                 .diagnostic = anton::String(allocator),
                 .extended_diagnostic = anton::String(allocator)};
  }

  anton::String format_diagnostic_location(Allocator* const allocator,
                                           anton::String_View const source_path, i64 const line,
                                           i64 const column)
  {
    return anton::concat(allocator, source_path, u8":"_sv, anton::to_string(allocator, line),
                         u8":"_sv, anton::to_string(allocator, column), u8": "_sv);
  }

  anton::String format_diagnostic_location(Allocator* const allocator, Source_Info const& info)
  {
    return format_diagnostic_location(allocator, info.source_path, info.line, info.column);
  }

  anton::String_View get_source_bit(anton::String_View const source, i64 const offset,
                                    i64 const end_offset)
  {
    anton::String_View const source_bit{source.data() + offset, source.data() + end_offset};
    return source_bit;
  }

  anton::String_View get_source_bit(anton::String_View const source, Source_Info const& src_info)
  {
    return get_source_bit(source, src_info.offset, src_info.end_offset);
  }

  static void print_underline(anton::String& out, i64 const padding, i64 const underline_length)
  {
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

  [[nodiscard]] static Line_Limits find_line_limits(anton::String_View const source,
                                                    i64 const start_pos)
  {
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

  [[nodiscard]] static i64 calculate_integer_length(i64 integer)
  {
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

  void print_left_margin(Allocator* const allocator, anton::String& out, i64 const width,
                         anton::Optional<i64> const number)
  {
    if(number) {
      out += ' ';
      out += anton::to_string(allocator, number.value());
      out += u8" | "_sv;
    } else {
      for(i64 i = 0; i < width + 1; ++i) {
        out += ' ';
      }
      out += u8" | "_sv;
    }
  }

  void print_source_snippet(Context const& ctx, anton::String& out, anton::String_View const source,
                            i64 const offset, i64 const end_offset, i64 const line)
  {
    Line_Limits const limits = find_line_limits(source, offset);
    anton::String_View const source_bit{source.data() + limits.start, source.data() + limits.end};
    if(ctx.diagnostics.display_line_numbers) {
      i64 const line_number = line;
      i64 const line_number_width = calculate_integer_length(line_number);
      print_left_margin(ctx.allocator, out, line_number_width);
      out += '\n';
      print_left_margin(ctx.allocator, out, line_number_width, line_number);
      out += source_bit;
      out += '\n';
      print_left_margin(ctx.allocator, out, line_number_width);
      i64 const padding = offset - limits.start;
      i64 const underline = end_offset - offset;
      print_underline(out, padding, underline);
    } else {
      out += source_bit;
      out += U'\n';
      i64 const padding = offset - limits.start;
      i64 const underline = end_offset - offset;
      print_underline(out, padding, underline);
    }
  }

  void print_source_snippet(Context const& ctx, anton::String& out, anton::String_View const source,
                            Source_Info const& src_info)
  {
    print_source_snippet(ctx, out, source, src_info.offset, src_info.end_offset, src_info.line);
  }
} // namespace vush
