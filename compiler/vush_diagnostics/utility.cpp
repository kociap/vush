#include <vush_diagnostics/utility.hpp>

#include <vush_ast/ast.hpp>
#include <vush_core/context.hpp>

namespace vush {
  using namespace anton::literals;

  Error error_from_source(Allocator* const allocator, Source_Info const& info)
  {
    return Error{.line = info.line,
                 .column = info.column,
                 .source = anton::String(info.source->path, allocator),
                 .diagnostic = anton::String(allocator),
                 .extended_diagnostic = anton::String(allocator)};
  }

  Error error_from_source(Allocator* const allocator,
                          anton::String_View const source_path, i64 const line,
                          i64 const column)
  {
    return Error{.line = line,
                 .column = column,
                 .source = anton::String(source_path, allocator),
                 .diagnostic = anton::String(allocator),
                 .extended_diagnostic = anton::String(allocator)};
  }

  anton::String format_diagnostic_location(Allocator* const allocator,
                                           anton::String_View const source_path,
                                           i64 const line, i64 const column)
  {
    return anton::concat(allocator, source_path, u8":"_sv,
                         anton::to_string(allocator, line), u8":"_sv,
                         anton::to_string(allocator, column), u8": "_sv);
  }

  anton::String format_diagnostic_location(Allocator* const allocator,
                                           Source_Info const& info)
  {
    return format_diagnostic_location(allocator, info.source->path, info.line,
                                      info.column);
  }

  anton::String_View get_source_bit(anton::String_View const source,
                                    i64 const offset, i64 const end_offset)
  {
    anton::String_View const source_bit{source.data() + offset,
                                        source.data() + end_offset};
    return source_bit;
  }

  anton::String_View get_source_bit(anton::String_View const source,
                                    Source_Info const& src_info)
  {
    return get_source_bit(source, src_info.offset, src_info.end_offset);
  }

  static void print_underline(anton::String& out, i64 const padding,
                              i64 const underline_length)
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

  [[nodiscard]] static Line_Limits
  find_line_limits(anton::String_View const source, i64 const start_pos)
  {
    Line_Limits limits{start_pos, start_pos};
    char8 const* const begin = source.bytes_begin() - 1;
    char8 const* const end = source.bytes_end();
    // Search backwards
    for(char8 const* data = source.data() + start_pos - 1; data != begin;
        --data) {
      if(*data == '\n') {
        break;
      }

      limits.start -= 1;
    }
    // Search forward
    for(char8 const* data = source.data() + start_pos + 1; data != end;
        ++data) {
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

  void print_left_margin(Allocator* const allocator, anton::String& out,
                         i64 const width, anton::Optional<i64> const number)
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

  void print_source_snippet(Context const& ctx, anton::String& out,
                            anton::String_View const source, i64 const offset,
                            i64 const end_offset, i64 const line)
  {
    Line_Limits const limits = find_line_limits(source, offset);
    anton::String_View const source_bit{source.data() + limits.start,
                                        source.data() + limits.end};
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

  void print_source_snippet(Context const& ctx, anton::String& out,
                            anton::String_View const source,
                            Source_Info const& src_info)
  {
    print_source_snippet(ctx, out, source, src_info.offset, src_info.end_offset,
                         src_info.line);
  }

  anton::String stringify_type(Context const& ctx,
                               ast::Type const* const generic_type)
  {
    switch(generic_type->type_kind) {
    case ast::Type_Kind::type_builtin: {
      auto const type = static_cast<ast::Type_Builtin const*>(generic_type);
      switch(type->value) {
      case ast::Type_Builtin_Kind::e_void:
        return anton::String("void"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_bool:
        return anton::String("bool"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_int:
        return anton::String("int"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uint:
        return anton::String("uint"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_float:
        return anton::String("float"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_double:
        return anton::String("double"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_vec2:
        return anton::String("vec2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_vec3:
        return anton::String("vec3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_vec4:
        return anton::String("vec4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dvec2:
        return anton::String("dvec2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dvec3:
        return anton::String("dvec3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dvec4:
        return anton::String("dvec4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_bvec2:
        return anton::String("bvec2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_bvec3:
        return anton::String("bvec3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_bvec4:
        return anton::String("bvec4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_ivec2:
        return anton::String("ivec2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_ivec3:
        return anton::String("ivec3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_ivec4:
        return anton::String("ivec4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uvec2:
        return anton::String("uvec2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uvec3:
        return anton::String("uvec3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uvec4:
        return anton::String("uvec4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat2:
        return anton::String("mat2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat3:
        return anton::String("mat3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat4:
        return anton::String("mat4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat2x3:
        return anton::String("mat2x3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat2x4:
        return anton::String("mat2x4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat3x2:
        return anton::String("mat3x2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat3x4:
        return anton::String("mat3x4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat4x2:
        return anton::String("mat4x2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_mat4x3:
        return anton::String("mat4x3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat2:
        return anton::String("dmat2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat3:
        return anton::String("dmat3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat4:
        return anton::String("dmat4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat2x3:
        return anton::String("dmat2x3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat2x4:
        return anton::String("dmat2x4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat3x2:
        return anton::String("dmat3x2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat3x4:
        return anton::String("dmat3x4"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat4x2:
        return anton::String("dmat4x2"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_dmat4x3:
        return anton::String("dmat4x3"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler1D:
        return anton::String("sampler1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_texture1D:
        return anton::String("texture1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_image1D:
        return anton::String("image1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler1DShadow:
        return anton::String("sampler1DShadow"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler1DArray:
        return anton::String("sampler1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_texture1DArray:
        return anton::String("texture1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_image1DArray:
        return anton::String("image1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler1DArrayShadow:
        return anton::String("sampler1DArrayShadow"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler2D:
        return anton::String("sampler2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_texture2D:
        return anton::String("texture2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_image2D:
        return anton::String("image2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler2DShadow:
        return anton::String("sampler2DShadow"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler2DArray:
        return anton::String("sampler2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_texture2DArray:
        return anton::String("texture2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_image2DArray:
        return anton::String("image2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler2DArrayShadow:
        return anton::String("sampler2DArrayShadow"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler2DMS:
        return anton::String("sampler2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_texture2DMS:
        return anton::String("texture2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_image2DMS:
        return anton::String("image2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler2DMSArray:
        return anton::String("sampler2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_texture2DMSArray:
        return anton::String("texture2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_image2DMSArray:
        return anton::String("image2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler2DRect:
        return anton::String("sampler2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_texture2DRect:
        return anton::String("texture2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_image2DRect:
        return anton::String("image2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler2DRectShadow:
        return anton::String("sampler2DRectShadow"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler3D:
        return anton::String("sampler3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_texture3D:
        return anton::String("texture3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_image3D:
        return anton::String("image3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_samplerCube:
        return anton::String("samplerCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_textureCube:
        return anton::String("textureCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_imageCube:
        return anton::String("imageCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_samplerCubeShadow:
        return anton::String("samplerCubeShadow"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_samplerCubeArray:
        return anton::String("samplerCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_textureCubeArray:
        return anton::String("textureCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_imageCubeArray:
        return anton::String("imageCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_samplerCubeArrayShadow:
        return anton::String("samplerCubeArrayShadow"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_samplerBuffer:
        return anton::String("samplerBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_textureBuffer:
        return anton::String("textureBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_imageBuffer:
        return anton::String("imageBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_subpassInput:
        return anton::String("subpassInput"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_subpassInputMS:
        return anton::String("subpassInputMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isampler1D:
        return anton::String("isampler1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itexture1D:
        return anton::String("itexture1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimage1D:
        return anton::String("iimage1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isampler1DArray:
        return anton::String("isampler1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itexture1DArray:
        return anton::String("itexture1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimage1DArray:
        return anton::String("iimage1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isampler2D:
        return anton::String("isampler2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itexture2D:
        return anton::String("itexture2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimage2D:
        return anton::String("iimage2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isampler2DArray:
        return anton::String("isampler2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itexture2DArray:
        return anton::String("itexture2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimage2DArray:
        return anton::String("iimage2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isampler2DMS:
        return anton::String("isampler2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itexture2DMS:
        return anton::String("itexture2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimage2DMS:
        return anton::String("iimage2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isampler2DMSArray:
        return anton::String("isampler2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itexture2DMSArray:
        return anton::String("itexture2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimage2DMSArray:
        return anton::String("iimage2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isampler2DRect:
        return anton::String("isampler2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itexture2DRect:
        return anton::String("itexture2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimage2DRect:
        return anton::String("iimage2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isampler3D:
        return anton::String("isampler3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itexture3D:
        return anton::String("itexture3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimage3D:
        return anton::String("iimage3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isamplerCube:
        return anton::String("isamplerCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itextureCube:
        return anton::String("itextureCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimageCube:
        return anton::String("iimageCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isamplerCubeArray:
        return anton::String("isamplerCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itextureCubeArray:
        return anton::String("itextureCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimageCubeArray:
        return anton::String("iimageCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isamplerBuffer:
        return anton::String("isamplerBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_itextureBuffer:
        return anton::String("itextureBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_iimageBuffer:
        return anton::String("iimageBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isubpassInput:
        return anton::String("isubpassInput"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_isubpassInputMS:
        return anton::String("isubpassInputMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usampler1D:
        return anton::String("usampler1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utexture1D:
        return anton::String("utexture1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimage1D:
        return anton::String("uimage1D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usampler1DArray:
        return anton::String("usampler1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utexture1DArray:
        return anton::String("utexture1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimage1DArray:
        return anton::String("uimage1DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usampler2D:
        return anton::String("usampler2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utexture2D:
        return anton::String("utexture2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimage2D:
        return anton::String("uimage2D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usampler2DArray:
        return anton::String("usampler2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utexture2DArray:
        return anton::String("utexture2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimage2DArray:
        return anton::String("uimage2DArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usampler2DMS:
        return anton::String("usampler2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utexture2DMS:
        return anton::String("utexture2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimage2DMS:
        return anton::String("uimage2DMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usampler2DMSArray:
        return anton::String("usampler2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utexture2DMSArray:
        return anton::String("utexture2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimage2DMSArray:
        return anton::String("uimage2DMSArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usampler2DRect:
        return anton::String("usampler2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utexture2DRect:
        return anton::String("utexture2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimage2DRect:
        return anton::String("uimage2DRect"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usampler3D:
        return anton::String("usampler3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utexture3D:
        return anton::String("utexture3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimage3D:
        return anton::String("uimage3D"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usamplerCube:
        return anton::String("usamplerCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utextureCube:
        return anton::String("utextureCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimageCube:
        return anton::String("uimageCube"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usamplerCubeArray:
        return anton::String("usamplerCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utextureCubeArray:
        return anton::String("utextureCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimageCubeArray:
        return anton::String("uimageCubeArray"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usamplerBuffer:
        return anton::String("usamplerBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_utextureBuffer:
        return anton::String("utextureBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_uimageBuffer:
        return anton::String("uimageBuffer"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usubpassInput:
        return anton::String("usubpassInput"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_usubpassInputMS:
        return anton::String("usubpassInputMS"_sv, ctx.allocator);
      case ast::Type_Builtin_Kind::e_sampler:
        return anton::String("sampler"_sv, ctx.allocator);
      }
    } break;

    case ast::Type_Kind::type_struct: {
      auto const type = static_cast<ast::Type_Struct const*>(generic_type);
      return anton::String(type->value, ctx.allocator);
    } break;

    case ast::Type_Kind::type_array: {
      auto const type = static_cast<ast::Type_Array const*>(generic_type);
      anton::String base = stringify_type(ctx, type->base);
      anton::String size;
      if(type->size) {
        switch(type->size->kind) {
        case ast::Lt_Integer_Kind::i32: {
          size = anton::to_string(ctx.allocator, type->size->i32_value);
        } break;

        case ast::Lt_Integer_Kind::u32: {
          size = anton::to_string(ctx.allocator, type->size->u32_value);
        } break;
        }
      }
      return anton::concat(ctx.allocator, "["_sv, base, ";"_sv, size, "]"_sv);
    } break;
    }
  }

  anton::String stringify_builtin_function(Context const& ctx,
                                           ast::Decl_Function const* const fn)
  {
    anton::String return_type = stringify_type(ctx, fn->return_type);
    anton::String result = anton::concat(ctx.allocator, return_type, " "_sv,
                                         fn->identifier.value, "("_sv);
    for(bool first = true;
        ast::Fn_Parameter const* const parameter: fn->parameters) {
      if(!first) {
        result += ", "_sv;
      }
      result += stringify_type(ctx, parameter->type);
      result += " "_sv;
      result += parameter->identifier.value;

      first = false;
    }
    result += ") {...}"_sv;
    return result;
  }
} // namespace vush
