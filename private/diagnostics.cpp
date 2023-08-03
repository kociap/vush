#include <diagnostics.hpp>

#include <anton/format.hpp>

#include <ast.hpp>
#include <diagnostics/utility.hpp>

namespace vush {
  using namespace anton::literals;

  anton::String Error::format(Allocator* const allocator,
                              bool const include_extended_diagnostic) const
  {
    // Add 32 bytes for line and column numbers, colons, spaces and newlines.
    i64 const size =
      source.size_bytes() + diagnostic.size_bytes() + extended_diagnostic.size_bytes() + 32;
    anton::String error_message{anton::reserve, size, allocator};
    error_message += format_diagnostic_location(allocator, source, line, column);
    error_message += diagnostic;
    if(include_extended_diagnostic && extended_diagnostic.size_bytes() > 0) {
      error_message += "\n"_sv;
      error_message += extended_diagnostic;
    }
    return error_message;
  }

  [[nodiscard]] static anton::String stringify_type(Context const& ctx,
                                                    ast::Type const* const generic_type)
  {
    switch(generic_type->node_kind) {
    case ast::Node_Kind::type_builtin: {
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
      case ast::Type_Builtin_Kind::e_samplerShadow:
        return anton::String("samplerShadow"_sv, ctx.allocator);
      }
    } break;

    case ast::Node_Kind::type_struct: {
      auto const type = static_cast<ast::Type_Struct const*>(generic_type);
      return anton::String(type->value, ctx.allocator);
    } break;

    case ast::Node_Kind::type_array: {
      auto const type = static_cast<ast::Type_Array const*>(generic_type);
      anton::String base = stringify_type(ctx, type->base);
      anton::String size;
      if(type->size) {
        size = anton::String(type->size->value, ctx.allocator);
      }
      return anton::concat(ctx.allocator, "["_sv, base, ";"_sv, size, "]"_sv);
    } break;

    default:
      ANTON_ASSERT(false, "invalid type kind");
      ANTON_UNREACHABLE();
    }
  }

  [[nodiscard]] static anton::String stringify_builtin_function(Context const& ctx,
                                                                ast::Decl_Function const* const fn)
  {
    anton::String return_type = stringify_type(ctx, fn->return_type);
    anton::String result =
      anton::concat(ctx.allocator, return_type, " "_sv, fn->identifier->value, "("_sv);
    for(bool first = true; ast::Fn_Parameter const* const parameter: fn->parameters) {
      if(!first) {
        result += ", "_sv;
      }
      result += stringify_type(ctx.allocator, parameter->type);
      result += " "_sv;
      result += parameter->identifier->value;

      first = false;
    }
    result += ") {...}"_sv;
    return result;
  }

  anton::String format_undefined_symbol(Context const& ctx, Source_Info const& symbol)
  {
    anton::String_View const source = ctx.find_source(symbol.source_path)->data;
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

  Error err_undefined_symbol(Context const& ctx, Source_Info const& symbol)
  {
    Error error = error_from_source(ctx.allocator, symbol);
    anton::String_View const source = ctx.find_source(symbol.source_path)->data;
    anton::String_View const name = get_source_bit(source, symbol);
    error.diagnostic = anton::format(ctx.allocator, u8"error: undefined symbol '{}'"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, symbol);
    error.extended_diagnostic +=
      anton::format(ctx.allocator, u8" '{}' used, but not defined"_sv, name);
    return error;
  }

  Error err_symbol_redefinition(Context const& ctx, Source_Info const& old_symbol,
                                Source_Info const& new_symbol)
  {
    Error error = error_from_source(ctx.allocator, new_symbol);
    anton::String_View const new_source = ctx.find_source(new_symbol.source_path)->data;
    anton::String_View const old_source = ctx.find_source(old_symbol.source_path)->data;
    anton::String_View const name = get_source_bit(new_source, new_symbol);
    error.diagnostic =
      anton::format(ctx.allocator, u8"error: symbol '{}' is defined multiple times"_sv, name);
    error.extended_diagnostic = format_diagnostic_location(ctx.allocator, old_symbol);
    error.extended_diagnostic += '\n';
    print_source_snippet(ctx, error.extended_diagnostic, old_source, old_symbol);
    error.extended_diagnostic +=
      anton::format(ctx.allocator, u8" definition of '{}' here\n"_sv, name);
    error.extended_diagnostic += format_diagnostic_location(ctx.allocator, new_symbol);
    error.extended_diagnostic += '\n';
    print_source_snippet(ctx, error.extended_diagnostic, new_source, new_symbol);
    error.extended_diagnostic += u8" redefined here"_sv;
    return error;
  }

  // anton::String format_called_symbol_does_not_name_function(Context const& ctx, Source_Info const& symbol) {
  //     anton::String_View const source = ctx.find_source(symbol.source_path)->data;
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

  Error err_invalid_integer_suffix(Context const& ctx, Source_Info const& suffix)
  {
    Error error = error_from_source(ctx.allocator, suffix);
    anton::String_View const source = ctx.find_source(suffix.source_path)->data;
    error.diagnostic = anton::format(ctx.allocator, "error: invalid integer suffix '{}'"_sv,
                                     get_source_bit(source, suffix));
    print_source_snippet(ctx, error.extended_diagnostic, source, suffix);
    error.extended_diagnostic += " valid suffixes are 'u' and 'U'"_sv;
    return error;
  }

  Error err_invalid_float_suffix(Context const& ctx, Source_Info const& suffix)
  {
    Error error = error_from_source(ctx.allocator, suffix);
    anton::String_View const source = ctx.find_source(suffix.source_path)->data;
    error.diagnostic = anton::format(ctx.allocator, "error: invalid float suffix '{}'"_sv,
                                     get_source_bit(source, suffix));
    print_source_snippet(ctx, error.extended_diagnostic, source, suffix);
    error.extended_diagnostic += " valid suffixes are 'd' and 'D'"_sv;
    return error;
  }

  anton::String format_integer_literal_overflow(Context const& ctx, Source_Info const& integer)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, integer);
    message += u8"error: integer literal requires more than 32 bits\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const source = ctx.find_source(integer.source_path)->data;
      print_source_snippet(ctx, message, source, integer);
      message += '\n';
    }
    return message;
  }

  anton::String format_integer_literal_leading_zeros(Context const& ctx, Source_Info const& integer)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, integer);
    message += u8"error: leading zeros in decimal integer literals are not allowed\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const source = ctx.find_source(integer.source_path)->data;
      print_source_snippet(ctx, message, source, integer);
      message += '\n';
    }
    return message;
  }

  Error err_overload_on_return_type(Context const& ctx, Source_Info const& identifier1,
                                    Source_Info const& return1, Source_Info const& identifier2,
                                    Source_Info const& return2)
  {
    // TODO: Separate error function for user-builtin functions.
    Error error = error_from_source(ctx.allocator, identifier2);
    anton::String_View const source1 = ctx.find_source(identifier1.source_path)->data;
    error.diagnostic = anton::String(
      "error: functions may not be overloaded on their return type alone"_sv, ctx.allocator);
    error.extended_diagnostic = format_diagnostic_location(ctx.allocator, identifier1);
    error.extended_diagnostic += '\n';
    print_source_snippet(ctx, error.extended_diagnostic, source1, identifier1);
    error.extended_diagnostic +=
      anton::format(ctx.allocator, u8"overload with return type '{}' defined here\n"_sv,
                    get_source_bit(source1, return1));
    anton::String_View const source2 = ctx.find_source(identifier2.source_path)->data;
    error.extended_diagnostic += format_diagnostic_location(ctx.allocator, identifier2);
    error.extended_diagnostic += '\n';
    print_source_snippet(ctx, error.extended_diagnostic, source2, identifier2);
    error.extended_diagnostic += anton::format(
      ctx.allocator,
      u8"overload with a different return type '{}', but identical parameters defined here\n"_sv,
      get_source_bit(source2, return2));
    return error;
  }

  Error err_immutable_variable_missing_initializer(Context const& ctx, Source_Info const& constant)
  {
    Error error = error_from_source(ctx.allocator, constant);
    anton::String_View const source = ctx.find_source(constant.source_path)->data;
    error.diagnostic = "error: immutable variable is missing an initializer"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, constant);
    error.extended_diagnostic += " an immutable variable must be initialized"_sv;
    return error;
  }

  anton::String format_variable_declaration_in_global_scope(Context const& ctx,
                                                            Source_Info const& declaration)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, declaration);
    message += u8"error: illegal declaration of a variable in global scope\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const& source = ctx.find_source(declaration.source_path)->data;
      print_source_snippet(ctx, message, source, declaration);
      message += '\n';
    }
    return message;
  }

  // Error err_immutable_variable_missing_initializer(Context const& ctx, Source_Info const& constant) {
  //     anton::String message = format_diagnostic_location(ctx.allocator, constant);
  //     message += u8"error: missing constant initializer\n"_sv;
  //     if(ctx.diagnostics.extended) {
  //         anton::String_View const source = ctx.find_source(constant.source_path)->data;
  //         print_source_snippet(ctx, message, source, constant);
  //         message += '\n';
  //     }
  //     return message;
  // }

  anton::String format_expression_not_implicitly_convertible_to_bool(Context const& ctx,
                                                                     Source_Info const& expression)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, expression);
    message += u8"error: expression is not implicitly convertible to bool\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const source = ctx.find_source(expression.source_path)->data;
      print_source_snippet(ctx, message, source, expression);
      message += '\n';
    }
    return message;
  }

  anton::String format_illegal_image_layout_qualifier_on_non_sourced_parameter(
    Context const& ctx, Source_Info const& qualifier, Source_Info const& parameter_identifier)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, qualifier);
    anton::String_View const source = ctx.find_source(qualifier.source_path)->data;
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
    anton::String message = format_diagnostic_location(ctx.allocator, qualifier);
    anton::String_View const source = ctx.find_source(qualifier.source_path)->data;
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

  anton::String format_missing_vertex_stage_error([[maybe_unused]] Context const& ctx,
                                                  anton::String const& pass_name)
  {
    return anton::format(ctx.allocator, u8"error: missing vertex stage in pass '{}'\n"_sv,
                         pass_name);
  }

  anton::String format_graphics_and_compute_stages([[maybe_unused]] Context const& ctx,
                                                   anton::String const& pass_name)
  {
    return anton::format(
      ctx.allocator,
      u8"error: pass must have either compute or graphics (vertex, fragment) stages. '{}' has both\n"_sv,
      pass_name);
  }

  Error err_stage_return_must_be_builtin_or_struct(Context const& ctx,
                                                   anton::String_View const pass_name,
                                                   Source_Info const& stage,
                                                   Source_Info const& return_type)
  {
    Error error = error_from_source(ctx.allocator, return_type);
    anton::String_View const source = ctx.find_source(return_type.source_path)->data;
    anton::String_View const stage_str = get_source_bit(source, stage);
    error.diagnostic = anton::format(
      ctx.allocator, "error: the return type of the {} stage of '{}' is not a builtin or struct"_sv,
      stage_str, pass_name);
    print_source_snippet(ctx, error.extended_diagnostic, source, return_type);
    error.extended_diagnostic += " return type must be a builtin or struct"_sv;
    return error;
  }

  Error err_compute_return_must_be_void(Context const& ctx, anton::String_View const pass_name,
                                        Source_Info const& return_type)
  {
    Error error = error_from_source(ctx.allocator, return_type);
    error.diagnostic += anton::format(
      ctx.allocator, "error: the return type of the compute stage of '{}' must be void\n"_sv,
      pass_name);
    anton::String_View const source = ctx.find_source(return_type.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, return_type);
    error.extended_diagnostic += " return type must be void"_sv;
    return error;
  }

  Error err_duplicate_attribute(Context const& ctx, Source_Info const& old_attr,
                                Source_Info const& new_attr)
  {
    Error error = error_from_source(ctx.allocator, new_attr);
    anton::String_View const source = ctx.find_source(new_attr.source_path)->data;
    error.diagnostic = anton::format(ctx.allocator, "error: duplicate attribute '{}'"_sv,
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
    anton::String_View const source = ctx.find_source(attr.source_path)->data;
    error.diagnostic = anton::format(ctx.allocator, "error: illegal attribute '{}'"_sv,
                                     get_source_bit(source, attr));
    print_source_snippet(ctx, error.extended_diagnostic, source, attr);
    error.extended_diagnostic += " attribute not allowed"_sv;
    return error;
  }

  Error err_empty_struct(Context const& ctx, Source_Info const& struct_name)
  {
    Error error = error_from_source(ctx.allocator, struct_name);
    anton::String_View const source = ctx.find_source(struct_name.source_path)->data;
    anton::String_View const name = get_source_bit(source, struct_name);
    error.diagnostic = anton::format(
      ctx.allocator, "error: structs must have at least one member, but '{}' is empty"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, struct_name);
    error.extended_diagnostic += " defined here with an empty body"_sv;
    return error;
  }

  Error err_duplicate_struct_member(Context const& ctx, Source_Info const& first_member_name,
                                    Source_Info const& second_member_name)
  {
    Error error = error_from_source(ctx.allocator, second_member_name);
    anton::String_View const source = ctx.find_source(second_member_name.source_path)->data;
    anton::String_View const name = get_source_bit(source, second_member_name);
    error.diagnostic = anton::format(ctx.allocator, "error: duplicate member '{}'"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, first_member_name);
    error.extended_diagnostic += " defined here\n"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, second_member_name);
    error.extended_diagnostic += " duplicated here"_sv;
    return error;
  }

  Error err_opaque_type_in_struct(Context const& ctx, Source_Info const& type)
  {
    Error error = error_from_source(ctx.allocator, type);
    anton::String_View const source = ctx.find_source(type.source_path)->data;
    anton::String_View const name = get_source_bit(source, type);
    error.diagnostic = anton::format(
      ctx.allocator, "error: opaque type '{}' may not be used inside struct"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, type);
    error.extended_diagnostic += " opaque type used\n"_sv;
    return error;
  }

  Error err_recursive_type_definition(Context const& ctx, Source_Info const& struct_name,
                                      Source_Info const& type)
  {
    Error error = error_from_source(ctx.allocator, type);
    anton::String_View const source = ctx.find_source(struct_name.source_path)->data;
    anton::String_View const name = get_source_bit(source, struct_name);
    error.diagnostic =
      anton::format(ctx.allocator, "error: recursively defined type '{}'"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, type);
    error.extended_diagnostic +=
      anton::format(ctx.allocator, " '{}' used within its own definition\n"_sv, name);
    return error;
  }

  Error err_source_import_failed(Context const& ctx, Source_Info const& import_info,
                                 anton::String_View const source_callback_message)
  {
    Error error = error_from_source(ctx.allocator, import_info);
    error.diagnostic =
      anton::String("error: source import failed with the following error: "_sv, ctx.allocator);
    error.diagnostic += source_callback_message;
    anton::String_View const source = ctx.find_source(import_info.source_path)->data;
    print_source_snippet(ctx, error.extended_diagnostic, source, import_info);
    return error;
  }

  Error err_source_import_failed_no_location(Context const& ctx,
                                             anton::String_View const source_callback_message)
  {
    Error error;
    error.line = 1;
    error.column = 1;
    error.end_line = 1;
    error.end_column = 1;
    error.source = anton::String("<vush>"_sv, ctx.allocator);
    error.diagnostic =
      anton::String("error: source import failed with the following error: "_sv, ctx.allocator);
    error.diagnostic += source_callback_message;
    error.extended_diagnostic = anton::String(ctx.allocator);
    return error;
  }

  Error err_duplicate_label(Context const& ctx, Source_Info const& first, Source_Info const& second)
  {
    Error error = error_from_source(ctx.allocator, second);
    anton::String_View const source = ctx.find_source(first.source_path)->data;
    anton::String_View const label = get_source_bit(source, first);
    error.diagnostic = anton::format(ctx.allocator, "error: duplicate label '{}'"_sv, label);
    print_source_snippet(ctx, error.extended_diagnostic, source, first);
    error.extended_diagnostic += " label first appeared here...\n"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, second);
    error.extended_diagnostic += " ...and then the second time here"_sv;
    return error;
  }

  anton::String format_duplicate_sourced_parameter(Context const& ctx, Source_Info const& first,
                                                   Source_Info const& first_type,
                                                   Source_Info const& second,
                                                   Source_Info const& second_type)
  {
    anton::String message = format_diagnostic_location(ctx.allocator, second);
    message += u8"error: duplicate sourced parameter name with a different type\n"_sv;
    if(ctx.diagnostics.extended) {
      anton::String_View const first_source = ctx.find_source(first.source_path)->data;
      message += format_diagnostic_location(ctx.allocator, first);
      message += u8"first definition with type '"_sv;
      message += get_source_bit(first_source, first_type);
      message += u8"' found here\n"_sv;
      print_source_snippet(ctx, message, first_source, first);
      message += '\n';
      anton::String_View const second_source = ctx.find_source(second.source_path)->data;
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
  //         anton::String_View const first_source = ctx.find_source(first.source_path)->data;
  //         message += format_diagnostic_location(ctx.allocator, first);
  //         message += u8"first occurence of 'default' found here\n"_sv;
  //         print_source_snippet(ctx, message, first_source, first);
  //         message += '\n';
  //         anton::String_View const second_source = ctx.find_source(second.source_path)->data;
  //         message += format_diagnostic_location(ctx.allocator, second);
  //         message += u8"second occurence of 'default' found here\n"_sv;
  //         print_source_snippet(ctx, message, second_source, second);
  //         message += '\n';
  //     }
  //     return message;
  // }

  // Error err_duplicate_label(Context const& ctx, Source_Info const& first, Source_Info const& second) {
  //     anton::String_View const second_source = ctx.find_source(second.source_path)->data;
  //     an*ton::String message = format_diagnostic_location(ctx.allocator, second);
  //     message += u8"error: duplicate '"_sv;
  //     message += get_source_bit(second_source, second);
  //     message += u8"' label in switch statement\n"_sv;
  //     if(ctx.diagnostics.extended) {
  //         anton::String_View const first_source = ctx.find_source(first.source_path)->data;
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

  Error err_identifier_is_not_a_constant(Context const& ctx, Source_Info const& identifier)
  {
    Error error = error_from_source(ctx.allocator, identifier);
    anton::String_View const source = ctx.find_source(identifier.source_path)->data;
    anton::String_View const name = get_source_bit(source, identifier);
    error.diagnostic = anton::format(ctx.allocator, "error: '{}' is not a constant"_sv, name);
    print_source_snippet(ctx, error.extended_diagnostic, source, identifier);
    error.extended_diagnostic += anton::format(ctx.allocator, " '{}' is not a constant"_sv, name);
    return error;
  }

  Error err_expression_is_not_constant_evaluable(Context const& ctx, Source_Info const& expression)
  {
    Error error = error_from_source(ctx.allocator, expression);
    anton::String_View const source = ctx.find_source(expression.source_path)->data;
    error.diagnostic = "error: expression is not constant evaluable'"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, expression);
    error.extended_diagnostic += " not constant evaluable"_sv;
    return error;
  }

  Error err_no_matching_overload(Context const& ctx, ast::Expr_Call const* const call,
                                 anton::Slice<ast::Decl_Function const* const> const overloads)
  {
    Error error = error_from_source(ctx.allocator, call->source_info);
    // Stringify the types of the arguments of the call.
    anton::String arguments(ctx.allocator);
    arguments += "("_sv;
    for(bool first = true; ast::Expr const* const argument: call->arguments) {
      ast::Type const* const type = ctx.find_node_type(argument);
      ANTON_ASSERT(type != nullptr, "argument does not have type");
      if(!first) {
        arguments += ", "_sv;
      }
      first = false;
      arguments += stringify_type(ctx, type);
    }
    arguments += ")"_sv;

    error.diagnostic =
      anton::format("error: no matching function for call to '{}' with arguments '{}'"_sv,
                    call->identifier->value, arguments);
    for(bool first = true; ast::Decl_Function const* const fn: overloads) {
      if(!first) {
        error.extended_diagnostic += u8'\n';
      }

      first = false;

      auto result = ctx.find_source(fn->source_info.source_path);
      if(result != nullptr) {
        ANTON_ASSERT(!fn->builtin, "builtin function has source");
        anton::String_View const source = result->data;
        print_source_snippet(ctx, error.extended_diagnostic, source, call->source_info);
        error.extended_diagnostic += " candidate function not viable"_sv;
      } else {
        ANTON_ASSERT(fn->builtin, "non-builtin function has no source");
        error.extended_diagnostic += format_diagnostic_location(ctx.allocator, "<vush>", 1, 1);
        error.extended_diagnostic += "note: builtin function is not a viable candidate\n"_sv;
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
        error.extended_diagnostic += '\n';
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
        error.extended_diagnostic += stringify_builtin_function(ctx, fn);
        error.extended_diagnostic += '\n';
        print_left_margin(ctx.allocator, error.extended_diagnostic, 0);
      }
    }
    return error;
  }

  Error err_init_invalid_struct_initializer_kind(Context const& ctx,
                                                 ast::Initializer const* const initializer)
  {
    Source_Info const source_info = initializer->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source = ctx.find_source(source_info.source_path)->data;
    error.diagnostic = "error: initializer is not named initializer"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic += " struct initializers must be named initializers"_sv;
    return error;
  }

  Error err_type_has_no_field_named(Context const& ctx, ast::Type const* type,
                                    ast::Identifier const* field_identifier)
  {
    Source_Info const field_source_info = field_identifier->source_info;
    Source_Info const type_source_info = type->source_info;
    Error error = error_from_source(ctx.allocator, field_source_info);
    anton::String_View const type_source = ctx.find_source(type_source_info.source_path)->data;
    anton::String_View const type_value = get_source_bit(type_source, type_source_info);
    anton::String_View const field_source = ctx.find_source(field_source_info.source_path)->data;
    anton::String_View const field_value = get_source_bit(field_source, field_source_info);
    error.diagnostic =
      anton::format("error: '{}' does not have field '{}'"_sv, type_value, field_value);
    print_source_snippet(ctx, error.extended_diagnostic, field_source, field_source_info);
    return error;
  }

  Error err_vector_swizzle_invalid(Context const& ctx, ast::Identifier const* field)
  {
    Source_Info const source_info = field->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source = ctx.find_source(source_info.source_path)->data;
    anton::String_View const field_code = get_source_bit(source, source_info);
    error.diagnostic = anton::format("error: invalid vector swizzle '{}'"_sv, field_code);
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic +=
      " vector swizzle must contain at most 4 of { x, y, z, w, r, g, b, a, s, t, u, v }"_sv;
    return error;
  }

  Error err_vector_swizzle_overlong(Context const& ctx, ast::Type_Builtin const* type,
                                    ast::Identifier const* field)
  {
    Source_Info const source_info = field->source_info;
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source = ctx.find_source(source_info.source_path)->data;
    anton::String_View const field_code = get_source_bit(source, source_info);
    error.diagnostic =
      anton::format("error: vector swizzle '{}' overlong for type '{}'"_sv, field_code, ""_sv);
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    return error;
  }

  Error err_unimplemented(Context const& ctx, Source_Info const& source_info)
  {
    Error error = error_from_source(ctx.allocator, source_info);
    anton::String_View const source = ctx.find_source(source_info.source_path)->data;
    error.diagnostic = "error: unimplemented"_sv;
    print_source_snippet(ctx, error.extended_diagnostic, source, source_info);
    error.extended_diagnostic += " resulted in the compiler to reach an unimplemented path"_sv;
    return error;
  }

  Error err_cannot_convert_type(Context const& ctx, Source_Info const& where, ast::Type const* to,
                                ast::Type const* from)
  {
    Error error = error_from_source(ctx.allocator, where);
    anton::String_View const source = ctx.find_source(where.source_path)->data;
    anton::String from_string = stringify_type(ctx, from);
    anton::String to_string = stringify_type(ctx, to);
    error.diagnostic =
      anton::format("error: cannot convert '{}' to '{}'"_sv, from_string, to_string);
    print_source_snippet(ctx, error.extended_diagnostic, source, where);
    error.extended_diagnostic += anton::format(" '{}' cannot be converted"_sv, from_string);
    return error;
  }
} // namespace vush
