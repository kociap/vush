#include <vush_ast/ast.hpp>

#include <anton/intrinsics.hpp>

#include <vush_core/memory.hpp>

namespace vush::ast {
  using namespace anton::literals;

  template<>
  bool instanceof<Expr_Field>(Node const& node)
  {
    return node.node_kind == Node_Kind::expr_field;
  }

  template<>
  bool instanceof<Basic_Initializer>(Node const& node)
  {
    return node.node_kind == Node_Kind::basic_initializer;
  }

  template<>
  bool instanceof<Field_Initializer>(Node const& node)
  {
    return node.node_kind == Node_Kind::field_initializer;
  }

  template<>
  bool instanceof<Index_Initializer>(Node const& node)
  {
    return node.node_kind == Node_Kind::index_initializer;
  }

  bool is_sourced_parameter(Fn_Parameter const& parameter)
  {
    return parameter.source.value.size_bytes() != 0;
  }

  bool is_input_parameter(Fn_Parameter const& parameter)
  {
    return parameter.source.value == "in"_sv;
  }

  bool is_output_parameter(Fn_Parameter const& parameter)
  {
    return parameter.source.value == "out"_sv;
  }

  bool is_image_parameter(Fn_Parameter const& parameter)
  {
    return parameter.source.value == "images"_sv;
  }

  anton::Optional<Type_Builtin_Kind>
  enumify_builtin_type_kind(anton::String_View const type)
  {
    static constexpr anton::String_View builtin_types_strings[] = {
      "void",
      "bool",
      "int",
      "uint",
      "float",
      "double",
      "vec2",
      "vec3",
      "vec4",
      "dvec2",
      "dvec3",
      "dvec4",
      "bvec2",
      "bvec3",
      "bvec4",
      "ivec2",
      "ivec3",
      "ivec4",
      "uvec2",
      "uvec3",
      "uvec4",
      "mat2",
      "mat2x2",
      "mat3",
      "mat3x3",
      "mat4",
      "mat4x4",
      "mat2x3",
      "mat2x4",
      "mat3x2",
      "mat3x4",
      "mat4x2",
      "mat4x3",
      "dmat2",
      "dmat2x2",
      "dmat3",
      "dmat3x3",
      "dmat4",
      "dmat4x4",
      "dmat2x3",
      "dmat2x4",
      "dmat3x2",
      "dmat3x4",
      "dmat4x2",
      "dmat4x3",
      "sampler1D",
      "texture1D",
      "image1D",
      "sampler1DShadow",
      "sampler1DArray",
      "texture1DArray",
      "image1DArray",
      "sampler1DArrayShadow",
      "sampler2D",
      "texture2D",
      "image2D",
      "sampler2DShadow",
      "sampler2DArray",
      "texture2DArray",
      "image2DArray",
      "sampler2DArrayShadow",
      "sampler2DMS",
      "texture2DMS",
      "image2DMS",
      "sampler2DMSArray",
      "texture2DMSArray",
      "image2DMSArray",
      "sampler3D",
      "texture3D",
      "image3D",
      "samplerCube",
      "textureCube",
      "imageCube",
      "samplerCubeShadow",
      "samplerCubeArray",
      "textureCubeArray",
      "imageCubeArray",
      "samplerCubeArrayShadow",
      "samplerBuffer",
      "textureBuffer",
      "imageBuffer",
      "subpassInput",
      "subpassInputMS",
      "isampler1D",
      "itexture1D",
      "iimage1D",
      "isampler1DArray",
      "itexture1DArray",
      "iimage1DArray",
      "isampler2D",
      "itexture2D",
      "iimage2D",
      "isampler2DArray",
      "itexture2DArray",
      "iimage2DArray",
      "isampler2DMS",
      "itexture2DMS",
      "iimage2DMS",
      "isampler2DMSArray",
      "itexture2DMSArray",
      "iimage2DMSArray",
      "isampler3D",
      "itexture3D",
      "iimage3D",
      "isamplerCube",
      "itextureCube",
      "iimageCube",
      "isamplerCubeArray",
      "itextureCubeArray",
      "iimageCubeArray",
      "isamplerBuffer",
      "itextureBuffer",
      "iimageBuffer",
      "isubpassInput",
      "isubpassInputMS",
      "usampler1D",
      "utexture1D",
      "uimage1D",
      "usampler1DArray",
      "utexture1DArray",
      "uimage1DArray",
      "usampler2D",
      "utexture2D",
      "uimage2D",
      "usampler2DArray",
      "utexture2DArray",
      "uimage2DArray",
      "usampler2DMS",
      "utexture2DMS",
      "uimage2DMS",
      "usampler2DMSArray",
      "utexture2DMSArray",
      "uimage2DMSArray",
      "usampler3D",
      "utexture3D",
      "uimage3D",
      "usamplerCube",
      "utextureCube",
      "uimageCube",
      "usamplerCubeArray",
      "utextureCubeArray",
      "uimageCubeArray",
      "usamplerBuffer",
      "utextureBuffer",
      "uimageBuffer",
      "usubpassInput",
      "usubpassInputMS",
      "sampler",
    };

    static constexpr Type_Builtin_Kind builtin_types[] = {
      Type_Builtin_Kind::e_void,
      Type_Builtin_Kind::e_bool,
      Type_Builtin_Kind::e_int,
      Type_Builtin_Kind::e_uint,
      Type_Builtin_Kind::e_float,
      Type_Builtin_Kind::e_double,
      Type_Builtin_Kind::e_vec2,
      Type_Builtin_Kind::e_vec3,
      Type_Builtin_Kind::e_vec4,
      Type_Builtin_Kind::e_dvec2,
      Type_Builtin_Kind::e_dvec3,
      Type_Builtin_Kind::e_dvec4,
      Type_Builtin_Kind::e_bvec2,
      Type_Builtin_Kind::e_bvec3,
      Type_Builtin_Kind::e_bvec4,
      Type_Builtin_Kind::e_ivec2,
      Type_Builtin_Kind::e_ivec3,
      Type_Builtin_Kind::e_ivec4,
      Type_Builtin_Kind::e_uvec2,
      Type_Builtin_Kind::e_uvec3,
      Type_Builtin_Kind::e_uvec4,
      Type_Builtin_Kind::e_mat2,
      Type_Builtin_Kind::e_mat2,
      Type_Builtin_Kind::e_mat3,
      Type_Builtin_Kind::e_mat3,
      Type_Builtin_Kind::e_mat4,
      Type_Builtin_Kind::e_mat4,
      Type_Builtin_Kind::e_mat2x3,
      Type_Builtin_Kind::e_mat2x4,
      Type_Builtin_Kind::e_mat3x2,
      Type_Builtin_Kind::e_mat3x4,
      Type_Builtin_Kind::e_mat4x2,
      Type_Builtin_Kind::e_mat4x3,
      Type_Builtin_Kind::e_dmat2,
      Type_Builtin_Kind::e_dmat2,
      Type_Builtin_Kind::e_dmat3,
      Type_Builtin_Kind::e_dmat3,
      Type_Builtin_Kind::e_dmat4,
      Type_Builtin_Kind::e_dmat4,
      Type_Builtin_Kind::e_dmat2x3,
      Type_Builtin_Kind::e_dmat2x4,
      Type_Builtin_Kind::e_dmat3x2,
      Type_Builtin_Kind::e_dmat3x4,
      Type_Builtin_Kind::e_dmat4x2,
      Type_Builtin_Kind::e_dmat4x3,
      Type_Builtin_Kind::e_sampler1D,
      Type_Builtin_Kind::e_texture1D,
      Type_Builtin_Kind::e_image1D,
      Type_Builtin_Kind::e_sampler1DShadow,
      Type_Builtin_Kind::e_sampler1DArray,
      Type_Builtin_Kind::e_texture1DArray,
      Type_Builtin_Kind::e_image1DArray,
      Type_Builtin_Kind::e_sampler1DArrayShadow,
      Type_Builtin_Kind::e_sampler2D,
      Type_Builtin_Kind::e_texture2D,
      Type_Builtin_Kind::e_image2D,
      Type_Builtin_Kind::e_sampler2DShadow,
      Type_Builtin_Kind::e_sampler2DArray,
      Type_Builtin_Kind::e_texture2DArray,
      Type_Builtin_Kind::e_image2DArray,
      Type_Builtin_Kind::e_sampler2DArrayShadow,
      Type_Builtin_Kind::e_sampler2DMS,
      Type_Builtin_Kind::e_texture2DMS,
      Type_Builtin_Kind::e_image2DMS,
      Type_Builtin_Kind::e_sampler2DMSArray,
      Type_Builtin_Kind::e_texture2DMSArray,
      Type_Builtin_Kind::e_image2DMSArray,
      Type_Builtin_Kind::e_sampler3D,
      Type_Builtin_Kind::e_texture3D,
      Type_Builtin_Kind::e_image3D,
      Type_Builtin_Kind::e_samplerCube,
      Type_Builtin_Kind::e_textureCube,
      Type_Builtin_Kind::e_imageCube,
      Type_Builtin_Kind::e_samplerCubeShadow,
      Type_Builtin_Kind::e_samplerCubeArray,
      Type_Builtin_Kind::e_textureCubeArray,
      Type_Builtin_Kind::e_imageCubeArray,
      Type_Builtin_Kind::e_samplerCubeArrayShadow,
      Type_Builtin_Kind::e_samplerBuffer,
      Type_Builtin_Kind::e_textureBuffer,
      Type_Builtin_Kind::e_imageBuffer,
      Type_Builtin_Kind::e_subpassInput,
      Type_Builtin_Kind::e_subpassInputMS,
      Type_Builtin_Kind::e_isampler1D,
      Type_Builtin_Kind::e_itexture1D,
      Type_Builtin_Kind::e_iimage1D,
      Type_Builtin_Kind::e_isampler1DArray,
      Type_Builtin_Kind::e_itexture1DArray,
      Type_Builtin_Kind::e_iimage1DArray,
      Type_Builtin_Kind::e_isampler2D,
      Type_Builtin_Kind::e_itexture2D,
      Type_Builtin_Kind::e_iimage2D,
      Type_Builtin_Kind::e_isampler2DArray,
      Type_Builtin_Kind::e_itexture2DArray,
      Type_Builtin_Kind::e_iimage2DArray,
      Type_Builtin_Kind::e_isampler2DMS,
      Type_Builtin_Kind::e_itexture2DMS,
      Type_Builtin_Kind::e_iimage2DMS,
      Type_Builtin_Kind::e_isampler2DMSArray,
      Type_Builtin_Kind::e_itexture2DMSArray,
      Type_Builtin_Kind::e_iimage2DMSArray,
      Type_Builtin_Kind::e_isampler3D,
      Type_Builtin_Kind::e_itexture3D,
      Type_Builtin_Kind::e_iimage3D,
      Type_Builtin_Kind::e_isamplerCube,
      Type_Builtin_Kind::e_itextureCube,
      Type_Builtin_Kind::e_iimageCube,
      Type_Builtin_Kind::e_isamplerCubeArray,
      Type_Builtin_Kind::e_itextureCubeArray,
      Type_Builtin_Kind::e_iimageCubeArray,
      Type_Builtin_Kind::e_isamplerBuffer,
      Type_Builtin_Kind::e_itextureBuffer,
      Type_Builtin_Kind::e_iimageBuffer,
      Type_Builtin_Kind::e_isubpassInput,
      Type_Builtin_Kind::e_isubpassInputMS,
      Type_Builtin_Kind::e_usampler1D,
      Type_Builtin_Kind::e_utexture1D,
      Type_Builtin_Kind::e_uimage1D,
      Type_Builtin_Kind::e_usampler1DArray,
      Type_Builtin_Kind::e_utexture1DArray,
      Type_Builtin_Kind::e_uimage1DArray,
      Type_Builtin_Kind::e_usampler2D,
      Type_Builtin_Kind::e_utexture2D,
      Type_Builtin_Kind::e_uimage2D,
      Type_Builtin_Kind::e_usampler2DArray,
      Type_Builtin_Kind::e_utexture2DArray,
      Type_Builtin_Kind::e_uimage2DArray,
      Type_Builtin_Kind::e_usampler2DMS,
      Type_Builtin_Kind::e_utexture2DMS,
      Type_Builtin_Kind::e_uimage2DMS,
      Type_Builtin_Kind::e_usampler2DMSArray,
      Type_Builtin_Kind::e_utexture2DMSArray,
      Type_Builtin_Kind::e_uimage2DMSArray,
      Type_Builtin_Kind::e_usampler3D,
      Type_Builtin_Kind::e_utexture3D,
      Type_Builtin_Kind::e_uimage3D,
      Type_Builtin_Kind::e_usamplerCube,
      Type_Builtin_Kind::e_utextureCube,
      Type_Builtin_Kind::e_uimageCube,
      Type_Builtin_Kind::e_usamplerCubeArray,
      Type_Builtin_Kind::e_utextureCubeArray,
      Type_Builtin_Kind::e_uimageCubeArray,
      Type_Builtin_Kind::e_usamplerBuffer,
      Type_Builtin_Kind::e_utextureBuffer,
      Type_Builtin_Kind::e_uimageBuffer,
      Type_Builtin_Kind::e_usubpassInput,
      Type_Builtin_Kind::e_usubpassInputMS,
      Type_Builtin_Kind::e_sampler,
    };

    constexpr i64 array_size =
      sizeof(builtin_types_strings) / sizeof(anton::String_View);
    for(i64 i = 0; i < array_size; ++i) {
      if(type == builtin_types_strings[i]) {
        return builtin_types[i];
      }
    }

    return anton::null_optional;
  }

  bool compare_types_equal(Type const& lhs, Type const& rhs)
  {
    if(lhs.type_kind != rhs.type_kind) {
      return false;
    }

    Type_Kind const kind = lhs.type_kind;
    switch(kind) {
    case Type_Kind::type_builtin: {
      Type_Builtin const& lhs_v = static_cast<Type_Builtin const&>(lhs);
      Type_Builtin const& rhs_v = static_cast<Type_Builtin const&>(rhs);
      return lhs_v.value == rhs_v.value;
    }

    case Type_Kind::type_struct: {
      Type_Struct const& lhs_v = static_cast<Type_Struct const&>(lhs);
      Type_Struct const& rhs_v = static_cast<Type_Struct const&>(rhs);
      return lhs_v.value == rhs_v.value;
    }

    case Type_Kind::type_array: {
      Type_Array const& lhs_v = static_cast<Type_Array const&>(lhs);
      Type_Array const& rhs_v = static_cast<Type_Array const&>(rhs);
      if(!compare_types_equal(*lhs_v.base, *rhs_v.base)) {
        return false;
      }

      // Both unsized are the sme types.
      if(!lhs_v.size && !rhs_v.size) {
        return true;
      }

      // One sized and one unsized are different types.
      if((lhs_v.size && !rhs_v.size) || (!lhs_v.size && rhs_v.size)) {
        return false;
      }

      // Otherwise compare the sizes to determine whether types are equal.
      return compare_integer_literals(*lhs_v.size, *rhs_v.size) ==
             anton::Strong_Ordering::equal;
    }
    }
  }

  bool is_buffer(Decl_Buffer const* buffer)
  {
    for(Attribute const& attribute: buffer->attributes) {
      anton::String_View identifier = attribute.identifier.value;
      if(identifier == "push_constant"_sv || identifier == "uniform"_sv) {
        return false;
      }
    }
    return true;
  }

  bool is_uniform(Decl_Buffer const* buffer)
  {
    for(Attribute const& attribute: buffer->attributes) {
      if(attribute.identifier.value == "uniform"_sv) {
        return true;
      }
    }
    return false;
  }

  bool is_push_constant(Decl_Buffer const* buffer)
  {
    for(Attribute const& attribute: buffer->attributes) {
      if(attribute.identifier.value == "push_constant"_sv) {
        return true;
      }
    }
    return false;
  }

  i32 vector_swizzle_char_to_index(char8 const c)
  {
    // Swizzles:
    // xyzw
    // rgba
    // stuv
    switch(c) {
    case 'x':
    case 'r':
    case 's':
      return 0;

    case 'y':
    case 'g':
    case 't':
      return 1;

    case 'z':
    case 'b':
    case 'u':
      return 2;

    case 'w':
    case 'a':
    case 'v':
      return 3;

    default:
      ANTON_ASSERT(false, "invalid swizzle char");
      return -1;
    }
  }

  u32 get_lt_integer_value_as_u32(Lt_Integer const& integer)
  {
    switch(integer.kind) {
    case Lt_Integer_Kind::i32:
      return integer.i32_value;

    case Lt_Integer_Kind::u32:
      return integer.u32_value;
    }
  }

  [[nodiscard]] static anton::Strong_Ordering
  compare_integers_strong(u32 const lhs, u32 const rhs)
  {
    if(lhs < rhs) {
      return anton::Strong_Ordering::less;
    } else if(lhs == rhs) {
      return anton::Strong_Ordering::equal;
    } else {
      return anton::Strong_Ordering::greater;
    }
  }

  anton::Strong_Ordering compare_integer_literals(Lt_Integer const& lhs,
                                                  Lt_Integer const& rhs)
  {
    u32 const lhs_value = get_lt_integer_value_as_u32(lhs);
    u32 const rhs_value = get_lt_integer_value_as_u32(rhs);
    return compare_integers_strong(lhs_value, rhs_value);
  }

  bool is_assignment_arithmetic(Stmt_Assignment const* assignment)
  {
    return assignment->kind != Assignment_Kind::e_assign;
  }

} // namespace vush::ast
