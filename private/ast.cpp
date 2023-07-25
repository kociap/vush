#include <ast.hpp>

#include <anton/intrinsics.hpp>
#include <memory.hpp>

namespace vush {
  using namespace anton::literals;

  Syntax_Token::Syntax_Token(Syntax_Node_Kind kind, anton::String value,
                             Source_Info const& source_info)
    : value(ANTON_MOV(value)), source_info(source_info), kind(kind)
  {
  }

  Syntax_Node::Syntax_Node(Syntax_Node_Kind kind, Array<SNOT> array, Source_Info const& source_info)
    : children(ANTON_MOV(array)), source_info(source_info), kind(kind)
  {
  }

  namespace ast {
    bool is_integer(Type const& type)
    {
      Type_Builtin const& builtin = static_cast<Type_Builtin const&>(type);
      return type.node_kind == Node_Kind::type_builtin &&
             (builtin.value == Type_Builtin_Kind::glsl_int ||
              builtin.value == Type_Builtin_Kind::glsl_uint);
    }

    bool is_void(Type const& type)
    {
      return type.node_kind == Node_Kind::type_builtin &&
             static_cast<Type_Builtin const&>(type).value == Type_Builtin_Kind::glsl_void;
    }

    bool is_vector(Type const& type)
    {
      if(type.node_kind != Node_Kind::type_builtin) {
        return false;
      }

      Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
      return v == Type_Builtin_Kind::glsl_vec2 || v == Type_Builtin_Kind::glsl_vec3 ||
             v == Type_Builtin_Kind::glsl_vec4 || v == Type_Builtin_Kind::glsl_dvec2 ||
             v == Type_Builtin_Kind::glsl_dvec3 || v == Type_Builtin_Kind::glsl_dvec4 ||
             v == Type_Builtin_Kind::glsl_bvec2 || v == Type_Builtin_Kind::glsl_bvec3 ||
             v == Type_Builtin_Kind::glsl_bvec4 || v == Type_Builtin_Kind::glsl_ivec2 ||
             v == Type_Builtin_Kind::glsl_ivec3 || v == Type_Builtin_Kind::glsl_ivec4 ||
             v == Type_Builtin_Kind::glsl_uvec2 || v == Type_Builtin_Kind::glsl_uvec3 ||
             v == Type_Builtin_Kind::glsl_uvec4;
    }

    bool is_matrix(Type const& type)
    {
      if(type.node_kind != Node_Kind::type_builtin) {
        return false;
      }

      Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
      return v == Type_Builtin_Kind::glsl_mat2 || v == Type_Builtin_Kind::glsl_mat3 ||
             v == Type_Builtin_Kind::glsl_mat4 || v == Type_Builtin_Kind::glsl_mat2x3 ||
             v == Type_Builtin_Kind::glsl_mat2x4 || v == Type_Builtin_Kind::glsl_mat3x2 ||
             v == Type_Builtin_Kind::glsl_mat3x4 || v == Type_Builtin_Kind::glsl_mat4x2 ||
             v == Type_Builtin_Kind::glsl_mat4x3 || v == Type_Builtin_Kind::glsl_dmat2 ||
             v == Type_Builtin_Kind::glsl_dmat3 || v == Type_Builtin_Kind::glsl_dmat4 ||
             v == Type_Builtin_Kind::glsl_dmat2x3 || v == Type_Builtin_Kind::glsl_dmat2x4 ||
             v == Type_Builtin_Kind::glsl_dmat3x2 || v == Type_Builtin_Kind::glsl_dmat3x4 ||
             v == Type_Builtin_Kind::glsl_dmat4x2 || v == Type_Builtin_Kind::glsl_dmat4x3;
    }

    bool is_opaque_type(Type const& type)
    {
      ANTON_ASSERT(type.node_kind == Node_Kind::type_builtin ||
                     type.node_kind == Node_Kind::type_user_defined ||
                     type.node_kind == Node_Kind::type_array,
                   u8"unknown ast node type");
      if(type.node_kind == Node_Kind::type_builtin) {
        Type_Builtin const& t = static_cast<Type_Builtin const&>(type);
        return is_opaque_builtin_type_kind(t.value);
      } else if(type.node_kind == Node_Kind::type_user_defined) {
        return false;
      } else {
        Type_Array const& t = static_cast<Type_Array const&>(type);
        return is_opaque_type(*t.base);
      }
    }

    bool is_array(Type const& type)
    {
      return type.node_kind == Node_Kind::type_array;
    }

    bool is_sized_array(Type const& type)
    {
      return type.node_kind == Node_Kind::type_array && static_cast<Type_Array const&>(type).size;
    }

    bool is_unsized_array(Type const& type)
    {
      return type.node_kind == Node_Kind::type_array && !static_cast<Type_Array const&>(type).size;
    }

    bool is_image_type(Type const& type)
    {
      ANTON_ASSERT(type.node_kind == Node_Kind::type_builtin ||
                     type.node_kind == Node_Kind::type_user_defined ||
                     type.node_kind == Node_Kind::type_array,
                   u8"unknown ast node type");

      if(type.node_kind == Node_Kind::type_user_defined) {
        return false;
      }

      if(type.node_kind == Node_Kind::type_builtin) {
        Type_Builtin const& t = (Type_Builtin const&)type;
        return is_image_builtin_type_kind(t.value);
      }

      Type_Array const& t = (Type_Array const&)type;
      return is_image_type(*t.base);
    }

    // anton::String stringify_type(Allocator* const allocator, Type const& type) {
    //     ANTON_ASSERT(type.node_kind == Node_Kind::type_builtin || type.node_kind == Node_Kind::type_user_defined || type.node_kind == Node_Kind::type_array,
    //                  u8"unknown ast node type");
    //     if(type.node_kind == Node_Kind::type_builtin) {
    //         Type_Builtin const& t = static_cast<Type_Builtin const&>(type);
    //         anton::String_View sv = stringify_builtin_type_kind(t.value);
    //         return anton::String(sv, allocator);
    //     } else if(type.node_kind == Node_Kind::type_user_defined) {
    //         Type_User_Defined const& t = static_cast<Type_User_Defined const&>(type);
    //         return anton::String(t.value, allocator);
    //     } else {
    //         Type_Array const& t = static_cast<Type_Array const&>(type);
    //         anton::String str = stringify_type(allocator, *t.base);
    //         str += u8"[";
    //         if(t.size) {
    //             str += t.size->value;
    //         }
    //         str += u8"]";
    //         return str;
    //     }
    // }

    bool is_opaque_builtin_type_kind(Type_Builtin_Kind const type)
    {
      return static_cast<i32>(type) >= static_cast<i32>(Type_Builtin_Kind::glsl_sampler1D);
    }

    bool is_image_builtin_type_kind(Type_Builtin_Kind const type)
    {
      return type == Type_Builtin_Kind::glsl_image1D ||
             type == Type_Builtin_Kind::glsl_image1DArray ||
             type == Type_Builtin_Kind::glsl_image2D ||
             type == Type_Builtin_Kind::glsl_image2DArray ||
             type == Type_Builtin_Kind::glsl_image2DMS ||
             type == Type_Builtin_Kind::glsl_image2DMSArray ||
             type == Type_Builtin_Kind::glsl_image2DRect ||
             type == Type_Builtin_Kind::glsl_image3D || type == Type_Builtin_Kind::glsl_imageCube ||
             type == Type_Builtin_Kind::glsl_imageCubeArray ||
             type == Type_Builtin_Kind::glsl_imageBuffer ||
             type == Type_Builtin_Kind::glsl_iimage1D ||
             type == Type_Builtin_Kind::glsl_iimage1DArray ||
             type == Type_Builtin_Kind::glsl_iimage2D ||
             type == Type_Builtin_Kind::glsl_iimage2DArray ||
             type == Type_Builtin_Kind::glsl_iimage2DMS ||
             type == Type_Builtin_Kind::glsl_iimage2DMSArray ||
             type == Type_Builtin_Kind::glsl_iimage2DRect ||
             type == Type_Builtin_Kind::glsl_iimage3D ||
             type == Type_Builtin_Kind::glsl_iimageCube ||
             type == Type_Builtin_Kind::glsl_iimageCubeArray ||
             type == Type_Builtin_Kind::glsl_iimageBuffer ||
             type == Type_Builtin_Kind::glsl_uimage1D ||
             type == Type_Builtin_Kind::glsl_uimage1DArray ||
             type == Type_Builtin_Kind::glsl_uimage2D ||
             type == Type_Builtin_Kind::glsl_uimage2DArray ||
             type == Type_Builtin_Kind::glsl_uimage2DMS ||
             type == Type_Builtin_Kind::glsl_uimage2DMSArray ||
             type == Type_Builtin_Kind::glsl_uimage2DRect ||
             type == Type_Builtin_Kind::glsl_uimage3D ||
             type == Type_Builtin_Kind::glsl_uimageCube ||
             type == Type_Builtin_Kind::glsl_uimageCubeArray ||
             type == Type_Builtin_Kind::glsl_uimageBuffer;
    }

    bool is_type(Node const& node)
    {
      return node.node_kind == Node_Kind::type_builtin ||
             node.node_kind == Node_Kind::type_user_defined ||
             node.node_kind == Node_Kind::type_array;
    }

    bool is_sourced_parameter(Fn_Parameter const& parameter)
    {
      return parameter.source;
    }

    bool is_vertex_input_parameter(Fn_Parameter const& parameter)
    {
      return parameter.source && parameter.source->value == u8"in";
    }

    anton::Optional<Type_Builtin_Kind> enumify_builtin_type_kind(anton::String_View const type)
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
        "sampler2DRect",
        "texture2DRect",
        "image2DRect",
        "sampler2DRectShadow",
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
        "isampler2DRect",
        "itexture2DRect",
        "iimage2DRect",
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
        "usampler2DRect",
        "utexture2DRect",
        "uimage2DRect",
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
        "samplerShadow",
      };

      static constexpr Type_Builtin_Kind builtin_types[] = {
        Type_Builtin_Kind::glsl_void,
        Type_Builtin_Kind::glsl_bool,
        Type_Builtin_Kind::glsl_int,
        Type_Builtin_Kind::glsl_uint,
        Type_Builtin_Kind::glsl_float,
        Type_Builtin_Kind::glsl_double,
        Type_Builtin_Kind::glsl_vec2,
        Type_Builtin_Kind::glsl_vec3,
        Type_Builtin_Kind::glsl_vec4,
        Type_Builtin_Kind::glsl_dvec2,
        Type_Builtin_Kind::glsl_dvec3,
        Type_Builtin_Kind::glsl_dvec4,
        Type_Builtin_Kind::glsl_bvec2,
        Type_Builtin_Kind::glsl_bvec3,
        Type_Builtin_Kind::glsl_bvec4,
        Type_Builtin_Kind::glsl_ivec2,
        Type_Builtin_Kind::glsl_ivec3,
        Type_Builtin_Kind::glsl_ivec4,
        Type_Builtin_Kind::glsl_uvec2,
        Type_Builtin_Kind::glsl_uvec3,
        Type_Builtin_Kind::glsl_uvec4,
        Type_Builtin_Kind::glsl_mat2,
        Type_Builtin_Kind::glsl_mat2,
        Type_Builtin_Kind::glsl_mat3,
        Type_Builtin_Kind::glsl_mat3,
        Type_Builtin_Kind::glsl_mat4,
        Type_Builtin_Kind::glsl_mat4,
        Type_Builtin_Kind::glsl_mat2x3,
        Type_Builtin_Kind::glsl_mat2x4,
        Type_Builtin_Kind::glsl_mat3x2,
        Type_Builtin_Kind::glsl_mat3x4,
        Type_Builtin_Kind::glsl_mat4x2,
        Type_Builtin_Kind::glsl_mat4x3,
        Type_Builtin_Kind::glsl_dmat2,
        Type_Builtin_Kind::glsl_dmat2,
        Type_Builtin_Kind::glsl_dmat3,
        Type_Builtin_Kind::glsl_dmat3,
        Type_Builtin_Kind::glsl_dmat4,
        Type_Builtin_Kind::glsl_dmat4,
        Type_Builtin_Kind::glsl_dmat2x3,
        Type_Builtin_Kind::glsl_dmat2x4,
        Type_Builtin_Kind::glsl_dmat3x2,
        Type_Builtin_Kind::glsl_dmat3x4,
        Type_Builtin_Kind::glsl_dmat4x2,
        Type_Builtin_Kind::glsl_dmat4x3,
        Type_Builtin_Kind::glsl_sampler1D,
        Type_Builtin_Kind::glsl_texture1D,
        Type_Builtin_Kind::glsl_image1D,
        Type_Builtin_Kind::glsl_sampler1DShadow,
        Type_Builtin_Kind::glsl_sampler1DArray,
        Type_Builtin_Kind::glsl_texture1DArray,
        Type_Builtin_Kind::glsl_image1DArray,
        Type_Builtin_Kind::glsl_sampler1DArrayShadow,
        Type_Builtin_Kind::glsl_sampler2D,
        Type_Builtin_Kind::glsl_texture2D,
        Type_Builtin_Kind::glsl_image2D,
        Type_Builtin_Kind::glsl_sampler2DShadow,
        Type_Builtin_Kind::glsl_sampler2DArray,
        Type_Builtin_Kind::glsl_texture2DArray,
        Type_Builtin_Kind::glsl_image2DArray,
        Type_Builtin_Kind::glsl_sampler2DArrayShadow,
        Type_Builtin_Kind::glsl_sampler2DMS,
        Type_Builtin_Kind::glsl_texture2DMS,
        Type_Builtin_Kind::glsl_image2DMS,
        Type_Builtin_Kind::glsl_sampler2DMSArray,
        Type_Builtin_Kind::glsl_texture2DMSArray,
        Type_Builtin_Kind::glsl_image2DMSArray,
        Type_Builtin_Kind::glsl_sampler2DRect,
        Type_Builtin_Kind::glsl_texture2DRect,
        Type_Builtin_Kind::glsl_image2DRect,
        Type_Builtin_Kind::glsl_sampler2DRectShadow,
        Type_Builtin_Kind::glsl_sampler3D,
        Type_Builtin_Kind::glsl_texture3D,
        Type_Builtin_Kind::glsl_image3D,
        Type_Builtin_Kind::glsl_samplerCube,
        Type_Builtin_Kind::glsl_textureCube,
        Type_Builtin_Kind::glsl_imageCube,
        Type_Builtin_Kind::glsl_samplerCubeShadow,
        Type_Builtin_Kind::glsl_samplerCubeArray,
        Type_Builtin_Kind::glsl_textureCubeArray,
        Type_Builtin_Kind::glsl_imageCubeArray,
        Type_Builtin_Kind::glsl_samplerCubeArrayShadow,
        Type_Builtin_Kind::glsl_samplerBuffer,
        Type_Builtin_Kind::glsl_textureBuffer,
        Type_Builtin_Kind::glsl_imageBuffer,
        Type_Builtin_Kind::glsl_subpassInput,
        Type_Builtin_Kind::glsl_subpassInputMS,
        Type_Builtin_Kind::glsl_isampler1D,
        Type_Builtin_Kind::glsl_itexture1D,
        Type_Builtin_Kind::glsl_iimage1D,
        Type_Builtin_Kind::glsl_isampler1DArray,
        Type_Builtin_Kind::glsl_itexture1DArray,
        Type_Builtin_Kind::glsl_iimage1DArray,
        Type_Builtin_Kind::glsl_isampler2D,
        Type_Builtin_Kind::glsl_itexture2D,
        Type_Builtin_Kind::glsl_iimage2D,
        Type_Builtin_Kind::glsl_isampler2DArray,
        Type_Builtin_Kind::glsl_itexture2DArray,
        Type_Builtin_Kind::glsl_iimage2DArray,
        Type_Builtin_Kind::glsl_isampler2DMS,
        Type_Builtin_Kind::glsl_itexture2DMS,
        Type_Builtin_Kind::glsl_iimage2DMS,
        Type_Builtin_Kind::glsl_isampler2DMSArray,
        Type_Builtin_Kind::glsl_itexture2DMSArray,
        Type_Builtin_Kind::glsl_iimage2DMSArray,
        Type_Builtin_Kind::glsl_isampler2DRect,
        Type_Builtin_Kind::glsl_itexture2DRect,
        Type_Builtin_Kind::glsl_iimage2DRect,
        Type_Builtin_Kind::glsl_isampler3D,
        Type_Builtin_Kind::glsl_itexture3D,
        Type_Builtin_Kind::glsl_iimage3D,
        Type_Builtin_Kind::glsl_isamplerCube,
        Type_Builtin_Kind::glsl_itextureCube,
        Type_Builtin_Kind::glsl_iimageCube,
        Type_Builtin_Kind::glsl_isamplerCubeArray,
        Type_Builtin_Kind::glsl_itextureCubeArray,
        Type_Builtin_Kind::glsl_iimageCubeArray,
        Type_Builtin_Kind::glsl_isamplerBuffer,
        Type_Builtin_Kind::glsl_itextureBuffer,
        Type_Builtin_Kind::glsl_iimageBuffer,
        Type_Builtin_Kind::glsl_isubpassInput,
        Type_Builtin_Kind::glsl_isubpassInputMS,
        Type_Builtin_Kind::glsl_usampler1D,
        Type_Builtin_Kind::glsl_utexture1D,
        Type_Builtin_Kind::glsl_uimage1D,
        Type_Builtin_Kind::glsl_usampler1DArray,
        Type_Builtin_Kind::glsl_utexture1DArray,
        Type_Builtin_Kind::glsl_uimage1DArray,
        Type_Builtin_Kind::glsl_usampler2D,
        Type_Builtin_Kind::glsl_utexture2D,
        Type_Builtin_Kind::glsl_uimage2D,
        Type_Builtin_Kind::glsl_usampler2DArray,
        Type_Builtin_Kind::glsl_utexture2DArray,
        Type_Builtin_Kind::glsl_uimage2DArray,
        Type_Builtin_Kind::glsl_usampler2DMS,
        Type_Builtin_Kind::glsl_utexture2DMS,
        Type_Builtin_Kind::glsl_uimage2DMS,
        Type_Builtin_Kind::glsl_usampler2DMSArray,
        Type_Builtin_Kind::glsl_utexture2DMSArray,
        Type_Builtin_Kind::glsl_uimage2DMSArray,
        Type_Builtin_Kind::glsl_usampler2DRect,
        Type_Builtin_Kind::glsl_utexture2DRect,
        Type_Builtin_Kind::glsl_uimage2DRect,
        Type_Builtin_Kind::glsl_usampler3D,
        Type_Builtin_Kind::glsl_utexture3D,
        Type_Builtin_Kind::glsl_uimage3D,
        Type_Builtin_Kind::glsl_usamplerCube,
        Type_Builtin_Kind::glsl_utextureCube,
        Type_Builtin_Kind::glsl_uimageCube,
        Type_Builtin_Kind::glsl_usamplerCubeArray,
        Type_Builtin_Kind::glsl_utextureCubeArray,
        Type_Builtin_Kind::glsl_uimageCubeArray,
        Type_Builtin_Kind::glsl_usamplerBuffer,
        Type_Builtin_Kind::glsl_utextureBuffer,
        Type_Builtin_Kind::glsl_uimageBuffer,
        Type_Builtin_Kind::glsl_usubpassInput,
        Type_Builtin_Kind::glsl_usubpassInputMS,
        Type_Builtin_Kind::glsl_sampler,
        Type_Builtin_Kind::glsl_samplerShadow,
      };

      constexpr i64 array_size = sizeof(builtin_types_strings) / sizeof(anton::String_View);
      for(i64 i = 0; i < array_size; ++i) {
        if(type == builtin_types_strings[i]) {
          return builtin_types[i];
        }
      }

      return anton::null_optional;
    }

    bool compare_types_equal(Type const& lhs, Type const& rhs)
    {
      if(lhs.node_kind != rhs.node_kind) {
        return false;
      }

      Node_Kind const kind = lhs.node_kind;
      switch(kind) {
        case Node_Kind::type_builtin: {
          Type_Builtin const& lhs_v = static_cast<Type_Builtin const&>(lhs);
          Type_Builtin const& rhs_v = static_cast<Type_Builtin const&>(rhs);
          return lhs_v.value == rhs_v.value;
        }

        case Node_Kind::type_user_defined: {
          Type_User_Defined const& lhs_v = static_cast<Type_User_Defined const&>(lhs);
          Type_User_Defined const& rhs_v = static_cast<Type_User_Defined const&>(rhs);
          return lhs_v.value == rhs_v.value;
        }

        case Node_Kind::type_array: {
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
          return lhs_v.size->value == rhs_v.size->value;
        }

        default:
          ANTON_UNREACHABLE();
      }
    }
  } // namespace ast
} // namespace vush
