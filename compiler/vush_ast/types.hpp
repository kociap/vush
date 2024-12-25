#pragma once

#include <anton/optional.hpp>

#include <vush_ast/fwd.hpp>
#include <vush_core/types.hpp>

namespace vush::ast {
  struct Qualifiers {
    bool mut = false;
  };

  enum struct Type_Kind : u8 {
    type_builtin,
    type_struct,
    type_array,
  };

  struct Type {
    Type_Kind type_kind;
    Qualifiers qualifiers;
    Source_Info source_info;

    Type(Source_Info const& source_info, Type_Kind type_kind)
      : type_kind(type_kind), source_info(source_info)
    {
    }

    Type(Source_Info const& source_info, Type_Kind type_kind,
         Qualifiers qualifiers)
      : type_kind(type_kind), qualifiers(qualifiers), source_info(source_info)
    {
    }
  };

  template<typename T>
  [[nodiscard]] bool instanceof(Type const& node);

  template<typename T>
  [[nodiscard]] bool instanceof(Type const* node)
  {
    return instanceof<T>(*node);
  }

  [[nodiscard]] bool compare_types_equal(Type const& lhs, Type const& rhs);

  // is_integer_based
  // Check whether the type's fundamental operations operate on integer
  // numbers.
  //
  [[nodiscard]] bool is_integer_based(Type const& type);

  // is_signed_integer_based
  // Check whether the type's fundamental operations operate on signed integer
  // numbers.
  //
  [[nodiscard]] bool is_signed_integer_based(Type const& type);

  // is_unsigned_integer_based
  // Check whether the type's fundamental operations operate on unsigned integer
  // numbers.
  //
  [[nodiscard]] bool is_unsigned_integer_based(Type const& type);

  // is_fp_based
  // Check whether the type's fundamental operations operate on floating point
  // numbers.
  //
  [[nodiscard]] bool is_fp_based(Type const& type);

  [[nodiscard]] bool is_void(Type const& type);
  [[nodiscard]] bool is_scalar(Type const& type);
  [[nodiscard]] bool is_integer(Type const& type);
  [[nodiscard]] bool is_fp(Type const& type);
  [[nodiscard]] bool is_signed_integer(Type const& type);
  [[nodiscard]] bool is_unsigned_integer(Type const& type);
  [[nodiscard]] bool is_vector(Type const& type);
  [[nodiscard]] bool is_vector2(Type const& type);
  [[nodiscard]] bool is_vector3(Type const& type);
  [[nodiscard]] bool is_vector4(Type const& type);
  [[nodiscard]] bool is_bool_vector(Type const& type);
  [[nodiscard]] bool is_integer_vector(Type const& type);
  [[nodiscard]] bool is_signed_integer_vector(Type const& type);
  [[nodiscard]] bool is_unsigned_integer_vector(Type const& type);
  [[nodiscard]] bool is_fp_vector(Type const& type);
  [[nodiscard]] bool is_i32_vector(Type const& type);
  [[nodiscard]] bool is_u32_vector(Type const& type);
  [[nodiscard]] bool is_f32_vector(Type const& type);
  [[nodiscard]] bool is_f64_vector(Type const& type);
  [[nodiscard]] bool is_matrix(Type const& type);
  [[nodiscard]] bool is_f32_matrix(Type const& type);
  [[nodiscard]] bool is_f64_matrix(Type const& type);

  // get_vector_size
  // Calculate the size (number of elements) of a vector type.
  //
  // Returns:
  // Size or -1 if not a vector type.
  //
  [[nodiscard]] i64 get_vector_size(Type const& type);

  // get_matrix_rows
  // Calculate the number of matrix rows.
  //
  // Returns:
  // Number of rows or -1 if not a matrix type.
  //
  [[nodiscard]] i64 get_matrix_rows(Type const& type);

  // get_matrix_columns
  // Calculate the number of matrix columns.
  //
  // Returns:
  // Number of columns or -1 if not a matrix type.
  //
  [[nodiscard]] i64 get_matrix_columns(Type const& type);

  // is_opaque_type
  // Check whether a type is opaque, i.e. is an opaque builtin type, a struct
  // containing an opaque type or an array of opaque types.
  //
  // If a struct does not have a corresponding definition, returns true.
  //
  [[nodiscard]] bool is_opaque_type(Type const& type);

  // is_arithmetic_type
  // Check whether type is a type that supports arithmetic operations, e.g.
  // addition.
  //
  [[nodiscard]] bool is_arithmetic_type(Type const& type);

  [[nodiscard]] bool is_array(Type const& type);
  [[nodiscard]] bool is_unsized_array(Type const& type);
  [[nodiscard]] bool is_sized_array(Type const& type);
  [[nodiscard]] bool is_image_type(Type const& type);

  enum struct Type_Builtin_Kind : u8 {
    e_void,
    e_bool,
    e_int,
    e_uint,
    e_float,
    e_double,
    e_vec2,
    e_vec3,
    e_vec4,
    e_dvec2,
    e_dvec3,
    e_dvec4,
    e_bvec2,
    e_bvec3,
    e_bvec4,
    e_ivec2,
    e_ivec3,
    e_ivec4,
    e_uvec2,
    e_uvec3,
    e_uvec4,
    e_mat2,
    e_mat3,
    e_mat4,
    e_mat2x3,
    e_mat2x4,
    e_mat3x2,
    e_mat3x4,
    e_mat4x2,
    e_mat4x3,
    e_dmat2,
    e_dmat3,
    e_dmat4,
    e_dmat2x3,
    e_dmat2x4,
    e_dmat3x2,
    e_dmat3x4,
    e_dmat4x2,
    e_dmat4x3,
    // Image types.
    e_image1D,
    e_image1DArray,
    e_image2D,
    e_image2DArray,
    e_image2DMS,
    e_image2DMSArray,
    e_image2DRect,
    e_image3D,
    e_imageBuffer,
    e_imageCube,
    e_imageCubeArray,
    e_iimage1D,
    e_iimage1DArray,
    e_iimage2D,
    e_iimage2DArray,
    e_iimage2DMS,
    e_iimage2DMSArray,
    e_iimage2DRect,
    e_iimage3D,
    e_iimageBuffer,
    e_iimageCube,
    e_iimageCubeArray,
    e_uimage1D,
    e_uimage1DArray,
    e_uimage2D,
    e_uimage2DArray,
    e_uimage2DMS,
    e_uimage2DMSArray,
    e_uimage2DRect,
    e_uimage3D,
    e_uimageBuffer,
    e_uimageCube,
    e_uimageCubeArray,
    // Sampler types.
    e_sampler,
    e_sampler1D,
    e_sampler1DArray,
    e_sampler1DArrayShadow,
    e_sampler1DShadow,
    e_sampler2D,
    e_sampler2DArray,
    e_sampler2DArrayShadow,
    e_sampler2DMS,
    e_sampler2DMSArray,
    e_sampler2DRect,
    e_sampler2DRectShadow,
    e_sampler2DShadow,
    e_sampler3D,
    e_samplerBuffer,
    e_samplerCube,
    e_samplerCubeArray,
    e_samplerCubeArrayShadow,
    e_samplerCubeShadow,
    e_isampler1D,
    e_isampler1DArray,
    e_isampler2D,
    e_isampler2DArray,
    e_isampler2DMS,
    e_isampler2DMSArray,
    e_isampler2DRect,
    e_isampler3D,
    e_isamplerBuffer,
    e_isamplerCube,
    e_isamplerCubeArray,
    e_usampler1D,
    e_usampler1DArray,
    e_usampler2D,
    e_usampler2DArray,
    e_usampler2DMS,
    e_usampler2DMSArray,
    e_usampler2DRect,
    e_usampler3D,
    e_usamplerBuffer,
    e_usamplerCube,
    e_usamplerCubeArray,
    // Texture types.
    e_texture1D,
    e_texture1DArray,
    e_texture2D,
    e_texture2DArray,
    e_texture2DMS,
    e_texture2DMSArray,
    e_texture2DRect,
    e_texture3D,
    e_textureBuffer,
    e_textureCube,
    e_textureCubeArray,
    e_itexture1D,
    e_itexture1DArray,
    e_itexture2D,
    e_itexture2DArray,
    e_itexture2DMS,
    e_itexture2DMSArray,
    e_itexture2DRect,
    e_itexture3D,
    e_itextureBuffer,
    e_itextureCube,
    e_itextureCubeArray,
    e_utexture1D,
    e_utexture1DArray,
    e_utexture2D,
    e_utexture2DArray,
    e_utexture2DMS,
    e_utexture2DMSArray,
    e_utexture2DRect,
    e_utexture3D,
    e_utextureBuffer,
    e_utextureCube,
    e_utextureCubeArray,
    // Subpass types.
    e_subpassInput,
    e_subpassInputMS,
    e_isubpassInput,
    e_isubpassInputMS,
    e_usubpassInput,
    e_usubpassInputMS,
  };

  [[nodiscard]] anton::Optional<Type_Builtin_Kind>
  enumify_builtin_type_kind(anton::String_View type);

  struct Type_Builtin: public Type {
    Type_Builtin_Kind value;

    Type_Builtin(Source_Info const& source_info, Type_Builtin_Kind value)
      : Type(source_info, Type_Kind::type_builtin), value(value)
    {
    }

    Type_Builtin(Source_Info const& source_info, Qualifiers qualifiers,
                 Type_Builtin_Kind value)
      : Type(source_info, Type_Kind::type_builtin, qualifiers), value(value)
    {
    }
  };

  struct Type_Struct: public Type {
    // The identifier value, that is the name of the type.
    anton::String_View value;
    Decl_Struct* definition = nullptr;

    Type_Struct(Source_Info const& source_info, anton::String_View value)
      : Type(source_info, Type_Kind::type_struct), value(value)
    {
    }

    Type_Struct(Source_Info const& source_info, Qualifiers qualifiers,
                anton::String&& value)
      : Type(source_info, Type_Kind::type_struct, qualifiers),
        value(ANTON_MOV(value))
    {
    }
  };

  struct Type_Array: public Type {
    Type* base;
    // nullptr when the array is unsized.
    Lt_Integer* size;

    Type_Array(Source_Info const& source_info, Type* base, Lt_Integer* size)
      : Type(source_info, Type_Kind::type_array), base(base), size(size)
    {
    }

    Type_Array(Source_Info const& source_info, Qualifiers qualifiers,
               Type* base, Lt_Integer* size)
      : Type(source_info, Type_Kind::type_array, qualifiers), base(base),
        size(size)
    {
    }
  };
} // namespace vush::ast
