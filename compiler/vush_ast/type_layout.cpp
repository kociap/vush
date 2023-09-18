#include <vush_ast/type_layout.hpp>

#include <vush_ast/ast.hpp>

namespace vush {
  Type_Layout get_builtin_type_layout(ast::Type_Builtin_Kind const kind)
  {
    switch(kind) {
    case ast::Type_Builtin_Kind::e_bool:
      return {
        .base_stride = 0,
        .stride = 1,
        .alignment = 1,
        .vector_elements = 1,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_int:
      return {
        .base_stride = 0,
        .stride = 4,
        .alignment = 4,
        .vector_elements = 1,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_uint:
      return {
        .base_stride = 0,
        .stride = 4,
        .alignment = 4,
        .vector_elements = 1,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_float:
      return {
        .base_stride = 0,
        .stride = 4,
        .alignment = 4,
        .vector_elements = 1,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_double:
      return {
        .base_stride = 0,
        .stride = 8,
        .alignment = 8,
        .vector_elements = 1,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_vec2:
      return {
        .base_stride = 4,
        .stride = 8,
        .alignment = 8,
        .vector_elements = 2,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_vec3:
      return {
        .base_stride = 4,
        .stride = 16,
        .alignment = 16,
        .vector_elements = 3,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_vec4:
      return {
        .base_stride = 4,
        .stride = 16,
        .alignment = 16,
        .vector_elements = 4,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_dvec2:
      return {
        .base_stride = 8,
        .stride = 16,
        .alignment = 16,
        .vector_elements = 2,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_dvec3:
      return {
        .base_stride = 8,
        .stride = 32,
        .alignment = 32,
        .vector_elements = 3,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_dvec4:
      return {
        .base_stride = 8,
        .stride = 32,
        .alignment = 32,
        .vector_elements = 3,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_bvec2:
      return {
        .base_stride = 1,
        .stride = 2,
        .alignment = 2,
        .vector_elements = 2,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_bvec3:
      return {
        .base_stride = 1,
        .stride = 4,
        .alignment = 4,
        .vector_elements = 3,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_bvec4:
      return {
        .base_stride = 1,
        .stride = 4,
        .alignment = 4,
        .vector_elements = 4,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_ivec2:
      return {
        .base_stride = 4,
        .stride = 8,
        .alignment = 8,
        .vector_elements = 2,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_ivec3:
      return {
        .base_stride = 4,
        .stride = 16,
        .alignment = 16,
        .vector_elements = 3,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_ivec4:
      return {
        .base_stride = 4,
        .stride = 16,
        .alignment = 16,
        .vector_elements = 4,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_uvec2:
      return {
        .base_stride = 4,
        .stride = 8,
        .alignment = 8,
        .vector_elements = 2,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_uvec3:
      return {
        .base_stride = 4,
        .stride = 16,
        .alignment = 16,
        .vector_elements = 3,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_uvec4:
      return {
        .base_stride = 4,
        .stride = 16,
        .alignment = 16,
        .vector_elements = 4,
        .matrix_columns = 1,
      };
    case ast::Type_Builtin_Kind::e_mat2:
      return {
        .base_stride = 4,
        .stride = 16,
        .alignment = 8,
        .vector_elements = 2,
        .matrix_columns = 2,
      };
    case ast::Type_Builtin_Kind::e_mat3:
      return {
        .base_stride = 4,
        .stride = 48,
        .alignment = 16,
        .vector_elements = 3,
        .matrix_columns = 3,
      };
    case ast::Type_Builtin_Kind::e_mat4:
      return {
        .base_stride = 4,
        .stride = 64,
        .alignment = 16,
        .vector_elements = 4,
        .matrix_columns = 4,
      };
    case ast::Type_Builtin_Kind::e_mat2x3:
      return {
        .base_stride = 4,
        .stride = 32,
        .alignment = 16,
        .vector_elements = 2,
        .matrix_columns = 3,
      };
    case ast::Type_Builtin_Kind::e_mat2x4:
      return {
        .base_stride = 4,
        .stride = 32,
        .alignment = 16,
        .vector_elements = 2,
        .matrix_columns = 4,
      };
    case ast::Type_Builtin_Kind::e_mat3x2:
      return {
        .base_stride = 4,
        .stride = 24,
        .alignment = 8,
        .vector_elements = 3,
        .matrix_columns = 2,
      };
    case ast::Type_Builtin_Kind::e_mat3x4:
      return {
        .base_stride = 4,
        .stride = 48,
        .alignment = 16,
        .vector_elements = 3,
        .matrix_columns = 4,
      };
    case ast::Type_Builtin_Kind::e_mat4x2:
      return {
        .base_stride = 4,
        .stride = 32,
        .alignment = 8,
        .vector_elements = 4,
        .matrix_columns = 2,
      };
    case ast::Type_Builtin_Kind::e_mat4x3:
      return {
        .base_stride = 4,
        .stride = 64,
        .alignment = 16,
        .vector_elements = 4,
        .matrix_columns = 3,
      };
    case ast::Type_Builtin_Kind::e_dmat2:
      return {
        .base_stride = 8,
        .stride = 32,
        .alignment = 16,
        .vector_elements = 2,
        .matrix_columns = 2,
      };
    case ast::Type_Builtin_Kind::e_dmat3:
      return {
        .base_stride = 8,
        .stride = 96,
        .alignment = 32,
        .vector_elements = 3,
        .matrix_columns = 3,
      };
    case ast::Type_Builtin_Kind::e_dmat4:
      return {
        .base_stride = 8,
        .stride = 128,
        .alignment = 32,
        .vector_elements = 4,
        .matrix_columns = 4,
      };
    case ast::Type_Builtin_Kind::e_dmat2x3:
      return {
        .base_stride = 8,
        .stride = 64,
        .alignment = 32,
        .vector_elements = 2,
        .matrix_columns = 3,
      };
    case ast::Type_Builtin_Kind::e_dmat2x4:
      return {
        .base_stride = 8,
        .stride = 64,
        .alignment = 32,
        .vector_elements = 2,
        .matrix_columns = 4,
      };
    case ast::Type_Builtin_Kind::e_dmat3x2:
      return {
        .base_stride = 8,
        .stride = 48,
        .alignment = 16,
        .vector_elements = 3,
        .matrix_columns = 2,
      };
    case ast::Type_Builtin_Kind::e_dmat3x4:
      return {
        .base_stride = 8,
        .stride = 96,
        .alignment = 32,
        .vector_elements = 3,
        .matrix_columns = 4,
      };
    case ast::Type_Builtin_Kind::e_dmat4x2:
      return {
        .base_stride = 8,
        .stride = 64,
        .alignment = 16,
        .vector_elements = 4,
        .matrix_columns = 2,
      };
    case ast::Type_Builtin_Kind::e_dmat4x3:
      return {
        .base_stride = 8,
        .stride = 128,
        .alignment = 32,
        .vector_elements = 4,
        .matrix_columns = 3,
      };

    case ast::Type_Builtin_Kind::e_void:
    case ast::Type_Builtin_Kind::e_sampler1D:
    case ast::Type_Builtin_Kind::e_texture1D:
    case ast::Type_Builtin_Kind::e_image1D:
    case ast::Type_Builtin_Kind::e_sampler1DShadow:
    case ast::Type_Builtin_Kind::e_sampler1DArray:
    case ast::Type_Builtin_Kind::e_texture1DArray:
    case ast::Type_Builtin_Kind::e_image1DArray:
    case ast::Type_Builtin_Kind::e_sampler1DArrayShadow:
    case ast::Type_Builtin_Kind::e_sampler2D:
    case ast::Type_Builtin_Kind::e_texture2D:
    case ast::Type_Builtin_Kind::e_image2D:
    case ast::Type_Builtin_Kind::e_sampler2DShadow:
    case ast::Type_Builtin_Kind::e_sampler2DArray:
    case ast::Type_Builtin_Kind::e_texture2DArray:
    case ast::Type_Builtin_Kind::e_image2DArray:
    case ast::Type_Builtin_Kind::e_sampler2DArrayShadow:
    case ast::Type_Builtin_Kind::e_sampler2DMS:
    case ast::Type_Builtin_Kind::e_texture2DMS:
    case ast::Type_Builtin_Kind::e_image2DMS:
    case ast::Type_Builtin_Kind::e_sampler2DMSArray:
    case ast::Type_Builtin_Kind::e_texture2DMSArray:
    case ast::Type_Builtin_Kind::e_image2DMSArray:
    case ast::Type_Builtin_Kind::e_sampler2DRect:
    case ast::Type_Builtin_Kind::e_texture2DRect:
    case ast::Type_Builtin_Kind::e_image2DRect:
    case ast::Type_Builtin_Kind::e_sampler2DRectShadow:
    case ast::Type_Builtin_Kind::e_sampler3D:
    case ast::Type_Builtin_Kind::e_texture3D:
    case ast::Type_Builtin_Kind::e_image3D:
    case ast::Type_Builtin_Kind::e_samplerCube:
    case ast::Type_Builtin_Kind::e_textureCube:
    case ast::Type_Builtin_Kind::e_imageCube:
    case ast::Type_Builtin_Kind::e_samplerCubeShadow:
    case ast::Type_Builtin_Kind::e_samplerCubeArray:
    case ast::Type_Builtin_Kind::e_textureCubeArray:
    case ast::Type_Builtin_Kind::e_imageCubeArray:
    case ast::Type_Builtin_Kind::e_samplerCubeArrayShadow:
    case ast::Type_Builtin_Kind::e_samplerBuffer:
    case ast::Type_Builtin_Kind::e_textureBuffer:
    case ast::Type_Builtin_Kind::e_imageBuffer:
    case ast::Type_Builtin_Kind::e_subpassInput:
    case ast::Type_Builtin_Kind::e_subpassInputMS:
    case ast::Type_Builtin_Kind::e_isampler1D:
    case ast::Type_Builtin_Kind::e_itexture1D:
    case ast::Type_Builtin_Kind::e_iimage1D:
    case ast::Type_Builtin_Kind::e_isampler1DArray:
    case ast::Type_Builtin_Kind::e_itexture1DArray:
    case ast::Type_Builtin_Kind::e_iimage1DArray:
    case ast::Type_Builtin_Kind::e_isampler2D:
    case ast::Type_Builtin_Kind::e_itexture2D:
    case ast::Type_Builtin_Kind::e_iimage2D:
    case ast::Type_Builtin_Kind::e_isampler2DArray:
    case ast::Type_Builtin_Kind::e_itexture2DArray:
    case ast::Type_Builtin_Kind::e_iimage2DArray:
    case ast::Type_Builtin_Kind::e_isampler2DMS:
    case ast::Type_Builtin_Kind::e_itexture2DMS:
    case ast::Type_Builtin_Kind::e_iimage2DMS:
    case ast::Type_Builtin_Kind::e_isampler2DMSArray:
    case ast::Type_Builtin_Kind::e_itexture2DMSArray:
    case ast::Type_Builtin_Kind::e_iimage2DMSArray:
    case ast::Type_Builtin_Kind::e_isampler2DRect:
    case ast::Type_Builtin_Kind::e_itexture2DRect:
    case ast::Type_Builtin_Kind::e_iimage2DRect:
    case ast::Type_Builtin_Kind::e_isampler3D:
    case ast::Type_Builtin_Kind::e_itexture3D:
    case ast::Type_Builtin_Kind::e_iimage3D:
    case ast::Type_Builtin_Kind::e_isamplerCube:
    case ast::Type_Builtin_Kind::e_itextureCube:
    case ast::Type_Builtin_Kind::e_iimageCube:
    case ast::Type_Builtin_Kind::e_isamplerCubeArray:
    case ast::Type_Builtin_Kind::e_itextureCubeArray:
    case ast::Type_Builtin_Kind::e_iimageCubeArray:
    case ast::Type_Builtin_Kind::e_isamplerBuffer:
    case ast::Type_Builtin_Kind::e_itextureBuffer:
    case ast::Type_Builtin_Kind::e_iimageBuffer:
    case ast::Type_Builtin_Kind::e_isubpassInput:
    case ast::Type_Builtin_Kind::e_isubpassInputMS:
    case ast::Type_Builtin_Kind::e_usampler1D:
    case ast::Type_Builtin_Kind::e_utexture1D:
    case ast::Type_Builtin_Kind::e_uimage1D:
    case ast::Type_Builtin_Kind::e_usampler1DArray:
    case ast::Type_Builtin_Kind::e_utexture1DArray:
    case ast::Type_Builtin_Kind::e_uimage1DArray:
    case ast::Type_Builtin_Kind::e_usampler2D:
    case ast::Type_Builtin_Kind::e_utexture2D:
    case ast::Type_Builtin_Kind::e_uimage2D:
    case ast::Type_Builtin_Kind::e_usampler2DArray:
    case ast::Type_Builtin_Kind::e_utexture2DArray:
    case ast::Type_Builtin_Kind::e_uimage2DArray:
    case ast::Type_Builtin_Kind::e_usampler2DMS:
    case ast::Type_Builtin_Kind::e_utexture2DMS:
    case ast::Type_Builtin_Kind::e_uimage2DMS:
    case ast::Type_Builtin_Kind::e_usampler2DMSArray:
    case ast::Type_Builtin_Kind::e_utexture2DMSArray:
    case ast::Type_Builtin_Kind::e_uimage2DMSArray:
    case ast::Type_Builtin_Kind::e_usampler2DRect:
    case ast::Type_Builtin_Kind::e_utexture2DRect:
    case ast::Type_Builtin_Kind::e_uimage2DRect:
    case ast::Type_Builtin_Kind::e_usampler3D:
    case ast::Type_Builtin_Kind::e_utexture3D:
    case ast::Type_Builtin_Kind::e_uimage3D:
    case ast::Type_Builtin_Kind::e_usamplerCube:
    case ast::Type_Builtin_Kind::e_utextureCube:
    case ast::Type_Builtin_Kind::e_uimageCube:
    case ast::Type_Builtin_Kind::e_usamplerCubeArray:
    case ast::Type_Builtin_Kind::e_utextureCubeArray:
    case ast::Type_Builtin_Kind::e_uimageCubeArray:
    case ast::Type_Builtin_Kind::e_usamplerBuffer:
    case ast::Type_Builtin_Kind::e_utextureBuffer:
    case ast::Type_Builtin_Kind::e_uimageBuffer:
    case ast::Type_Builtin_Kind::e_usubpassInput:
    case ast::Type_Builtin_Kind::e_usubpassInputMS:
    case ast::Type_Builtin_Kind::e_sampler:
    case ast::Type_Builtin_Kind::e_samplerShadow:
      return {
        .base_stride = 0,
        .stride = 0,
        .alignment = 0,
        .vector_elements = 0,
        .matrix_columns = 0,
      };
    }
  }
} // namespace vush
