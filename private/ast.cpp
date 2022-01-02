#include <ast.hpp>

#include <anton/intrinsics.hpp>

namespace vush {
    bool is_opaque_type(Builtin_GLSL_Type const type) {
        return static_cast<i32>(type) >= static_cast<i32>(Builtin_GLSL_Type::glsl_sampler1D);
    }

    bool is_image_type(Builtin_GLSL_Type const type) {
        return type == Builtin_GLSL_Type::glsl_image1D || type == Builtin_GLSL_Type::glsl_image1DArray || type == Builtin_GLSL_Type::glsl_image2D ||
               type == Builtin_GLSL_Type::glsl_image2DArray || type == Builtin_GLSL_Type::glsl_image2DMS || type == Builtin_GLSL_Type::glsl_image2DMSArray ||
               type == Builtin_GLSL_Type::glsl_image2DRect || type == Builtin_GLSL_Type::glsl_image3D || type == Builtin_GLSL_Type::glsl_imageCube ||
               type == Builtin_GLSL_Type::glsl_imageCubeArray || type == Builtin_GLSL_Type::glsl_imageBuffer || type == Builtin_GLSL_Type::glsl_iimage1D ||
               type == Builtin_GLSL_Type::glsl_iimage1DArray || type == Builtin_GLSL_Type::glsl_iimage2D || type == Builtin_GLSL_Type::glsl_iimage2DArray ||
               type == Builtin_GLSL_Type::glsl_iimage2DMS || type == Builtin_GLSL_Type::glsl_iimage2DMSArray || type == Builtin_GLSL_Type::glsl_iimage2DRect ||
               type == Builtin_GLSL_Type::glsl_iimage3D || type == Builtin_GLSL_Type::glsl_iimageCube || type == Builtin_GLSL_Type::glsl_iimageCubeArray ||
               type == Builtin_GLSL_Type::glsl_iimageBuffer || type == Builtin_GLSL_Type::glsl_uimage1D || type == Builtin_GLSL_Type::glsl_uimage1DArray ||
               type == Builtin_GLSL_Type::glsl_uimage2D || type == Builtin_GLSL_Type::glsl_uimage2DArray || type == Builtin_GLSL_Type::glsl_uimage2DMS ||
               type == Builtin_GLSL_Type::glsl_uimage2DMSArray || type == Builtin_GLSL_Type::glsl_uimage2DRect || type == Builtin_GLSL_Type::glsl_uimage3D ||
               type == Builtin_GLSL_Type::glsl_uimageCube || type == Builtin_GLSL_Type::glsl_uimageCubeArray || type == Builtin_GLSL_Type::glsl_uimageBuffer;
    }

    anton::Optional<Builtin_GLSL_Type> enumify_builtin_glsl_type(anton::String_View const type) {
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

        static constexpr Builtin_GLSL_Type builtin_types[] = {
            Builtin_GLSL_Type::glsl_void,
            Builtin_GLSL_Type::glsl_bool,
            Builtin_GLSL_Type::glsl_int,
            Builtin_GLSL_Type::glsl_uint,
            Builtin_GLSL_Type::glsl_float,
            Builtin_GLSL_Type::glsl_double,
            Builtin_GLSL_Type::glsl_vec2,
            Builtin_GLSL_Type::glsl_vec3,
            Builtin_GLSL_Type::glsl_vec4,
            Builtin_GLSL_Type::glsl_dvec2,
            Builtin_GLSL_Type::glsl_dvec3,
            Builtin_GLSL_Type::glsl_dvec4,
            Builtin_GLSL_Type::glsl_bvec2,
            Builtin_GLSL_Type::glsl_bvec3,
            Builtin_GLSL_Type::glsl_bvec4,
            Builtin_GLSL_Type::glsl_ivec2,
            Builtin_GLSL_Type::glsl_ivec3,
            Builtin_GLSL_Type::glsl_ivec4,
            Builtin_GLSL_Type::glsl_uvec2,
            Builtin_GLSL_Type::glsl_uvec3,
            Builtin_GLSL_Type::glsl_uvec4,
            Builtin_GLSL_Type::glsl_mat2,
            Builtin_GLSL_Type::glsl_mat2,
            Builtin_GLSL_Type::glsl_mat3,
            Builtin_GLSL_Type::glsl_mat3,
            Builtin_GLSL_Type::glsl_mat4,
            Builtin_GLSL_Type::glsl_mat4,
            Builtin_GLSL_Type::glsl_mat2x3,
            Builtin_GLSL_Type::glsl_mat2x4,
            Builtin_GLSL_Type::glsl_mat3x2,
            Builtin_GLSL_Type::glsl_mat3x4,
            Builtin_GLSL_Type::glsl_mat4x2,
            Builtin_GLSL_Type::glsl_mat4x3,
            Builtin_GLSL_Type::glsl_dmat2,
            Builtin_GLSL_Type::glsl_dmat2,
            Builtin_GLSL_Type::glsl_dmat3,
            Builtin_GLSL_Type::glsl_dmat3,
            Builtin_GLSL_Type::glsl_dmat4,
            Builtin_GLSL_Type::glsl_dmat4,
            Builtin_GLSL_Type::glsl_dmat2x3,
            Builtin_GLSL_Type::glsl_dmat2x4,
            Builtin_GLSL_Type::glsl_dmat3x2,
            Builtin_GLSL_Type::glsl_dmat3x4,
            Builtin_GLSL_Type::glsl_dmat4x2,
            Builtin_GLSL_Type::glsl_dmat4x3,
            Builtin_GLSL_Type::glsl_sampler1D,
            Builtin_GLSL_Type::glsl_texture1D,
            Builtin_GLSL_Type::glsl_image1D,
            Builtin_GLSL_Type::glsl_sampler1DShadow,
            Builtin_GLSL_Type::glsl_sampler1DArray,
            Builtin_GLSL_Type::glsl_texture1DArray,
            Builtin_GLSL_Type::glsl_image1DArray,
            Builtin_GLSL_Type::glsl_sampler1DArrayShadow,
            Builtin_GLSL_Type::glsl_sampler2D,
            Builtin_GLSL_Type::glsl_texture2D,
            Builtin_GLSL_Type::glsl_image2D,
            Builtin_GLSL_Type::glsl_sampler2DShadow,
            Builtin_GLSL_Type::glsl_sampler2DArray,
            Builtin_GLSL_Type::glsl_texture2DArray,
            Builtin_GLSL_Type::glsl_image2DArray,
            Builtin_GLSL_Type::glsl_sampler2DArrayShadow,
            Builtin_GLSL_Type::glsl_sampler2DMS,
            Builtin_GLSL_Type::glsl_texture2DMS,
            Builtin_GLSL_Type::glsl_image2DMS,
            Builtin_GLSL_Type::glsl_sampler2DMSArray,
            Builtin_GLSL_Type::glsl_texture2DMSArray,
            Builtin_GLSL_Type::glsl_image2DMSArray,
            Builtin_GLSL_Type::glsl_sampler2DRect,
            Builtin_GLSL_Type::glsl_texture2DRect,
            Builtin_GLSL_Type::glsl_image2DRect,
            Builtin_GLSL_Type::glsl_sampler2DRectShadow,
            Builtin_GLSL_Type::glsl_sampler3D,
            Builtin_GLSL_Type::glsl_texture3D,
            Builtin_GLSL_Type::glsl_image3D,
            Builtin_GLSL_Type::glsl_samplerCube,
            Builtin_GLSL_Type::glsl_textureCube,
            Builtin_GLSL_Type::glsl_imageCube,
            Builtin_GLSL_Type::glsl_samplerCubeShadow,
            Builtin_GLSL_Type::glsl_samplerCubeArray,
            Builtin_GLSL_Type::glsl_textureCubeArray,
            Builtin_GLSL_Type::glsl_imageCubeArray,
            Builtin_GLSL_Type::glsl_samplerCubeArrayShadow,
            Builtin_GLSL_Type::glsl_samplerBuffer,
            Builtin_GLSL_Type::glsl_textureBuffer,
            Builtin_GLSL_Type::glsl_imageBuffer,
            Builtin_GLSL_Type::glsl_subpassInput,
            Builtin_GLSL_Type::glsl_subpassInputMS,
            Builtin_GLSL_Type::glsl_isampler1D,
            Builtin_GLSL_Type::glsl_itexture1D,
            Builtin_GLSL_Type::glsl_iimage1D,
            Builtin_GLSL_Type::glsl_isampler1DArray,
            Builtin_GLSL_Type::glsl_itexture1DArray,
            Builtin_GLSL_Type::glsl_iimage1DArray,
            Builtin_GLSL_Type::glsl_isampler2D,
            Builtin_GLSL_Type::glsl_itexture2D,
            Builtin_GLSL_Type::glsl_iimage2D,
            Builtin_GLSL_Type::glsl_isampler2DArray,
            Builtin_GLSL_Type::glsl_itexture2DArray,
            Builtin_GLSL_Type::glsl_iimage2DArray,
            Builtin_GLSL_Type::glsl_isampler2DMS,
            Builtin_GLSL_Type::glsl_itexture2DMS,
            Builtin_GLSL_Type::glsl_iimage2DMS,
            Builtin_GLSL_Type::glsl_isampler2DMSArray,
            Builtin_GLSL_Type::glsl_itexture2DMSArray,
            Builtin_GLSL_Type::glsl_iimage2DMSArray,
            Builtin_GLSL_Type::glsl_isampler2DRect,
            Builtin_GLSL_Type::glsl_itexture2DRect,
            Builtin_GLSL_Type::glsl_iimage2DRect,
            Builtin_GLSL_Type::glsl_isampler3D,
            Builtin_GLSL_Type::glsl_itexture3D,
            Builtin_GLSL_Type::glsl_iimage3D,
            Builtin_GLSL_Type::glsl_isamplerCube,
            Builtin_GLSL_Type::glsl_itextureCube,
            Builtin_GLSL_Type::glsl_iimageCube,
            Builtin_GLSL_Type::glsl_isamplerCubeArray,
            Builtin_GLSL_Type::glsl_itextureCubeArray,
            Builtin_GLSL_Type::glsl_iimageCubeArray,
            Builtin_GLSL_Type::glsl_isamplerBuffer,
            Builtin_GLSL_Type::glsl_itextureBuffer,
            Builtin_GLSL_Type::glsl_iimageBuffer,
            Builtin_GLSL_Type::glsl_isubpassInput,
            Builtin_GLSL_Type::glsl_isubpassInputMS,
            Builtin_GLSL_Type::glsl_usampler1D,
            Builtin_GLSL_Type::glsl_utexture1D,
            Builtin_GLSL_Type::glsl_uimage1D,
            Builtin_GLSL_Type::glsl_usampler1DArray,
            Builtin_GLSL_Type::glsl_utexture1DArray,
            Builtin_GLSL_Type::glsl_uimage1DArray,
            Builtin_GLSL_Type::glsl_usampler2D,
            Builtin_GLSL_Type::glsl_utexture2D,
            Builtin_GLSL_Type::glsl_uimage2D,
            Builtin_GLSL_Type::glsl_usampler2DArray,
            Builtin_GLSL_Type::glsl_utexture2DArray,
            Builtin_GLSL_Type::glsl_uimage2DArray,
            Builtin_GLSL_Type::glsl_usampler2DMS,
            Builtin_GLSL_Type::glsl_utexture2DMS,
            Builtin_GLSL_Type::glsl_uimage2DMS,
            Builtin_GLSL_Type::glsl_usampler2DMSArray,
            Builtin_GLSL_Type::glsl_utexture2DMSArray,
            Builtin_GLSL_Type::glsl_uimage2DMSArray,
            Builtin_GLSL_Type::glsl_usampler2DRect,
            Builtin_GLSL_Type::glsl_utexture2DRect,
            Builtin_GLSL_Type::glsl_uimage2DRect,
            Builtin_GLSL_Type::glsl_usampler3D,
            Builtin_GLSL_Type::glsl_utexture3D,
            Builtin_GLSL_Type::glsl_uimage3D,
            Builtin_GLSL_Type::glsl_usamplerCube,
            Builtin_GLSL_Type::glsl_utextureCube,
            Builtin_GLSL_Type::glsl_uimageCube,
            Builtin_GLSL_Type::glsl_usamplerCubeArray,
            Builtin_GLSL_Type::glsl_utextureCubeArray,
            Builtin_GLSL_Type::glsl_uimageCubeArray,
            Builtin_GLSL_Type::glsl_usamplerBuffer,
            Builtin_GLSL_Type::glsl_utextureBuffer,
            Builtin_GLSL_Type::glsl_uimageBuffer,
            Builtin_GLSL_Type::glsl_usubpassInput,
            Builtin_GLSL_Type::glsl_usubpassInputMS,
            Builtin_GLSL_Type::glsl_sampler,
            Builtin_GLSL_Type::glsl_samplerShadow,
        };

        constexpr i64 array_size = sizeof(builtin_types_strings) / sizeof(anton::String_View);
        for(i64 i = 0; i < array_size; ++i) {
            if(type == builtin_types_strings[i]) {
                return builtin_types[i];
            }
        }

        return anton::null_optional;
    }

    anton::String_View stringify(Builtin_GLSL_Type type) {
        switch(type) {
            case Builtin_GLSL_Type::glsl_void:
                return u8"void";
            case Builtin_GLSL_Type::glsl_bool:
                return u8"bool";
            case Builtin_GLSL_Type::glsl_int:
                return u8"int";
            case Builtin_GLSL_Type::glsl_uint:
                return u8"uint";
            case Builtin_GLSL_Type::glsl_float:
                return u8"float";
            case Builtin_GLSL_Type::glsl_double:
                return u8"double";
            case Builtin_GLSL_Type::glsl_vec2:
                return u8"vec2";
            case Builtin_GLSL_Type::glsl_vec3:
                return u8"vec3";
            case Builtin_GLSL_Type::glsl_vec4:
                return u8"vec4";
            case Builtin_GLSL_Type::glsl_dvec2:
                return u8"dvec2";
            case Builtin_GLSL_Type::glsl_dvec3:
                return u8"dvec3";
            case Builtin_GLSL_Type::glsl_dvec4:
                return u8"dvec4";
            case Builtin_GLSL_Type::glsl_bvec2:
                return u8"bvec2";
            case Builtin_GLSL_Type::glsl_bvec3:
                return u8"bvec3";
            case Builtin_GLSL_Type::glsl_bvec4:
                return u8"bvec4";
            case Builtin_GLSL_Type::glsl_ivec2:
                return u8"ivec2";
            case Builtin_GLSL_Type::glsl_ivec3:
                return u8"ivec3";
            case Builtin_GLSL_Type::glsl_ivec4:
                return u8"ivec4";
            case Builtin_GLSL_Type::glsl_uvec2:
                return u8"uvec2";
            case Builtin_GLSL_Type::glsl_uvec3:
                return u8"uvec3";
            case Builtin_GLSL_Type::glsl_uvec4:
                return u8"uvec4";
            case Builtin_GLSL_Type::glsl_mat2:
                return u8"mat2";
            case Builtin_GLSL_Type::glsl_mat3:
                return u8"mat3";
            case Builtin_GLSL_Type::glsl_mat4:
                return u8"mat4";
            case Builtin_GLSL_Type::glsl_mat2x3:
                return u8"mat2x3";
            case Builtin_GLSL_Type::glsl_mat2x4:
                return u8"mat2x4";
            case Builtin_GLSL_Type::glsl_mat3x2:
                return u8"mat3x2";
            case Builtin_GLSL_Type::glsl_mat3x4:
                return u8"mat3x4";
            case Builtin_GLSL_Type::glsl_mat4x2:
                return u8"mat4x2";
            case Builtin_GLSL_Type::glsl_mat4x3:
                return u8"mat4x3";
            case Builtin_GLSL_Type::glsl_dmat2:
                return u8"dmat2";
            case Builtin_GLSL_Type::glsl_dmat3:
                return u8"dmat3";
            case Builtin_GLSL_Type::glsl_dmat4:
                return u8"dmat4";
            case Builtin_GLSL_Type::glsl_dmat2x3:
                return u8"dmat2x3";
            case Builtin_GLSL_Type::glsl_dmat2x4:
                return u8"dmat2x4";
            case Builtin_GLSL_Type::glsl_dmat3x2:
                return u8"dmat3x2";
            case Builtin_GLSL_Type::glsl_dmat3x4:
                return u8"dmat3x4";
            case Builtin_GLSL_Type::glsl_dmat4x2:
                return u8"dmat4x2";
            case Builtin_GLSL_Type::glsl_dmat4x3:
                return u8"dmat4x3";
            case Builtin_GLSL_Type::glsl_sampler1D:
                return u8"sampler1D";
            case Builtin_GLSL_Type::glsl_texture1D:
                return u8"texture1D";
            case Builtin_GLSL_Type::glsl_image1D:
                return u8"image1D";
            case Builtin_GLSL_Type::glsl_sampler1DShadow:
                return u8"sampler1DShadow";
            case Builtin_GLSL_Type::glsl_sampler1DArray:
                return u8"sampler1DArray";
            case Builtin_GLSL_Type::glsl_texture1DArray:
                return u8"texture1DArray";
            case Builtin_GLSL_Type::glsl_image1DArray:
                return u8"image1DArray";
            case Builtin_GLSL_Type::glsl_sampler1DArrayShadow:
                return u8"sampler1DArrayShadow";
            case Builtin_GLSL_Type::glsl_sampler2D:
                return u8"sampler2D";
            case Builtin_GLSL_Type::glsl_texture2D:
                return u8"texture2D";
            case Builtin_GLSL_Type::glsl_image2D:
                return u8"image2D";
            case Builtin_GLSL_Type::glsl_sampler2DShadow:
                return u8"sampler2DShadow";
            case Builtin_GLSL_Type::glsl_sampler2DArray:
                return u8"sampler2DArray";
            case Builtin_GLSL_Type::glsl_texture2DArray:
                return u8"texture2DArray";
            case Builtin_GLSL_Type::glsl_image2DArray:
                return u8"image2DArray";
            case Builtin_GLSL_Type::glsl_sampler2DArrayShadow:
                return u8"sampler2DArrayShadow";
            case Builtin_GLSL_Type::glsl_sampler2DMS:
                return u8"sampler2DMS";
            case Builtin_GLSL_Type::glsl_texture2DMS:
                return u8"texture2DMS";
            case Builtin_GLSL_Type::glsl_image2DMS:
                return u8"image2DMS";
            case Builtin_GLSL_Type::glsl_sampler2DMSArray:
                return u8"sampler2DMSArray";
            case Builtin_GLSL_Type::glsl_texture2DMSArray:
                return u8"texture2DMSArray";
            case Builtin_GLSL_Type::glsl_image2DMSArray:
                return u8"image2DMSArray";
            case Builtin_GLSL_Type::glsl_sampler2DRect:
                return u8"sampler2DRect";
            case Builtin_GLSL_Type::glsl_texture2DRect:
                return u8"texture2DRect";
            case Builtin_GLSL_Type::glsl_image2DRect:
                return u8"image2DRect";
            case Builtin_GLSL_Type::glsl_sampler2DRectShadow:
                return u8"sampler2DRectShadow";
            case Builtin_GLSL_Type::glsl_sampler3D:
                return u8"sampler3D";
            case Builtin_GLSL_Type::glsl_texture3D:
                return u8"texture3D";
            case Builtin_GLSL_Type::glsl_image3D:
                return u8"image3D";
            case Builtin_GLSL_Type::glsl_samplerCube:
                return u8"samplerCube";
            case Builtin_GLSL_Type::glsl_textureCube:
                return u8"textureCube";
            case Builtin_GLSL_Type::glsl_imageCube:
                return u8"imageCube";
            case Builtin_GLSL_Type::glsl_samplerCubeShadow:
                return u8"samplerCubeShadow";
            case Builtin_GLSL_Type::glsl_samplerCubeArray:
                return u8"samplerCubeArray";
            case Builtin_GLSL_Type::glsl_textureCubeArray:
                return u8"textureCubeArray";
            case Builtin_GLSL_Type::glsl_imageCubeArray:
                return u8"imageCubeArray";
            case Builtin_GLSL_Type::glsl_samplerCubeArrayShadow:
                return u8"samplerCubeArrayShadow";
            case Builtin_GLSL_Type::glsl_samplerBuffer:
                return u8"samplerBuffer";
            case Builtin_GLSL_Type::glsl_textureBuffer:
                return u8"textureBuffer";
            case Builtin_GLSL_Type::glsl_imageBuffer:
                return u8"imageBuffer";
            case Builtin_GLSL_Type::glsl_subpassInput:
                return u8"subpassInput";
            case Builtin_GLSL_Type::glsl_subpassInputMS:
                return u8"subpassInputMS";
            case Builtin_GLSL_Type::glsl_isampler1D:
                return u8"isampler1D";
            case Builtin_GLSL_Type::glsl_itexture1D:
                return u8"itexture1D";
            case Builtin_GLSL_Type::glsl_iimage1D:
                return u8"iimage1D";
            case Builtin_GLSL_Type::glsl_isampler1DArray:
                return u8"isampler1DArray";
            case Builtin_GLSL_Type::glsl_itexture1DArray:
                return u8"itexture1DArray";
            case Builtin_GLSL_Type::glsl_iimage1DArray:
                return u8"iimage1DArray";
            case Builtin_GLSL_Type::glsl_isampler2D:
                return u8"isampler2D";
            case Builtin_GLSL_Type::glsl_itexture2D:
                return u8"itexture2D";
            case Builtin_GLSL_Type::glsl_iimage2D:
                return u8"iimage2D";
            case Builtin_GLSL_Type::glsl_isampler2DArray:
                return u8"isampler2DArray";
            case Builtin_GLSL_Type::glsl_itexture2DArray:
                return u8"itexture2DArray";
            case Builtin_GLSL_Type::glsl_iimage2DArray:
                return u8"iimage2DArray";
            case Builtin_GLSL_Type::glsl_isampler2DMS:
                return u8"isampler2DMS";
            case Builtin_GLSL_Type::glsl_itexture2DMS:
                return u8"itexture2DMS";
            case Builtin_GLSL_Type::glsl_iimage2DMS:
                return u8"iimage2DMS";
            case Builtin_GLSL_Type::glsl_isampler2DMSArray:
                return u8"isampler2DMSArray";
            case Builtin_GLSL_Type::glsl_itexture2DMSArray:
                return u8"itexture2DMSArray";
            case Builtin_GLSL_Type::glsl_iimage2DMSArray:
                return u8"iimage2DMSArray";
            case Builtin_GLSL_Type::glsl_isampler2DRect:
                return u8"isampler2DRect";
            case Builtin_GLSL_Type::glsl_itexture2DRect:
                return u8"itexture2DRect";
            case Builtin_GLSL_Type::glsl_iimage2DRect:
                return u8"iimage2DRect";
            case Builtin_GLSL_Type::glsl_isampler3D:
                return u8"isampler3D";
            case Builtin_GLSL_Type::glsl_itexture3D:
                return u8"itexture3D";
            case Builtin_GLSL_Type::glsl_iimage3D:
                return u8"iimage3D";
            case Builtin_GLSL_Type::glsl_isamplerCube:
                return u8"isamplerCube";
            case Builtin_GLSL_Type::glsl_itextureCube:
                return u8"itextureCube";
            case Builtin_GLSL_Type::glsl_iimageCube:
                return u8"iimageCube";
            case Builtin_GLSL_Type::glsl_isamplerCubeArray:
                return u8"isamplerCubeArray";
            case Builtin_GLSL_Type::glsl_itextureCubeArray:
                return u8"itextureCubeArray";
            case Builtin_GLSL_Type::glsl_iimageCubeArray:
                return u8"iimageCubeArray";
            case Builtin_GLSL_Type::glsl_isamplerBuffer:
                return u8"isamplerBuffer";
            case Builtin_GLSL_Type::glsl_itextureBuffer:
                return u8"itextureBuffer";
            case Builtin_GLSL_Type::glsl_iimageBuffer:
                return u8"iimageBuffer";
            case Builtin_GLSL_Type::glsl_isubpassInput:
                return u8"isubpassInput";
            case Builtin_GLSL_Type::glsl_isubpassInputMS:
                return u8"isubpassInputMS";
            case Builtin_GLSL_Type::glsl_usampler1D:
                return u8"usampler1D";
            case Builtin_GLSL_Type::glsl_utexture1D:
                return u8"utexture1D";
            case Builtin_GLSL_Type::glsl_uimage1D:
                return u8"uimage1D";
            case Builtin_GLSL_Type::glsl_usampler1DArray:
                return u8"usampler1DArray";
            case Builtin_GLSL_Type::glsl_utexture1DArray:
                return u8"utexture1DArray";
            case Builtin_GLSL_Type::glsl_uimage1DArray:
                return u8"uimage1DArray";
            case Builtin_GLSL_Type::glsl_usampler2D:
                return u8"usampler2D";
            case Builtin_GLSL_Type::glsl_utexture2D:
                return u8"utexture2D";
            case Builtin_GLSL_Type::glsl_uimage2D:
                return u8"uimage2D";
            case Builtin_GLSL_Type::glsl_usampler2DArray:
                return u8"usampler2DArray";
            case Builtin_GLSL_Type::glsl_utexture2DArray:
                return u8"utexture2DArray";
            case Builtin_GLSL_Type::glsl_uimage2DArray:
                return u8"uimage2DArray";
            case Builtin_GLSL_Type::glsl_usampler2DMS:
                return u8"usampler2DMS";
            case Builtin_GLSL_Type::glsl_utexture2DMS:
                return u8"utexture2DMS";
            case Builtin_GLSL_Type::glsl_uimage2DMS:
                return u8"uimage2DMS";
            case Builtin_GLSL_Type::glsl_usampler2DMSArray:
                return u8"usampler2DMSArray";
            case Builtin_GLSL_Type::glsl_utexture2DMSArray:
                return u8"utexture2DMSArray";
            case Builtin_GLSL_Type::glsl_uimage2DMSArray:
                return u8"uimage2DMSArray";
            case Builtin_GLSL_Type::glsl_usampler2DRect:
                return u8"usampler2DRect";
            case Builtin_GLSL_Type::glsl_utexture2DRect:
                return u8"utexture2DRect";
            case Builtin_GLSL_Type::glsl_uimage2DRect:
                return u8"uimage2DRect";
            case Builtin_GLSL_Type::glsl_usampler3D:
                return u8"usampler3D";
            case Builtin_GLSL_Type::glsl_utexture3D:
                return u8"utexture3D";
            case Builtin_GLSL_Type::glsl_uimage3D:
                return u8"uimage3D";
            case Builtin_GLSL_Type::glsl_usamplerCube:
                return u8"usamplerCube";
            case Builtin_GLSL_Type::glsl_utextureCube:
                return u8"utextureCube";
            case Builtin_GLSL_Type::glsl_uimageCube:
                return u8"uimageCube";
            case Builtin_GLSL_Type::glsl_usamplerCubeArray:
                return u8"usamplerCubeArray";
            case Builtin_GLSL_Type::glsl_utextureCubeArray:
                return u8"utextureCubeArray";
            case Builtin_GLSL_Type::glsl_uimageCubeArray:
                return u8"uimageCubeArray";
            case Builtin_GLSL_Type::glsl_usamplerBuffer:
                return u8"usamplerBuffer";
            case Builtin_GLSL_Type::glsl_utextureBuffer:
                return u8"utextureBuffer";
            case Builtin_GLSL_Type::glsl_uimageBuffer:
                return u8"uimageBuffer";
            case Builtin_GLSL_Type::glsl_usubpassInput:
                return u8"usubpassInput";
            case Builtin_GLSL_Type::glsl_usubpassInputMS:
                return u8"usubpassInputMS";
            case Builtin_GLSL_Type::glsl_sampler:
                return u8"sampler";
            case Builtin_GLSL_Type::glsl_samplerShadow:
                return u8"samplerShadow";
        }
    }

    bool operator==(Type const& lhs, Type const& rhs) {
        if(lhs.node_type != rhs.node_type) {
            return false;
        }

        AST_Node_Type const type = lhs.node_type;
        switch(type) {
            case AST_Node_Type::builtin_type: {
                Builtin_Type const& lhs_t = static_cast<Builtin_Type const&>(lhs);
                Builtin_Type const& rhs_t = static_cast<Builtin_Type const&>(rhs);
                return lhs_t.type == rhs_t.type;
            }

            case AST_Node_Type::user_defined_type: {
                User_Defined_Type const& lhs_t = static_cast<User_Defined_Type const&>(lhs);
                User_Defined_Type const& rhs_t = static_cast<User_Defined_Type const&>(rhs);
                return lhs_t.identifier == rhs_t.identifier;
            }

            case AST_Node_Type::array_type: {
                Array_Type const& lhs_t = static_cast<Array_Type const&>(lhs);
                Array_Type const& rhs_t = static_cast<Array_Type const&>(rhs);
                if(*lhs_t.base != *rhs_t.base) {
                    return false;
                }

                if(!lhs_t.size && !rhs_t.size) {
                    return true;
                }

                return lhs_t.size->value == rhs_t.size->value;
            }

            default:
                ANTON_UNREACHABLE();
        }
    }

    bool operator!=(Type const& lhs, Type const& rhs) {
        return !(lhs == rhs);
    }

    bool is_void(Type const& type) {
        return type.node_type == AST_Node_Type::builtin_type && static_cast<Builtin_Type const&>(type).type == Builtin_GLSL_Type::glsl_void;
    }

    bool is_opaque_type(Type const& type) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type ||
                         type.node_type == AST_Node_Type::array_type,
                     u8"unknown ast node type");
        if(type.node_type == AST_Node_Type::builtin_type) {
            Builtin_Type const& t = static_cast<Builtin_Type const&>(type);
            return is_opaque_type(t.type);
        } else if(type.node_type == AST_Node_Type::user_defined_type) {
            return false;
        } else {
            Array_Type const& t = static_cast<Array_Type const&>(type);
            return is_opaque_type(*t.base);
        }
    }

    bool is_unsized_array(Type const& type) {
        return type.node_type == AST_Node_Type::array_type && !static_cast<Array_Type const&>(type).size;
    }

    bool is_sized_array(Type const& type) {
        return type.node_type == AST_Node_Type::array_type && static_cast<Array_Type const&>(type).size;
    }

    bool is_image_type(Type const& type) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type ||
                         type.node_type == AST_Node_Type::array_type,
                     u8"unknown ast node type");

        if(type.node_type == AST_Node_Type::user_defined_type) {
            return false;
        }

        if(type.node_type == AST_Node_Type::builtin_type) {
            Builtin_Type const& t = (Builtin_Type const&)type;
            return is_image_type(t.type);
        }

        Array_Type const& t = (Array_Type const&)type;
        return is_image_type(*t.base);
    }

    anton::String stringify_type(Type const& type) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type ||
                         type.node_type == AST_Node_Type::array_type,
                     u8"unknown ast node type");
        if(type.node_type == AST_Node_Type::builtin_type) {
            Builtin_Type const& t = static_cast<Builtin_Type const&>(type);
            anton::String_View sv = stringify(t.type);
            return anton::String(sv);
        } else if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& t = static_cast<User_Defined_Type const&>(type);
            return t.identifier;
        } else {
            Array_Type const& t = static_cast<Array_Type const&>(type);
            anton::String str = stringify_type(*t.base);
            str += u8"[";
            if(t.size) {
                str += t.size->value;
            }
            str += u8"]";
            return str;
        }
    }

    anton::String_View stringify(Image_Layout_Type type) {
        switch(type) {
            case Image_Layout_Type::rgba32f:
                return u8"rgba32f";
            case Image_Layout_Type::rgba16f:
                return u8"rgba16f";
            case Image_Layout_Type::rg32f:
                return u8"rg32f";
            case Image_Layout_Type::rg16f:
                return u8"rg16f";
            case Image_Layout_Type::r11f_g11f_b10f:
                return u8"r11f_g11f_b10f";
            case Image_Layout_Type::r32f:
                return u8"r32f";
            case Image_Layout_Type::r16f:
                return u8"r16f";
            case Image_Layout_Type::rgba16:
                return u8"rgba16";
            case Image_Layout_Type::rgb10_a2:
                return u8"rgb10_a2";
            case Image_Layout_Type::rgba8:
                return u8"rgba8";
            case Image_Layout_Type::rg16:
                return u8"rg16";
            case Image_Layout_Type::rg8:
                return u8"rg8";
            case Image_Layout_Type::r16:
                return u8"r16";
            case Image_Layout_Type::r8:
                return u8"r8";
            case Image_Layout_Type::rgba16_snorm:
                return u8"rgba16_snorm";
            case Image_Layout_Type::rgba8_snorm:
                return u8"rgba8_snorm";
            case Image_Layout_Type::rg16_snorm:
                return u8"rg16_snorm";
            case Image_Layout_Type::rg8_snorm:
                return u8"rg8_snorm";
            case Image_Layout_Type::r16_snorm:
                return u8"r16_snorm";
            case Image_Layout_Type::r8_snorm:
                return u8"r8_snorm";
            case Image_Layout_Type::rgba32i:
                return u8"rgba32i";
            case Image_Layout_Type::rgba16i:
                return u8"rgba16i";
            case Image_Layout_Type::rgba8i:
                return u8"rgba8i";
            case Image_Layout_Type::rg32i:
                return u8"rg32i";
            case Image_Layout_Type::rg16i:
                return u8"rg16i";
            case Image_Layout_Type::rg8i:
                return u8"rg8i";
            case Image_Layout_Type::r32i:
                return u8"r32i";
            case Image_Layout_Type::r16i:
                return u8"r16i";
            case Image_Layout_Type::r8i:
                return u8"r8i";
            case Image_Layout_Type::rgba32ui:
                return u8"rgba32ui";
            case Image_Layout_Type::rgba16ui:
                return u8"rgba16ui";
            case Image_Layout_Type::rgb10_a2ui:
                return u8"rgb10_a2ui";
            case Image_Layout_Type::rgba8ui:
                return u8"rgba8ui";
            case Image_Layout_Type::rg32ui:
                return u8"rg32ui";
            case Image_Layout_Type::rg16ui:
                return u8"rg16ui";
            case Image_Layout_Type::rg8ui:
                return u8"rg8ui";
            case Image_Layout_Type::r32ui:
                return u8"r32ui";
            case Image_Layout_Type::r16ui:
                return u8"r16ui";
            case Image_Layout_Type::r8ui:
                return u8"r8ui";
        }
    }

    bool is_sourced_parameter(Function_Parameter const& parameter) {
        return parameter.source;
    }

    bool is_vertex_input_parameter(Function_Parameter const& parameter) {
        return parameter.source && parameter.source->value == u8"in";
    }

    template<typename T>
    anton::Array<Owning_Ptr<T>> clone(anton::Array<Owning_Ptr<T>> const& array) {
        anton::Array<Owning_Ptr<T>> copy{anton::reserve, array.size()};
        for(Owning_Ptr<T> const& object: array) {
            copy.emplace_back(object->clone());
        }
        return copy;
    }

    AST_Node::AST_Node(Source_Info const& source_info, AST_Node_Type node_type): source_info(source_info), node_type(node_type) {}

    Owning_Ptr<AST_Node> AST_Node::clone() const {
        return Owning_Ptr{_clone()};
    }

    Identifier::Identifier(anton::String value, Source_Info const& source_info): AST_Node(source_info, AST_Node_Type::identifier), value(ANTON_MOV(value)) {}

    Owning_Ptr<Identifier> Identifier::clone() const {
        return Owning_Ptr{_clone()};
    }

    Identifier* Identifier::_clone() const {
        return new Identifier(value, source_info);
    }

    Owning_Ptr<Type> Type::clone() const {
        return Owning_Ptr{_clone()};
    }

    Builtin_Type::Builtin_Type(Builtin_GLSL_Type type, Source_Info const& source_info): Type(source_info, AST_Node_Type::builtin_type), type(type) {}

    Owning_Ptr<Builtin_Type> Builtin_Type::clone() const {
        return Owning_Ptr(_clone());
    }

    Builtin_Type* Builtin_Type::_clone() const {
        return new Builtin_Type(type, source_info);
    }

    User_Defined_Type::User_Defined_Type(anton::String identifier, Source_Info const& source_info)
        : Type(source_info, AST_Node_Type::user_defined_type), identifier(ANTON_MOV(identifier)) {}

    Owning_Ptr<User_Defined_Type> User_Defined_Type::clone() const {
        return Owning_Ptr{_clone()};
    }

    User_Defined_Type* User_Defined_Type::_clone() const {
        return new User_Defined_Type(identifier, source_info);
    }

    Array_Type::Array_Type(Owning_Ptr<Type> base, Owning_Ptr<Integer_Literal> size, Source_Info const& source_info)
        : Type(source_info, AST_Node_Type::array_type), base(ANTON_MOV(base)), size(ANTON_MOV(size)) {}

    Owning_Ptr<Array_Type> Array_Type::clone() const {
        return Owning_Ptr{_clone()};
    }

    Array_Type* Array_Type::_clone() const {
        if(size) {
            return new Array_Type(base->clone(), size->clone(), source_info);
        } else {
            return new Array_Type(base->clone(), nullptr, source_info);
        }
    }

    Owning_Ptr<Declaration> Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Declaration_If::Declaration_If(Owning_Ptr<Expression> condition, Declaration_List true_declarations, Declaration_List false_declarations,
                                   Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::declaration_if), condition(ANTON_MOV(condition)), true_declarations(ANTON_MOV(true_declarations)),
          false_declarations(ANTON_MOV(false_declarations)) {}

    Owning_Ptr<Declaration_If> Declaration_If::clone() const {
        return Owning_Ptr{_clone()};
    }

    Declaration_If* Declaration_If::_clone() const {
        return new Declaration_If(condition->clone(), vush::clone(true_declarations), vush::clone(false_declarations), source_info);
    }

    Import_Declaration::Import_Declaration(Owning_Ptr<String_Literal> path, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::import_declaration), path(ANTON_MOV(path)) {}

    Owning_Ptr<Import_Declaration> Import_Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Import_Declaration* Import_Declaration::_clone() const {
        return new Import_Declaration(path->clone(), source_info);
    }

    Variable_Declaration::Variable_Declaration(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer,
                                               Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::variable_declaration), type(ANTON_MOV(type)), identifier(ANTON_MOV(identifier)),
          initializer(ANTON_MOV(initializer)) {}

    Owning_Ptr<Variable_Declaration> Variable_Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Variable_Declaration* Variable_Declaration::_clone() const {
        if(initializer) {
            return new Variable_Declaration(type->clone(), identifier->clone(), initializer->clone(), source_info);
        } else {
            return new Variable_Declaration(type->clone(), identifier->clone(), nullptr, source_info);
        }
    }

    Constant_Declaration::Constant_Declaration(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer,
                                               Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::constant_declaration), type(ANTON_MOV(type)), identifier(ANTON_MOV(identifier)),
          initializer(ANTON_MOV(initializer)) {}

    Owning_Ptr<Constant_Declaration> Constant_Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Constant_Declaration* Constant_Declaration::_clone() const {
        if(initializer) {
            return new Constant_Declaration(type->clone(), identifier->clone(), initializer->clone(), source_info);
        } else {
            return new Constant_Declaration(type->clone(), identifier->clone(), nullptr, source_info);
        }
    }

    Struct_Member::Struct_Member(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer, Interpolation interpolation,
                                 bool invariant, Source_Info const& source_info)
        : AST_Node(source_info, AST_Node_Type::struct_member), type(ANTON_MOV(type)), identifier(ANTON_MOV(identifier)), initializer(ANTON_MOV(initializer)),
          interpolation(interpolation), invariant(invariant) {}

    Owning_Ptr<Struct_Member> Struct_Member::clone() const {
        return Owning_Ptr{_clone()};
    }

    Struct_Member* Struct_Member::_clone() const {
        return new Struct_Member(type->clone(), identifier->clone(), initializer->clone(), interpolation, invariant, source_info);
    }

    Struct_Declaration::Struct_Declaration(Owning_Ptr<Identifier> identifier, anton::Array<Owning_Ptr<Struct_Member>> members, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::struct_declaration), members(ANTON_MOV(members)), identifier(ANTON_MOV(identifier)) {}

    Owning_Ptr<Struct_Declaration> Struct_Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Struct_Declaration* Struct_Declaration::_clone() const {
        return new Struct_Declaration(identifier->clone(), vush::clone(members), source_info);
    }

    Settings_Declaration::Settings_Declaration(Owning_Ptr<Identifier> pass_name, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::settings_declaration), pass_name(ANTON_MOV(pass_name)) {}

    Settings_Declaration::Settings_Declaration(Owning_Ptr<Identifier> pass_name, anton::Array<Setting_Key_Value> settings, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::settings_declaration), pass_name(ANTON_MOV(pass_name)), settings(ANTON_MOV(settings)) {}

    Owning_Ptr<Settings_Declaration> Settings_Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Settings_Declaration* Settings_Declaration::_clone() const {
        return new Settings_Declaration(pass_name->clone(), settings, source_info);
    }

    Owning_Ptr<Attribute> Attribute::clone() const {
        return Owning_Ptr{_clone()};
    }

    Workgroup_Attribute::Workgroup_Attribute(Owning_Ptr<Integer_Literal> x, Owning_Ptr<Integer_Literal> y, Owning_Ptr<Integer_Literal> z,
                                             Source_Info const& source_info)
        : Attribute(source_info, AST_Node_Type::workgroup_attribute), x(ANTON_MOV(x)), y(ANTON_MOV(y)), z(ANTON_MOV(z)) {}

    Owning_Ptr<Workgroup_Attribute> Workgroup_Attribute::clone() const {
        return Owning_Ptr{_clone()};
    }

    Workgroup_Attribute* Workgroup_Attribute::_clone() const {
        return new Workgroup_Attribute(x->clone(), y->clone(), z->clone(), source_info);
    }

    Owning_Ptr<Function_Parameter_Node> Function_Parameter_Node::clone() const {
        return Owning_Ptr{_clone()};
    }

    Function_Param_If::Function_Param_If(Owning_Ptr<Expression> condition, Owning_Ptr<Function_Parameter_Node> true_param,
                                         Owning_Ptr<Function_Parameter_Node> false_param, Source_Info const& source_info)
        : Function_Parameter_Node(source_info, AST_Node_Type::function_param_if), condition(ANTON_MOV(condition)), true_param(ANTON_MOV(true_param)),
          false_param(ANTON_MOV(false_param)) {}

    Owning_Ptr<Function_Param_If> Function_Param_If::clone() const {
        return Owning_Ptr{_clone()};
    }

    Function_Param_If* Function_Param_If::_clone() const {
        if(false_param) {
            return new Function_Param_If(condition->clone(), true_param->clone(), false_param->clone(), source_info);
        } else {
            return new Function_Param_If(condition->clone(), true_param->clone(), nullptr, source_info);
        }
    }

    Owning_Ptr<Layout_Qualifier> Layout_Qualifier::clone() const {
        return Owning_Ptr{_clone()};
    }

    Image_Layout_Qualifier::Image_Layout_Qualifier(Image_Layout_Type type, Source_Info const& source_info)
        : Layout_Qualifier(source_info, AST_Node_Type::image_layout_qualifier), type(type) {}

    Owning_Ptr<Image_Layout_Qualifier> Image_Layout_Qualifier::clone() const {
        return Owning_Ptr{_clone()};
    }

    Image_Layout_Qualifier* Image_Layout_Qualifier::_clone() const {
        return new Image_Layout_Qualifier(type, source_info);
    }

    Function_Parameter::Function_Parameter(Owning_Ptr<Identifier> identifier, Owning_Ptr<Type> type, Owning_Ptr<Identifier> source,
                                           Owning_Ptr<Image_Layout_Qualifier> image_layout, Source_Info const& source_info)
        : Function_Parameter_Node(source_info, AST_Node_Type::function_parameter), type(ANTON_MOV(type)), identifier(ANTON_MOV(identifier)),
          source(ANTON_MOV(source)), image_layout(ANTON_MOV(image_layout)) {}

    Owning_Ptr<Function_Parameter> Function_Parameter::clone() const {
        return Owning_Ptr{_clone()};
    }

    Function_Parameter* Function_Parameter::_clone() const {
        Owning_Ptr<Image_Layout_Qualifier> _image_layout = image_layout ? image_layout->clone() : nullptr;
        Owning_Ptr<Identifier> _source = source ? source->clone() : nullptr;
        return new Function_Parameter(identifier->clone(), type->clone(), ANTON_MOV(_source), ANTON_MOV(_image_layout), source_info);
    }

    Function_Declaration::Function_Declaration(Attribute_List attributes, Owning_Ptr<Type> return_type, Owning_Ptr<Identifier> identifier,
                                               Parameter_List parameters, Statement_List body, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::function_declaration), parameters(ANTON_MOV(parameters)), attributes(ANTON_MOV(attributes)),
          identifier(ANTON_MOV(identifier)), return_type(ANTON_MOV(return_type)), body(ANTON_MOV(body)) {}

    Owning_Ptr<Function_Declaration> Function_Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Function_Declaration* Function_Declaration::_clone() const {
        return new Function_Declaration(vush::clone(attributes), return_type->clone(), identifier->clone(), vush::clone(parameters), vush::clone(body),
                                        source_info);
    }

    Overloaded_Function_Declaration::Overloaded_Function_Declaration(Owning_Ptr<Identifier> identifier,
                                                                     anton::Array<Owning_Ptr<Function_Declaration>> overloads, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::overloaded_function_declaration), identifier(ANTON_MOV(identifier)), overloads(ANTON_MOV(overloads)) {}

    Owning_Ptr<Overloaded_Function_Declaration> Overloaded_Function_Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Overloaded_Function_Declaration* Overloaded_Function_Declaration::_clone() const {
        return new Overloaded_Function_Declaration(identifier->clone(), vush::clone(overloads), source_info);
    }

    Pass_Stage_Declaration::Pass_Stage_Declaration(Attribute_List attributes, Owning_Ptr<Type> return_type, Owning_Ptr<Identifier> pass_name,
                                                   Stage_Type stage_type, Parameter_List parameters, Statement_List body, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::pass_stage_declaration), parameters(ANTON_MOV(parameters)), attributes(ANTON_MOV(attributes)),
          body(ANTON_MOV(body)), pass_name(ANTON_MOV(pass_name)), return_type(ANTON_MOV(return_type)), stage_type(ANTON_MOV(stage_type)) {}

    Owning_Ptr<Pass_Stage_Declaration> Pass_Stage_Declaration::clone() const {
        return Owning_Ptr{_clone()};
    }

    Pass_Stage_Declaration* Pass_Stage_Declaration::_clone() const {
        return new Pass_Stage_Declaration(vush::clone(attributes), return_type->clone(), pass_name->clone(), stage_type, vush::clone(parameters),
                                          vush::clone(body), source_info);
    }

    Owning_Ptr<Expression> Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Expression_If::Expression_If(Owning_Ptr<Expression> condition, Owning_Ptr<Expression> true_expression, Owning_Ptr<Expression> false_expression,
                                 Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::expression_if), condition(ANTON_MOV(condition)), true_expression(ANTON_MOV(true_expression)),
          false_expression(ANTON_MOV(false_expression)) {}

    Owning_Ptr<Expression_If> Expression_If::clone() const {
        return Owning_Ptr{_clone()};
    }

    Expression_If* Expression_If::_clone() const {
        return new Expression_If(condition->clone(), true_expression->clone(), false_expression->clone(), source_info);
    }

    Identifier_Expression::Identifier_Expression(anton::String value, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::identifier_expression), value(ANTON_MOV(value)) {}

    Owning_Ptr<Identifier_Expression> Identifier_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Identifier_Expression* Identifier_Expression::_clone() const {
        return new Identifier_Expression(value, source_info);
    }

    Assignment_Expression::Assignment_Expression(Owning_Ptr<Expression> lhs, Owning_Ptr<Expression> rhs, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::assignment_expression), lhs(ANTON_MOV(lhs)), rhs(ANTON_MOV(rhs)) {}

    Owning_Ptr<Assignment_Expression> Assignment_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Assignment_Expression* Assignment_Expression::_clone() const {
        return new Assignment_Expression(lhs->clone(), rhs->clone(), source_info);
    }

    Arithmetic_Assignment_Expression::Arithmetic_Assignment_Expression(Arithmetic_Assignment_Type type, Owning_Ptr<Expression> lhs, Owning_Ptr<Expression> rhs,
                                                                       Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::arithmetic_assignment_expression), lhs(ANTON_MOV(lhs)), rhs(ANTON_MOV(rhs)), type(ANTON_MOV(type)) {}

    Owning_Ptr<Arithmetic_Assignment_Expression> Arithmetic_Assignment_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Arithmetic_Assignment_Expression* Arithmetic_Assignment_Expression::_clone() const {
        return new Arithmetic_Assignment_Expression(type, lhs->clone(), rhs->clone(), source_info);
    }

    Elvis_Expression::Elvis_Expression(Owning_Ptr<Expression> condition, Owning_Ptr<Expression> true_expression, Owning_Ptr<Expression> false_expression,
                                       Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::elvis_expression), condition(ANTON_MOV(condition)), true_expression(ANTON_MOV(true_expression)),
          false_expression(ANTON_MOV(false_expression)) {}

    Owning_Ptr<Elvis_Expression> Elvis_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Elvis_Expression* Elvis_Expression::_clone() const {
        return new Elvis_Expression(condition->clone(), true_expression->clone(), false_expression->clone(), source_info);
    }

    Binary_Expression::Binary_Expression(Binary_Expression_Type type, Owning_Ptr<Expression> lhs, Owning_Ptr<Expression> rhs, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::binary_expression), lhs(ANTON_MOV(lhs)), rhs(ANTON_MOV(rhs)), type(type) {}

    Owning_Ptr<Binary_Expression> Binary_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Binary_Expression* Binary_Expression::_clone() const {
        return new Binary_Expression(type, lhs->clone(), rhs->clone(), source_info);
    }

    Unary_Expression::Unary_Expression(Unary_Type type, Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::unary_expression), expression(ANTON_MOV(expression)), type(type) {}

    Owning_Ptr<Unary_Expression> Unary_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Unary_Expression* Unary_Expression::_clone() const {
        return new Unary_Expression(type, expression->clone(), source_info);
    }

    Prefix_Increment_Expression::Prefix_Increment_Expression(Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::prefix_increment_expression), expression(ANTON_MOV(expression)) {}

    Owning_Ptr<Prefix_Increment_Expression> Prefix_Increment_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Prefix_Increment_Expression* Prefix_Increment_Expression::_clone() const {
        return new Prefix_Increment_Expression(expression->clone(), source_info);
    }

    Prefix_Decrement_Expression::Prefix_Decrement_Expression(Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::prefix_decrement_expression), expression(ANTON_MOV(expression)) {}

    Owning_Ptr<Prefix_Decrement_Expression> Prefix_Decrement_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Prefix_Decrement_Expression* Prefix_Decrement_Expression::_clone() const {
        return new Prefix_Decrement_Expression(expression->clone(), source_info);
    }

    Function_Call_Expression::Function_Call_Expression(Owning_Ptr<Identifier> identifier, anton::Array<Owning_Ptr<Expression>> arguments,
                                                       Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::function_call_expression), arguments(ANTON_MOV(arguments)), identifier(ANTON_MOV(identifier)) {}

    Owning_Ptr<Function_Call_Expression> Function_Call_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Function_Call_Expression* Function_Call_Expression::_clone() const {
        return new Function_Call_Expression(identifier->clone(), vush::clone(arguments), source_info);
    }

    Member_Access_Expression::Member_Access_Expression(Owning_Ptr<Expression> base, Owning_Ptr<Identifier> member, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::member_access_expression), base(ANTON_MOV(base)), member(ANTON_MOV(member)) {}

    Owning_Ptr<Member_Access_Expression> Member_Access_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Member_Access_Expression* Member_Access_Expression::_clone() const {
        return new Member_Access_Expression(base->clone(), member->clone(), source_info);
    }

    Array_Access_Expression::Array_Access_Expression(Owning_Ptr<Expression> base, Owning_Ptr<Expression> index, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::array_access_expression), base(ANTON_MOV(base)), index(ANTON_MOV(index)) {}

    Owning_Ptr<Array_Access_Expression> Array_Access_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Array_Access_Expression* Array_Access_Expression::_clone() const {
        return new Array_Access_Expression(base->clone(), index->clone(), source_info);
    }

    Postfix_Increment_Expression::Postfix_Increment_Expression(Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::postfix_increment_expression), expression(ANTON_MOV(expression)) {}

    Owning_Ptr<Postfix_Increment_Expression> Postfix_Increment_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Postfix_Increment_Expression* Postfix_Increment_Expression::_clone() const {
        return new Postfix_Increment_Expression(expression->clone(), source_info);
    }

    Postfix_Decrement_Expression::Postfix_Decrement_Expression(Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::postfix_decrement_expression), expression(ANTON_MOV(expression)) {}

    Owning_Ptr<Postfix_Decrement_Expression> Postfix_Decrement_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Postfix_Decrement_Expression* Postfix_Decrement_Expression::_clone() const {
        return new Postfix_Decrement_Expression(expression->clone(), source_info);
    }

    Parenthesised_Expression::Parenthesised_Expression(Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::parenthesised_expression), expression(ANTON_MOV(expression)) {}

    Owning_Ptr<Parenthesised_Expression> Parenthesised_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Parenthesised_Expression* Parenthesised_Expression::_clone() const {
        return new Parenthesised_Expression(expression->clone(), source_info);
    }

    Reinterpret_Expression::Reinterpret_Expression(Owning_Ptr<Type> target_type, Owning_Ptr<Expression> source, Owning_Ptr<Expression> index,
                                                   Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::reinterpret_expression), target_type(ANTON_MOV(target_type)), source(ANTON_MOV(source)),
          index(ANTON_MOV(index)) {}

    Owning_Ptr<Reinterpret_Expression> Reinterpret_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Reinterpret_Expression* Reinterpret_Expression::_clone() const {
        return new Reinterpret_Expression(target_type->clone(), source->clone(), index->clone(), source_info);
    }

    Default_Expression::Default_Expression(Source_Info const& source_info): Expression(source_info, AST_Node_Type::default_expression) {}

    Owning_Ptr<Default_Expression> Default_Expression::clone() const {
        return Owning_Ptr{_clone()};
    }

    Default_Expression* Default_Expression::_clone() const {
        return new Default_Expression(source_info);
    }

    String_Literal::String_Literal(anton::String value, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::string_literal), value(ANTON_MOV(value)) {}

    Owning_Ptr<String_Literal> String_Literal::clone() const {
        return Owning_Ptr{_clone()};
    }

    String_Literal* String_Literal::_clone() const {
        return new String_Literal(value, source_info);
    }

    Bool_Literal::Bool_Literal(bool value, Source_Info const& source_info): Expression(source_info, AST_Node_Type::bool_literal), value(value) {}

    Owning_Ptr<Bool_Literal> Bool_Literal::clone() const {
        return Owning_Ptr{_clone()};
    }

    Bool_Literal* Bool_Literal::_clone() const {
        return new Bool_Literal(value, source_info);
    }

    Integer_Literal::Integer_Literal(anton::String value, Integer_Literal_Type type, Integer_Literal_Base base, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::integer_literal), value(ANTON_MOV(value)), type(type), base(base) {}

    Owning_Ptr<Integer_Literal> Integer_Literal::clone() const {
        return Owning_Ptr{_clone()};
    }

    Integer_Literal* Integer_Literal::_clone() const {
        return new Integer_Literal(value, type, base, source_info);
    }

    Float_Literal::Float_Literal(anton::String value, Float_Literal_Type type, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::float_literal), value(ANTON_MOV(value)), type(type) {}

    Owning_Ptr<Float_Literal> Float_Literal::clone() const {
        return Owning_Ptr{_clone()};
    }

    Float_Literal* Float_Literal::_clone() const {
        return new Float_Literal(value, type, source_info);
    }

    Owning_Ptr<Statement> Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Block_Statement::Block_Statement(Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::block_statement), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<Block_Statement> Block_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Block_Statement* Block_Statement::_clone() const {
        return new Block_Statement(vush::clone(statements), source_info);
    }

    If_Statement::If_Statement(Owning_Ptr<Expression> condition, Statement_List true_statements, Statement_List false_statements,
                               Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::if_statement), condition(ANTON_MOV(condition)), true_statements(ANTON_MOV(true_statements)),
          false_statements(ANTON_MOV(false_statements)) {}

    Owning_Ptr<If_Statement> If_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    If_Statement* If_Statement::_clone() const {
        return new If_Statement(condition->clone(), vush::clone(true_statements), vush::clone(false_statements), source_info);
    }

    Case_Statement::Case_Statement(Expression_List labels, Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::case_statement), labels(ANTON_MOV(labels)), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<Case_Statement> Case_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Case_Statement* Case_Statement::_clone() const {
        return new Case_Statement(vush::clone(labels), vush::clone(statements), source_info);
    }

    Switch_Statement::Switch_Statement(Owning_Ptr<Expression> match_expression, anton::Array<Owning_Ptr<Case_Statement>> cases, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::switch_statement), cases(ANTON_MOV(cases)), match_expression(ANTON_MOV(match_expression)) {}

    Owning_Ptr<Switch_Statement> Switch_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Switch_Statement* Switch_Statement::_clone() const {
        return new Switch_Statement(match_expression->clone(), vush::clone(cases), source_info);
    }

    For_Statement::For_Statement(Owning_Ptr<Variable_Declaration> declaration, Owning_Ptr<Expression> condition, Owning_Ptr<Expression> post_expression,
                                 Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::for_statement), declaration(ANTON_MOV(declaration)), condition(ANTON_MOV(condition)),
          post_expression(ANTON_MOV(post_expression)), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<For_Statement> For_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    For_Statement* For_Statement::_clone() const {
        return new For_Statement(declaration->clone(), condition->clone(), post_expression->clone(), vush::clone(statements), source_info);
    }

    While_Statement::While_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::while_statement), condition(ANTON_MOV(condition)), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<While_Statement> While_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    While_Statement* While_Statement::_clone() const {
        return new While_Statement(condition->clone(), vush::clone(statements), source_info);
    }

    Do_While_Statement::Do_While_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::do_while_statement), condition(ANTON_MOV(condition)), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<Do_While_Statement> Do_While_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Do_While_Statement* Do_While_Statement::_clone() const {
        return new Do_While_Statement(condition->clone(), vush::clone(statements), source_info);
    }

    Return_Statement::Return_Statement(Owning_Ptr<Expression> return_expression, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::return_statement), return_expression(ANTON_MOV(return_expression)) {}

    Owning_Ptr<Return_Statement> Return_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Return_Statement* Return_Statement::_clone() const {
        if(return_expression) {
            return new Return_Statement(return_expression->clone(), source_info);
        } else {
            return new Return_Statement(nullptr, source_info);
        }
    }

    Break_Statement::Break_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::break_statement) {}

    Owning_Ptr<Break_Statement> Break_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Break_Statement* Break_Statement::_clone() const {
        return new Break_Statement(source_info);
    }

    Continue_Statement::Continue_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::continue_statement) {}

    Owning_Ptr<Continue_Statement> Continue_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Continue_Statement* Continue_Statement::_clone() const {
        return new Continue_Statement(source_info);
    }

    Discard_Statement::Discard_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::discard_statement) {}

    Owning_Ptr<Discard_Statement> Discard_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Discard_Statement* Discard_Statement::_clone() const {
        return new Discard_Statement(source_info);
    }

    Declaration_Statement::Declaration_Statement(Owning_Ptr<Declaration> declaration, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::declaration_statement), declaration(ANTON_MOV(declaration)) {}

    Owning_Ptr<Declaration_Statement> Declaration_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Declaration_Statement* Declaration_Statement::_clone() const {
        return new Declaration_Statement(declaration->clone(), source_info);
    }

    Expression_Statement::Expression_Statement(Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::expression_statement), expression(ANTON_MOV(expression)) {}

    Owning_Ptr<Expression_Statement> Expression_Statement::clone() const {
        return Owning_Ptr{_clone()};
    }

    Expression_Statement* Expression_Statement::_clone() const {
        return new Expression_Statement(expression->clone(), source_info);
    }
} // namespace vush
