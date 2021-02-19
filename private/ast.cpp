#include <ast.hpp>

namespace vush {
    bool is_opaque_type(Builtin_GLSL_Type const type) {
        return static_cast<i32>(type) >= static_cast<i32>(Builtin_GLSL_Type::glsl_sampler1D);
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
            return t.name;
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

    bool is_unsized_array(Type const& type) {
        return type.node_type == AST_Node_Type::array_type && !static_cast<Array_Type const&>(type).size;
    }

    bool is_sized_array(Type const& type) {
        return type.node_type == AST_Node_Type::array_type && static_cast<Array_Type const&>(type).size;
    }
} // namespace vush
