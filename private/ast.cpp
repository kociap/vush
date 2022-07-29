#include <ast.hpp>

#include <anton/intrinsics.hpp>
#include <memory.hpp>

namespace vush {
    using namespace anton::literals;

    Syntax_Token::Syntax_Token(Syntax_Node_Type type, anton::String value, Source_Info const& source_info)
        : value(ANTON_MOV(value)), source_info(source_info), type(type) {}

    Syntax_Node::Syntax_Node(Syntax_Node_Type type, Array<SNOT> array, Source_Info const& source_info)
        : children(ANTON_MOV(array)), source_info(source_info), type(type) {}

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
                return u8"void"_sv;
            case Builtin_GLSL_Type::glsl_bool:
                return u8"bool"_sv;
            case Builtin_GLSL_Type::glsl_int:
                return u8"int"_sv;
            case Builtin_GLSL_Type::glsl_uint:
                return u8"uint"_sv;
            case Builtin_GLSL_Type::glsl_float:
                return u8"float"_sv;
            case Builtin_GLSL_Type::glsl_double:
                return u8"double"_sv;
            case Builtin_GLSL_Type::glsl_vec2:
                return u8"vec2"_sv;
            case Builtin_GLSL_Type::glsl_vec3:
                return u8"vec3"_sv;
            case Builtin_GLSL_Type::glsl_vec4:
                return u8"vec4"_sv;
            case Builtin_GLSL_Type::glsl_dvec2:
                return u8"dvec2"_sv;
            case Builtin_GLSL_Type::glsl_dvec3:
                return u8"dvec3"_sv;
            case Builtin_GLSL_Type::glsl_dvec4:
                return u8"dvec4"_sv;
            case Builtin_GLSL_Type::glsl_bvec2:
                return u8"bvec2"_sv;
            case Builtin_GLSL_Type::glsl_bvec3:
                return u8"bvec3"_sv;
            case Builtin_GLSL_Type::glsl_bvec4:
                return u8"bvec4"_sv;
            case Builtin_GLSL_Type::glsl_ivec2:
                return u8"ivec2"_sv;
            case Builtin_GLSL_Type::glsl_ivec3:
                return u8"ivec3"_sv;
            case Builtin_GLSL_Type::glsl_ivec4:
                return u8"ivec4"_sv;
            case Builtin_GLSL_Type::glsl_uvec2:
                return u8"uvec2"_sv;
            case Builtin_GLSL_Type::glsl_uvec3:
                return u8"uvec3"_sv;
            case Builtin_GLSL_Type::glsl_uvec4:
                return u8"uvec4"_sv;
            case Builtin_GLSL_Type::glsl_mat2:
                return u8"mat2"_sv;
            case Builtin_GLSL_Type::glsl_mat3:
                return u8"mat3"_sv;
            case Builtin_GLSL_Type::glsl_mat4:
                return u8"mat4"_sv;
            case Builtin_GLSL_Type::glsl_mat2x3:
                return u8"mat2x3"_sv;
            case Builtin_GLSL_Type::glsl_mat2x4:
                return u8"mat2x4"_sv;
            case Builtin_GLSL_Type::glsl_mat3x2:
                return u8"mat3x2"_sv;
            case Builtin_GLSL_Type::glsl_mat3x4:
                return u8"mat3x4"_sv;
            case Builtin_GLSL_Type::glsl_mat4x2:
                return u8"mat4x2"_sv;
            case Builtin_GLSL_Type::glsl_mat4x3:
                return u8"mat4x3"_sv;
            case Builtin_GLSL_Type::glsl_dmat2:
                return u8"dmat2"_sv;
            case Builtin_GLSL_Type::glsl_dmat3:
                return u8"dmat3"_sv;
            case Builtin_GLSL_Type::glsl_dmat4:
                return u8"dmat4"_sv;
            case Builtin_GLSL_Type::glsl_dmat2x3:
                return u8"dmat2x3"_sv;
            case Builtin_GLSL_Type::glsl_dmat2x4:
                return u8"dmat2x4"_sv;
            case Builtin_GLSL_Type::glsl_dmat3x2:
                return u8"dmat3x2"_sv;
            case Builtin_GLSL_Type::glsl_dmat3x4:
                return u8"dmat3x4"_sv;
            case Builtin_GLSL_Type::glsl_dmat4x2:
                return u8"dmat4x2"_sv;
            case Builtin_GLSL_Type::glsl_dmat4x3:
                return u8"dmat4x3"_sv;
            case Builtin_GLSL_Type::glsl_sampler1D:
                return u8"sampler1D"_sv;
            case Builtin_GLSL_Type::glsl_texture1D:
                return u8"texture1D"_sv;
            case Builtin_GLSL_Type::glsl_image1D:
                return u8"image1D"_sv;
            case Builtin_GLSL_Type::glsl_sampler1DShadow:
                return u8"sampler1DShadow"_sv;
            case Builtin_GLSL_Type::glsl_sampler1DArray:
                return u8"sampler1DArray"_sv;
            case Builtin_GLSL_Type::glsl_texture1DArray:
                return u8"texture1DArray"_sv;
            case Builtin_GLSL_Type::glsl_image1DArray:
                return u8"image1DArray"_sv;
            case Builtin_GLSL_Type::glsl_sampler1DArrayShadow:
                return u8"sampler1DArrayShadow"_sv;
            case Builtin_GLSL_Type::glsl_sampler2D:
                return u8"sampler2D"_sv;
            case Builtin_GLSL_Type::glsl_texture2D:
                return u8"texture2D"_sv;
            case Builtin_GLSL_Type::glsl_image2D:
                return u8"image2D"_sv;
            case Builtin_GLSL_Type::glsl_sampler2DShadow:
                return u8"sampler2DShadow"_sv;
            case Builtin_GLSL_Type::glsl_sampler2DArray:
                return u8"sampler2DArray"_sv;
            case Builtin_GLSL_Type::glsl_texture2DArray:
                return u8"texture2DArray"_sv;
            case Builtin_GLSL_Type::glsl_image2DArray:
                return u8"image2DArray"_sv;
            case Builtin_GLSL_Type::glsl_sampler2DArrayShadow:
                return u8"sampler2DArrayShadow"_sv;
            case Builtin_GLSL_Type::glsl_sampler2DMS:
                return u8"sampler2DMS"_sv;
            case Builtin_GLSL_Type::glsl_texture2DMS:
                return u8"texture2DMS"_sv;
            case Builtin_GLSL_Type::glsl_image2DMS:
                return u8"image2DMS"_sv;
            case Builtin_GLSL_Type::glsl_sampler2DMSArray:
                return u8"sampler2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_texture2DMSArray:
                return u8"texture2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_image2DMSArray:
                return u8"image2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_sampler2DRect:
                return u8"sampler2DRect"_sv;
            case Builtin_GLSL_Type::glsl_texture2DRect:
                return u8"texture2DRect"_sv;
            case Builtin_GLSL_Type::glsl_image2DRect:
                return u8"image2DRect"_sv;
            case Builtin_GLSL_Type::glsl_sampler2DRectShadow:
                return u8"sampler2DRectShadow"_sv;
            case Builtin_GLSL_Type::glsl_sampler3D:
                return u8"sampler3D"_sv;
            case Builtin_GLSL_Type::glsl_texture3D:
                return u8"texture3D"_sv;
            case Builtin_GLSL_Type::glsl_image3D:
                return u8"image3D"_sv;
            case Builtin_GLSL_Type::glsl_samplerCube:
                return u8"samplerCube"_sv;
            case Builtin_GLSL_Type::glsl_textureCube:
                return u8"textureCube"_sv;
            case Builtin_GLSL_Type::glsl_imageCube:
                return u8"imageCube"_sv;
            case Builtin_GLSL_Type::glsl_samplerCubeShadow:
                return u8"samplerCubeShadow"_sv;
            case Builtin_GLSL_Type::glsl_samplerCubeArray:
                return u8"samplerCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_textureCubeArray:
                return u8"textureCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_imageCubeArray:
                return u8"imageCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_samplerCubeArrayShadow:
                return u8"samplerCubeArrayShadow"_sv;
            case Builtin_GLSL_Type::glsl_samplerBuffer:
                return u8"samplerBuffer"_sv;
            case Builtin_GLSL_Type::glsl_textureBuffer:
                return u8"textureBuffer"_sv;
            case Builtin_GLSL_Type::glsl_imageBuffer:
                return u8"imageBuffer"_sv;
            case Builtin_GLSL_Type::glsl_subpassInput:
                return u8"subpassInput"_sv;
            case Builtin_GLSL_Type::glsl_subpassInputMS:
                return u8"subpassInputMS"_sv;
            case Builtin_GLSL_Type::glsl_isampler1D:
                return u8"isampler1D"_sv;
            case Builtin_GLSL_Type::glsl_itexture1D:
                return u8"itexture1D"_sv;
            case Builtin_GLSL_Type::glsl_iimage1D:
                return u8"iimage1D"_sv;
            case Builtin_GLSL_Type::glsl_isampler1DArray:
                return u8"isampler1DArray"_sv;
            case Builtin_GLSL_Type::glsl_itexture1DArray:
                return u8"itexture1DArray"_sv;
            case Builtin_GLSL_Type::glsl_iimage1DArray:
                return u8"iimage1DArray"_sv;
            case Builtin_GLSL_Type::glsl_isampler2D:
                return u8"isampler2D"_sv;
            case Builtin_GLSL_Type::glsl_itexture2D:
                return u8"itexture2D"_sv;
            case Builtin_GLSL_Type::glsl_iimage2D:
                return u8"iimage2D"_sv;
            case Builtin_GLSL_Type::glsl_isampler2DArray:
                return u8"isampler2DArray"_sv;
            case Builtin_GLSL_Type::glsl_itexture2DArray:
                return u8"itexture2DArray"_sv;
            case Builtin_GLSL_Type::glsl_iimage2DArray:
                return u8"iimage2DArray"_sv;
            case Builtin_GLSL_Type::glsl_isampler2DMS:
                return u8"isampler2DMS"_sv;
            case Builtin_GLSL_Type::glsl_itexture2DMS:
                return u8"itexture2DMS"_sv;
            case Builtin_GLSL_Type::glsl_iimage2DMS:
                return u8"iimage2DMS"_sv;
            case Builtin_GLSL_Type::glsl_isampler2DMSArray:
                return u8"isampler2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_itexture2DMSArray:
                return u8"itexture2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_iimage2DMSArray:
                return u8"iimage2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_isampler2DRect:
                return u8"isampler2DRect"_sv;
            case Builtin_GLSL_Type::glsl_itexture2DRect:
                return u8"itexture2DRect"_sv;
            case Builtin_GLSL_Type::glsl_iimage2DRect:
                return u8"iimage2DRect"_sv;
            case Builtin_GLSL_Type::glsl_isampler3D:
                return u8"isampler3D"_sv;
            case Builtin_GLSL_Type::glsl_itexture3D:
                return u8"itexture3D"_sv;
            case Builtin_GLSL_Type::glsl_iimage3D:
                return u8"iimage3D"_sv;
            case Builtin_GLSL_Type::glsl_isamplerCube:
                return u8"isamplerCube"_sv;
            case Builtin_GLSL_Type::glsl_itextureCube:
                return u8"itextureCube"_sv;
            case Builtin_GLSL_Type::glsl_iimageCube:
                return u8"iimageCube"_sv;
            case Builtin_GLSL_Type::glsl_isamplerCubeArray:
                return u8"isamplerCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_itextureCubeArray:
                return u8"itextureCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_iimageCubeArray:
                return u8"iimageCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_isamplerBuffer:
                return u8"isamplerBuffer"_sv;
            case Builtin_GLSL_Type::glsl_itextureBuffer:
                return u8"itextureBuffer"_sv;
            case Builtin_GLSL_Type::glsl_iimageBuffer:
                return u8"iimageBuffer"_sv;
            case Builtin_GLSL_Type::glsl_isubpassInput:
                return u8"isubpassInput"_sv;
            case Builtin_GLSL_Type::glsl_isubpassInputMS:
                return u8"isubpassInputMS"_sv;
            case Builtin_GLSL_Type::glsl_usampler1D:
                return u8"usampler1D"_sv;
            case Builtin_GLSL_Type::glsl_utexture1D:
                return u8"utexture1D"_sv;
            case Builtin_GLSL_Type::glsl_uimage1D:
                return u8"uimage1D"_sv;
            case Builtin_GLSL_Type::glsl_usampler1DArray:
                return u8"usampler1DArray"_sv;
            case Builtin_GLSL_Type::glsl_utexture1DArray:
                return u8"utexture1DArray"_sv;
            case Builtin_GLSL_Type::glsl_uimage1DArray:
                return u8"uimage1DArray"_sv;
            case Builtin_GLSL_Type::glsl_usampler2D:
                return u8"usampler2D"_sv;
            case Builtin_GLSL_Type::glsl_utexture2D:
                return u8"utexture2D"_sv;
            case Builtin_GLSL_Type::glsl_uimage2D:
                return u8"uimage2D"_sv;
            case Builtin_GLSL_Type::glsl_usampler2DArray:
                return u8"usampler2DArray"_sv;
            case Builtin_GLSL_Type::glsl_utexture2DArray:
                return u8"utexture2DArray"_sv;
            case Builtin_GLSL_Type::glsl_uimage2DArray:
                return u8"uimage2DArray"_sv;
            case Builtin_GLSL_Type::glsl_usampler2DMS:
                return u8"usampler2DMS"_sv;
            case Builtin_GLSL_Type::glsl_utexture2DMS:
                return u8"utexture2DMS"_sv;
            case Builtin_GLSL_Type::glsl_uimage2DMS:
                return u8"uimage2DMS"_sv;
            case Builtin_GLSL_Type::glsl_usampler2DMSArray:
                return u8"usampler2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_utexture2DMSArray:
                return u8"utexture2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_uimage2DMSArray:
                return u8"uimage2DMSArray"_sv;
            case Builtin_GLSL_Type::glsl_usampler2DRect:
                return u8"usampler2DRect"_sv;
            case Builtin_GLSL_Type::glsl_utexture2DRect:
                return u8"utexture2DRect"_sv;
            case Builtin_GLSL_Type::glsl_uimage2DRect:
                return u8"uimage2DRect"_sv;
            case Builtin_GLSL_Type::glsl_usampler3D:
                return u8"usampler3D"_sv;
            case Builtin_GLSL_Type::glsl_utexture3D:
                return u8"utexture3D"_sv;
            case Builtin_GLSL_Type::glsl_uimage3D:
                return u8"uimage3D"_sv;
            case Builtin_GLSL_Type::glsl_usamplerCube:
                return u8"usamplerCube"_sv;
            case Builtin_GLSL_Type::glsl_utextureCube:
                return u8"utextureCube"_sv;
            case Builtin_GLSL_Type::glsl_uimageCube:
                return u8"uimageCube"_sv;
            case Builtin_GLSL_Type::glsl_usamplerCubeArray:
                return u8"usamplerCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_utextureCubeArray:
                return u8"utextureCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_uimageCubeArray:
                return u8"uimageCubeArray"_sv;
            case Builtin_GLSL_Type::glsl_usamplerBuffer:
                return u8"usamplerBuffer"_sv;
            case Builtin_GLSL_Type::glsl_utextureBuffer:
                return u8"utextureBuffer"_sv;
            case Builtin_GLSL_Type::glsl_uimageBuffer:
                return u8"uimageBuffer"_sv;
            case Builtin_GLSL_Type::glsl_usubpassInput:
                return u8"usubpassInput"_sv;
            case Builtin_GLSL_Type::glsl_usubpassInputMS:
                return u8"usubpassInputMS"_sv;
            case Builtin_GLSL_Type::glsl_sampler:
                return u8"sampler"_sv;
            case Builtin_GLSL_Type::glsl_samplerShadow:
                return u8"samplerShadow"_sv;
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

    anton::String stringify_type(Allocator* const allocator, Type const& type) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type ||
                         type.node_type == AST_Node_Type::array_type,
                     u8"unknown ast node type");
        if(type.node_type == AST_Node_Type::builtin_type) {
            Builtin_Type const& t = static_cast<Builtin_Type const&>(type);
            anton::String_View sv = stringify(t.type);
            return anton::String(sv, allocator);
        } else if(type.node_type == AST_Node_Type::user_defined_type) {
            User_Defined_Type const& t = static_cast<User_Defined_Type const&>(type);
            return anton::String(t.identifier, allocator);
        } else {
            Array_Type const& t = static_cast<Array_Type const&>(type);
            anton::String str = stringify_type(allocator, *t.base);
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
                return u8"rgba32f"_sv;
            case Image_Layout_Type::rgba16f:
                return u8"rgba16f"_sv;
            case Image_Layout_Type::rg32f:
                return u8"rg32f"_sv;
            case Image_Layout_Type::rg16f:
                return u8"rg16f"_sv;
            case Image_Layout_Type::r11f_g11f_b10f:
                return u8"r11f_g11f_b10f"_sv;
            case Image_Layout_Type::r32f:
                return u8"r32f"_sv;
            case Image_Layout_Type::r16f:
                return u8"r16f"_sv;
            case Image_Layout_Type::rgba16:
                return u8"rgba16"_sv;
            case Image_Layout_Type::rgb10_a2:
                return u8"rgb10_a2"_sv;
            case Image_Layout_Type::rgba8:
                return u8"rgba8"_sv;
            case Image_Layout_Type::rg16:
                return u8"rg16"_sv;
            case Image_Layout_Type::rg8:
                return u8"rg8"_sv;
            case Image_Layout_Type::r16:
                return u8"r16"_sv;
            case Image_Layout_Type::r8:
                return u8"r8"_sv;
            case Image_Layout_Type::rgba16_snorm:
                return u8"rgba16_snorm"_sv;
            case Image_Layout_Type::rgba8_snorm:
                return u8"rgba8_snorm"_sv;
            case Image_Layout_Type::rg16_snorm:
                return u8"rg16_snorm"_sv;
            case Image_Layout_Type::rg8_snorm:
                return u8"rg8_snorm"_sv;
            case Image_Layout_Type::r16_snorm:
                return u8"r16_snorm"_sv;
            case Image_Layout_Type::r8_snorm:
                return u8"r8_snorm"_sv;
            case Image_Layout_Type::rgba32i:
                return u8"rgba32i"_sv;
            case Image_Layout_Type::rgba16i:
                return u8"rgba16i"_sv;
            case Image_Layout_Type::rgba8i:
                return u8"rgba8i"_sv;
            case Image_Layout_Type::rg32i:
                return u8"rg32i"_sv;
            case Image_Layout_Type::rg16i:
                return u8"rg16i"_sv;
            case Image_Layout_Type::rg8i:
                return u8"rg8i"_sv;
            case Image_Layout_Type::r32i:
                return u8"r32i"_sv;
            case Image_Layout_Type::r16i:
                return u8"r16i"_sv;
            case Image_Layout_Type::r8i:
                return u8"r8i"_sv;
            case Image_Layout_Type::rgba32ui:
                return u8"rgba32ui"_sv;
            case Image_Layout_Type::rgba16ui:
                return u8"rgba16ui"_sv;
            case Image_Layout_Type::rgb10_a2ui:
                return u8"rgb10_a2ui"_sv;
            case Image_Layout_Type::rgba8ui:
                return u8"rgba8ui"_sv;
            case Image_Layout_Type::rg32ui:
                return u8"rg32ui"_sv;
            case Image_Layout_Type::rg16ui:
                return u8"rg16ui"_sv;
            case Image_Layout_Type::rg8ui:
                return u8"rg8ui"_sv;
            case Image_Layout_Type::r32ui:
                return u8"r32ui"_sv;
            case Image_Layout_Type::r16ui:
                return u8"r16ui"_sv;
            case Image_Layout_Type::r8ui:
                return u8"r8ui"_sv;
        }
    }

    bool is_sourced_parameter(Function_Parameter const& parameter) {
        return parameter.source;
    }

    bool is_vertex_input_parameter(Function_Parameter const& parameter) {
        return parameter.source && parameter.source->value == u8"in";
    }

#define ALLOC(type, ...) allocate<type>(allocator, __VA_ARGS__)

    template<typename T>
    Array<Owning_Ptr<T>> clone(Array<Owning_Ptr<T>> const& array, Allocator* const allocator) {
        Array<Owning_Ptr<T>> copy{anton::reserve, array.size(), array.get_allocator()};
        for(Owning_Ptr<T> const& object: array) {
            copy.emplace_back(object->clone(allocator));
        }
        return copy;
    }

    Owning_Ptr<AST_Node> AST_Node::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Owning_Ptr<Identifier> Identifier::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Identifier* Identifier::_clone(Allocator* const allocator) const {
        return ALLOC(Identifier, value, source_info);
    }

    Owning_Ptr<Type> Type::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Owning_Ptr<Builtin_Type> Builtin_Type::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Builtin_Type* Builtin_Type::_clone(Allocator* const allocator) const {
        return ALLOC(Builtin_Type, type, source_info);
    }

    User_Defined_Type::User_Defined_Type(anton::String_View identifier, Source_Info const& source_info)
        : Type(source_info, AST_Node_Type::user_defined_type), identifier(ANTON_MOV(identifier)) {}

    Owning_Ptr<User_Defined_Type> User_Defined_Type::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    User_Defined_Type* User_Defined_Type::_clone(Allocator* const allocator) const {
        return ALLOC(User_Defined_Type, identifier, source_info);
    }

    Array_Type::Array_Type(Owning_Ptr<Type> base, Owning_Ptr<Integer_Literal> size, Source_Info const& source_info)
        : Type(source_info, AST_Node_Type::array_type), base(ANTON_MOV(base)), size(ANTON_MOV(size)) {}

    Owning_Ptr<Array_Type> Array_Type::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Array_Type* Array_Type::_clone(Allocator* const allocator) const {
        if(size) {
            return ALLOC(Array_Type, base->clone(allocator), size->clone(allocator), source_info);
        } else {
            return ALLOC(Array_Type, base->clone(allocator), nullptr, source_info);
        }
    }

    Owning_Ptr<Declaration> Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Declaration_If::Declaration_If(Owning_Ptr<Expression> condition, Declaration_List true_declarations, Declaration_List false_declarations,
                                   Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::declaration_if), condition(ANTON_MOV(condition)), true_declarations(ANTON_MOV(true_declarations)),
          false_declarations(ANTON_MOV(false_declarations)) {}

    Owning_Ptr<Declaration_If> Declaration_If::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Declaration_If* Declaration_If::_clone(Allocator* const allocator) const {
        return ALLOC(Declaration_If, condition->clone(allocator), vush::clone(true_declarations, allocator), vush::clone(false_declarations, allocator),
                     source_info);
    }

    Import_Declaration::Import_Declaration(Owning_Ptr<String_Literal> path, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::import_declaration), path(ANTON_MOV(path)) {}

    Owning_Ptr<Import_Declaration> Import_Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Import_Declaration* Import_Declaration::_clone(Allocator* const allocator) const {
        return ALLOC(Import_Declaration, path->clone(allocator), source_info);
    }

    Variable_Declaration::Variable_Declaration(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer,
                                               Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::variable_declaration), type(ANTON_MOV(type)), identifier(ANTON_MOV(identifier)),
          initializer(ANTON_MOV(initializer)) {}

    Owning_Ptr<Variable_Declaration> Variable_Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Variable_Declaration* Variable_Declaration::_clone(Allocator* const allocator) const {
        if(initializer) {
            return ALLOC(Variable_Declaration, type->clone(allocator), identifier->clone(allocator), initializer->clone(allocator), source_info);
        } else {
            return ALLOC(Variable_Declaration, type->clone(allocator), identifier->clone(allocator), nullptr, source_info);
        }
    }

    Constant_Declaration::Constant_Declaration(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer,
                                               Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::constant_declaration), type(ANTON_MOV(type)), identifier(ANTON_MOV(identifier)),
          initializer(ANTON_MOV(initializer)) {}

    Owning_Ptr<Constant_Declaration> Constant_Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Constant_Declaration* Constant_Declaration::_clone(Allocator* const allocator) const {
        if(initializer) {
            return ALLOC(Constant_Declaration, type->clone(allocator), identifier->clone(allocator), initializer->clone(allocator), source_info);
        } else {
            return ALLOC(Constant_Declaration, type->clone(allocator), identifier->clone(allocator), nullptr, source_info);
        }
    }

    Struct_Member::Struct_Member(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer, Interpolation interpolation,
                                 bool invariant, Source_Info const& source_info)
        : AST_Node(source_info, AST_Node_Type::struct_member), type(ANTON_MOV(type)), identifier(ANTON_MOV(identifier)), initializer(ANTON_MOV(initializer)),
          interpolation(interpolation), invariant(invariant) {}

    Owning_Ptr<Struct_Member> Struct_Member::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Struct_Member* Struct_Member::_clone(Allocator* const allocator) const {
        return ALLOC(Struct_Member, type->clone(allocator), identifier->clone(allocator), initializer->clone(allocator), interpolation, invariant, source_info);
    }

    Struct_Declaration::Struct_Declaration(Owning_Ptr<Identifier> identifier, Array<Owning_Ptr<Struct_Member>> members, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::struct_declaration), members(ANTON_MOV(members)), identifier(ANTON_MOV(identifier)) {}

    Owning_Ptr<Struct_Declaration> Struct_Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Struct_Declaration* Struct_Declaration::_clone(Allocator* const allocator) const {
        return ALLOC(Struct_Declaration, identifier->clone(allocator), vush::clone(members, allocator), source_info);
    }

    Settings_Declaration::Settings_Declaration(Owning_Ptr<Identifier> pass_name, Array<Setting_Key_Value> settings, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::settings_declaration), pass_name(ANTON_MOV(pass_name)), settings(ANTON_MOV(settings)) {}

    Owning_Ptr<Settings_Declaration> Settings_Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Settings_Declaration* Settings_Declaration::_clone(Allocator* const allocator) const {
        return ALLOC(Settings_Declaration, pass_name->clone(allocator), settings, source_info);
    }

    Owning_Ptr<Attribute> Attribute::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Workgroup_Attribute::Workgroup_Attribute(Owning_Ptr<Integer_Literal> x, Owning_Ptr<Integer_Literal> y, Owning_Ptr<Integer_Literal> z,
                                             Source_Info const& source_info)
        : Attribute(source_info, AST_Node_Type::workgroup_attribute), x(ANTON_MOV(x)), y(ANTON_MOV(y)), z(ANTON_MOV(z)) {}

    Owning_Ptr<Workgroup_Attribute> Workgroup_Attribute::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Workgroup_Attribute* Workgroup_Attribute::_clone(Allocator* const allocator) const {
        return ALLOC(Workgroup_Attribute, x->clone(allocator), y->clone(allocator), z->clone(allocator), source_info);
    }

    Owning_Ptr<Function_Parameter_Node> Function_Parameter_Node::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Function_Param_If::Function_Param_If(Owning_Ptr<Expression> condition, Owning_Ptr<Function_Parameter_Node> true_param,
                                         Owning_Ptr<Function_Parameter_Node> false_param, Source_Info const& source_info)
        : Function_Parameter_Node(source_info, AST_Node_Type::function_param_if), condition(ANTON_MOV(condition)), true_param(ANTON_MOV(true_param)),
          false_param(ANTON_MOV(false_param)) {}

    Owning_Ptr<Function_Param_If> Function_Param_If::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Function_Param_If* Function_Param_If::_clone(Allocator* const allocator) const {
        if(false_param) {
            return ALLOC(Function_Param_If, condition->clone(allocator), true_param->clone(allocator), false_param->clone(allocator), source_info);
        } else {
            return ALLOC(Function_Param_If, condition->clone(allocator), true_param->clone(allocator), nullptr, source_info);
        }
    }

    Owning_Ptr<Layout_Qualifier> Layout_Qualifier::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Image_Layout_Qualifier::Image_Layout_Qualifier(Image_Layout_Type type, Source_Info const& source_info)
        : Layout_Qualifier(source_info, AST_Node_Type::image_layout_qualifier), type(type) {}

    Owning_Ptr<Image_Layout_Qualifier> Image_Layout_Qualifier::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Image_Layout_Qualifier* Image_Layout_Qualifier::_clone(Allocator* const allocator) const {
        return ALLOC(Image_Layout_Qualifier, type, source_info);
    }

    Function_Parameter::Function_Parameter(Owning_Ptr<Identifier> identifier, Owning_Ptr<Type> type, Owning_Ptr<Identifier> source,
                                           Owning_Ptr<Image_Layout_Qualifier> image_layout, Source_Info const& source_info)
        : Function_Parameter_Node(source_info, AST_Node_Type::function_parameter), type(ANTON_MOV(type)), identifier(ANTON_MOV(identifier)),
          source(ANTON_MOV(source)), image_layout(ANTON_MOV(image_layout)) {}

    Owning_Ptr<Function_Parameter> Function_Parameter::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Function_Parameter* Function_Parameter::_clone(Allocator* const allocator) const {
        Owning_Ptr<Image_Layout_Qualifier> _image_layout = image_layout ? image_layout->clone(allocator) : nullptr;
        Owning_Ptr<Identifier> _source = source ? source->clone(allocator) : nullptr;
        return ALLOC(Function_Parameter, identifier->clone(allocator), type->clone(allocator), ANTON_MOV(_source), ANTON_MOV(_image_layout), source_info);
    }

    Function_Declaration::Function_Declaration(Attribute_List attributes, Owning_Ptr<Type> return_type, Owning_Ptr<Identifier> identifier,
                                               Parameter_List parameters, Statement_List body, bool builtin, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::function_declaration), attributes(ANTON_MOV(attributes)), parameters(ANTON_MOV(parameters)),
          body(ANTON_MOV(body)), identifier(ANTON_MOV(identifier)), return_type(ANTON_MOV(return_type)), builtin(builtin) {}

    Owning_Ptr<Function_Declaration> Function_Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Function_Declaration* Function_Declaration::_clone(Allocator* const allocator) const {
        return ALLOC(Function_Declaration, vush::clone(attributes, allocator), return_type->clone(allocator), identifier->clone(allocator),
                     vush::clone(parameters, allocator), vush::clone(body, allocator), builtin, source_info);
    }

    Overloaded_Function_Declaration::Overloaded_Function_Declaration(Owning_Ptr<Identifier> identifier, bool builtin, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::overloaded_function_declaration), identifier(ANTON_MOV(identifier)), builtin(builtin) {}

    Overloaded_Function_Declaration::Overloaded_Function_Declaration(Owning_Ptr<Identifier> identifier, Array<Owning_Ptr<Function_Declaration>> overloads,
                                                                     bool builtin, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::overloaded_function_declaration), identifier(ANTON_MOV(identifier)), overloads(ANTON_MOV(overloads)),
          builtin(builtin) {}

    Owning_Ptr<Overloaded_Function_Declaration> Overloaded_Function_Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Overloaded_Function_Declaration* Overloaded_Function_Declaration::_clone(Allocator* const allocator) const {
        return ALLOC(Overloaded_Function_Declaration, identifier->clone(allocator), vush::clone(overloads, allocator), builtin, source_info);
    }

    Pass_Stage_Declaration::Pass_Stage_Declaration(Attribute_List attributes, Owning_Ptr<Type> return_type, Owning_Ptr<Identifier> pass_name,
                                                   Stage_Type stage_type, Parameter_List parameters, Statement_List body, Source_Info const& source_info)
        : Declaration(source_info, AST_Node_Type::pass_stage_declaration), attributes(ANTON_MOV(attributes)), parameters(ANTON_MOV(parameters)),
          body(ANTON_MOV(body)), pass_name(ANTON_MOV(pass_name)), return_type(ANTON_MOV(return_type)), stage_type(ANTON_MOV(stage_type)) {}

    Owning_Ptr<Pass_Stage_Declaration> Pass_Stage_Declaration::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Pass_Stage_Declaration* Pass_Stage_Declaration::_clone(Allocator* const allocator) const {
        return ALLOC(Pass_Stage_Declaration, vush::clone(attributes, allocator), return_type->clone(allocator), pass_name->clone(allocator), stage_type,
                     vush::clone(parameters, allocator), vush::clone(body, allocator), source_info);
    }

    Owning_Ptr<Expression> Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Expression_If::Expression_If(Owning_Ptr<Expression> condition, Owning_Ptr<Expression> true_expression, Owning_Ptr<Expression> false_expression,
                                 Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::expression_if), condition(ANTON_MOV(condition)), true_expression(ANTON_MOV(true_expression)),
          false_expression(ANTON_MOV(false_expression)) {}

    Owning_Ptr<Expression_If> Expression_If::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Expression_If* Expression_If::_clone(Allocator* const allocator) const {
        return ALLOC(Expression_If, condition->clone(allocator), true_expression->clone(allocator), false_expression->clone(allocator), source_info);
    }

    Identifier_Expression::Identifier_Expression(anton::String_View value, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::identifier_expression), value(ANTON_MOV(value)) {}

    Owning_Ptr<Identifier_Expression> Identifier_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Identifier_Expression* Identifier_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Identifier_Expression, value, source_info);
    }

    Binary_Expression::Binary_Expression(Binary_Expression_Type type, Owning_Ptr<Expression> lhs, Owning_Ptr<Expression> rhs, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::binary_expression), lhs(ANTON_MOV(lhs)), rhs(ANTON_MOV(rhs)), type(type) {}

    Owning_Ptr<Binary_Expression> Binary_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Binary_Expression* Binary_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Binary_Expression, type, lhs->clone(allocator), rhs->clone(allocator), source_info);
    }

    Prefix_Expression::Prefix_Expression(Prefix_Expression_Type const type, Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::prefix_expression), expression(ANTON_MOV(expression)), type(type) {}

    Owning_Ptr<Prefix_Expression> Prefix_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Prefix_Expression* Prefix_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Prefix_Expression, type, expression->clone(allocator), source_info);
    }

    Function_Call_Expression::Function_Call_Expression(Owning_Ptr<Identifier> identifier, Expression_List arguments, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::function_call_expression), arguments(ANTON_MOV(arguments)), identifier(ANTON_MOV(identifier)) {}

    Owning_Ptr<Function_Call_Expression> Function_Call_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Function_Call_Expression* Function_Call_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Function_Call_Expression, identifier->clone(allocator), vush::clone(arguments, allocator), source_info);
    }

    Member_Access_Expression::Member_Access_Expression(Owning_Ptr<Expression> base, Owning_Ptr<Identifier> member, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::member_access_expression), base(ANTON_MOV(base)), member(ANTON_MOV(member)) {}

    Owning_Ptr<Member_Access_Expression> Member_Access_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Member_Access_Expression* Member_Access_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Member_Access_Expression, base->clone(allocator), member->clone(allocator), source_info);
    }

    Array_Access_Expression::Array_Access_Expression(Owning_Ptr<Expression> base, Owning_Ptr<Expression> index, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::array_access_expression), base(ANTON_MOV(base)), index(ANTON_MOV(index)) {}

    Owning_Ptr<Array_Access_Expression> Array_Access_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Array_Access_Expression* Array_Access_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Array_Access_Expression, base->clone(allocator), index->clone(allocator), source_info);
    }

    Postfix_Expression::Postfix_Expression(Postfix_Expression_Type const type, Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::postfix_expression), expression(ANTON_MOV(expression)), type(type) {}

    Owning_Ptr<Postfix_Expression> Postfix_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Postfix_Expression* Postfix_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Postfix_Expression, type, expression->clone(allocator), source_info);
    }

    Parenthesised_Expression::Parenthesised_Expression(Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::parenthesised_expression), expression(ANTON_MOV(expression)) {}

    Owning_Ptr<Parenthesised_Expression> Parenthesised_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Parenthesised_Expression* Parenthesised_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Parenthesised_Expression, expression->clone(allocator), source_info);
    }

    Reinterpret_Expression::Reinterpret_Expression(Owning_Ptr<Type> target_type, Owning_Ptr<Expression> source, Owning_Ptr<Expression> index,
                                                   Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::reinterpret_expression), target_type(ANTON_MOV(target_type)), source(ANTON_MOV(source)),
          index(ANTON_MOV(index)) {}

    Owning_Ptr<Reinterpret_Expression> Reinterpret_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Reinterpret_Expression* Reinterpret_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Reinterpret_Expression, target_type->clone(allocator), source->clone(allocator), index->clone(allocator), source_info);
    }

    Default_Expression::Default_Expression(Source_Info const& source_info): Expression(source_info, AST_Node_Type::default_expression) {}

    Owning_Ptr<Default_Expression> Default_Expression::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Default_Expression* Default_Expression::_clone(Allocator* const allocator) const {
        return ALLOC(Default_Expression, source_info);
    }

    String_Literal::String_Literal(anton::String_View value, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::string_literal), value(ANTON_MOV(value)) {}

    Owning_Ptr<String_Literal> String_Literal::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    String_Literal* String_Literal::_clone(Allocator* const allocator) const {
        return ALLOC(String_Literal, value, source_info);
    }

    Bool_Literal::Bool_Literal(bool value, Source_Info const& source_info): Expression(source_info, AST_Node_Type::bool_literal), value(value) {}

    Owning_Ptr<Bool_Literal> Bool_Literal::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Bool_Literal* Bool_Literal::_clone(Allocator* const allocator) const {
        return ALLOC(Bool_Literal, value, source_info);
    }

    Integer_Literal::Integer_Literal(anton::String_View value, Integer_Literal_Type type, Integer_Literal_Base base, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::integer_literal), value(ANTON_MOV(value)), type(type), base(base) {}

    Owning_Ptr<Integer_Literal> Integer_Literal::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Integer_Literal* Integer_Literal::_clone(Allocator* const allocator) const {
        return ALLOC(Integer_Literal, value, type, base, source_info);
    }

    Float_Literal::Float_Literal(anton::String_View value, Float_Literal_Type type, Source_Info const& source_info)
        : Expression(source_info, AST_Node_Type::float_literal), value(ANTON_MOV(value)), type(type) {}

    Owning_Ptr<Float_Literal> Float_Literal::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Float_Literal* Float_Literal::_clone(Allocator* const allocator) const {
        return ALLOC(Float_Literal, value, type, source_info);
    }

    Owning_Ptr<Statement> Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Block_Statement::Block_Statement(Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::block_statement), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<Block_Statement> Block_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Block_Statement* Block_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Block_Statement, vush::clone(statements, allocator), source_info);
    }

    If_Statement::If_Statement(Owning_Ptr<Expression> condition, Statement_List true_statements, Statement_List false_statements,
                               Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::if_statement), condition(ANTON_MOV(condition)), true_statements(ANTON_MOV(true_statements)),
          false_statements(ANTON_MOV(false_statements)) {}

    Owning_Ptr<If_Statement> If_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    If_Statement* If_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(If_Statement, condition->clone(allocator), vush::clone(true_statements, allocator), vush::clone(false_statements, allocator), source_info);
    }

    Case_Statement::Case_Statement(Expression_List labels, Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::case_statement), labels(ANTON_MOV(labels)), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<Case_Statement> Case_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Case_Statement* Case_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Case_Statement, vush::clone(labels, allocator), vush::clone(statements, allocator), source_info);
    }

    Switch_Statement::Switch_Statement(Owning_Ptr<Expression> match_expression, Array<Owning_Ptr<Case_Statement>> cases, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::switch_statement), cases(ANTON_MOV(cases)), match_expression(ANTON_MOV(match_expression)) {}

    Owning_Ptr<Switch_Statement> Switch_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Switch_Statement* Switch_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Switch_Statement, match_expression->clone(allocator), vush::clone(cases, allocator), source_info);
    }

    For_Statement::For_Statement(Owning_Ptr<Variable_Declaration> declaration, Owning_Ptr<Expression> condition, Owning_Ptr<Expression> post_expression,
                                 Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::for_statement), declaration(ANTON_MOV(declaration)), condition(ANTON_MOV(condition)),
          post_expression(ANTON_MOV(post_expression)), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<For_Statement> For_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    For_Statement* For_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(For_Statement, declaration->clone(allocator), condition->clone(allocator), post_expression->clone(allocator),
                     vush::clone(statements, allocator), source_info);
    }

    While_Statement::While_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::while_statement), condition(ANTON_MOV(condition)), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<While_Statement> While_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    While_Statement* While_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(While_Statement, condition->clone(allocator), vush::clone(statements, allocator), source_info);
    }

    Do_While_Statement::Do_While_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::do_while_statement), condition(ANTON_MOV(condition)), statements(ANTON_MOV(statements)) {}

    Owning_Ptr<Do_While_Statement> Do_While_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Do_While_Statement* Do_While_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Do_While_Statement, condition->clone(allocator), vush::clone(statements, allocator), source_info);
    }

    Return_Statement::Return_Statement(Owning_Ptr<Expression> return_expression, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::return_statement), return_expression(ANTON_MOV(return_expression)) {}

    Owning_Ptr<Return_Statement> Return_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Return_Statement* Return_Statement::_clone(Allocator* const allocator) const {
        if(return_expression) {
            return ALLOC(Return_Statement, return_expression->clone(allocator), source_info);
        } else {
            return ALLOC(Return_Statement, nullptr, source_info);
        }
    }

    Break_Statement::Break_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::break_statement) {}

    Owning_Ptr<Break_Statement> Break_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Break_Statement* Break_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Break_Statement, source_info);
    }

    Continue_Statement::Continue_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::continue_statement) {}

    Owning_Ptr<Continue_Statement> Continue_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Continue_Statement* Continue_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Continue_Statement, source_info);
    }

    Discard_Statement::Discard_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::discard_statement) {}

    Owning_Ptr<Discard_Statement> Discard_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Discard_Statement* Discard_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Discard_Statement, source_info);
    }

    Declaration_Statement::Declaration_Statement(Owning_Ptr<Declaration> declaration, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::declaration_statement), declaration(ANTON_MOV(declaration)) {}

    Owning_Ptr<Declaration_Statement> Declaration_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Declaration_Statement* Declaration_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Declaration_Statement, declaration->clone(allocator), source_info);
    }

    Expression_Statement::Expression_Statement(Owning_Ptr<Expression> expression, Source_Info const& source_info)
        : Statement(source_info, AST_Node_Type::expression_statement), expression(ANTON_MOV(expression)) {}

    Owning_Ptr<Expression_Statement> Expression_Statement::clone(Allocator* const allocator) const {
        return Owning_Ptr{_clone(allocator), allocator};
    }

    Expression_Statement* Expression_Statement::_clone(Allocator* const allocator) const {
        return ALLOC(Expression_Statement, expression->clone(allocator), source_info);
    }
} // namespace vush
