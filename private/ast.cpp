#include <ast2.hpp>

#include <anton/intrinsics.hpp>
#include <memory.hpp>

namespace vush {
    using namespace anton::literals;

    Syntax_Token::Syntax_Token(Syntax_Node_Kind type, anton::String value, Source_Info const& source_info)
        : value(ANTON_MOV(value)), source_info(source_info), type(type) {}

    Syntax_Node::Syntax_Node(Syntax_Node_Kind type, Array<SNOT> array, Source_Info const& source_info)
        : children(ANTON_MOV(array)), source_info(source_info), type(type) {}

    namespace ast {
        bool is_integer(Type const& type) {
            Type_Builtin const& builtin = static_cast<Type_Builtin const&>(type);
            return type.node_kind == Node_Kind::type_builtin && (builtin.value == GLSL_Type::glsl_int || builtin.value == GLSL_Type::glsl_uint);
        }

        bool is_void(Type const& type) {
            return type.node_kind == Node_Kind::type_builtin && static_cast<Type_Builtin const&>(type).value == GLSL_Type::glsl_void;
        }

        bool is_vector(Type const& type) {
            if(type.node_kind != Node_Kind::type_builtin) {
                return false;
            }

            GLSL_Type const v = static_cast<Type_Builtin const&>(type).value;
            return v == GLSL_Type::glsl_vec2 || v == GLSL_Type::glsl_vec3 || v == GLSL_Type::glsl_vec4 || v == GLSL_Type::glsl_dvec2 ||
                   v == GLSL_Type::glsl_dvec3 || v == GLSL_Type::glsl_dvec4 || v == GLSL_Type::glsl_bvec2 || v == GLSL_Type::glsl_bvec3 ||
                   v == GLSL_Type::glsl_bvec4 || v == GLSL_Type::glsl_ivec2 || v == GLSL_Type::glsl_ivec3 || v == GLSL_Type::glsl_ivec4 ||
                   v == GLSL_Type::glsl_uvec2 || v == GLSL_Type::glsl_uvec3 || v == GLSL_Type::glsl_uvec4;
        }

        bool is_matrix(Type const& type) {
            if(type.node_kind != Node_Kind::type_builtin) {
                return false;
            }

            GLSL_Type const v = static_cast<Type_Builtin const&>(type).value;
            return v == GLSL_Type::glsl_mat2 || v == GLSL_Type::glsl_mat3 || v == GLSL_Type::glsl_mat4 || v == GLSL_Type::glsl_mat2x3 ||
                   v == GLSL_Type::glsl_mat2x4 || v == GLSL_Type::glsl_mat3x2 || v == GLSL_Type::glsl_mat3x4 || v == GLSL_Type::glsl_mat4x2 ||
                   v == GLSL_Type::glsl_mat4x3 || v == GLSL_Type::glsl_dmat2 || v == GLSL_Type::glsl_dmat3 || v == GLSL_Type::glsl_dmat4 ||
                   v == GLSL_Type::glsl_dmat2x3 || v == GLSL_Type::glsl_dmat2x4 || v == GLSL_Type::glsl_dmat3x2 || v == GLSL_Type::glsl_dmat3x4 ||
                   v == GLSL_Type::glsl_dmat4x2 || v == GLSL_Type::glsl_dmat4x3;
        }

        bool is_opaque_type(Type const& type) {
            ANTON_ASSERT(type.node_kind == Node_Kind::type_builtin || type.node_kind == Node_Kind::type_user_defined || type.node_kind == Node_Kind::type_array,
                         u8"unknown ast node type");
            if(type.node_kind == Node_Kind::type_builtin) {
                Type_Builtin const& t = static_cast<Type_Builtin const&>(type);
                return is_opaque_glsl_type(t.value);
            } else if(type.node_kind == Node_Kind::type_user_defined) {
                return false;
            } else {
                Type_Array const& t = static_cast<Type_Array const&>(type);
                return is_opaque_type(*t.base);
            }
        }

        bool is_array(Type const& type) {
            return type.node_kind == Node_Kind::type_array;
        }

        bool is_sized_array(Type const& type) {
            return type.node_kind == Node_Kind::type_array && static_cast<Type_Array const&>(type).size;
        }

        bool is_unsized_array(Type const& type) {
            return type.node_kind == Node_Kind::type_array && !static_cast<Type_Array const&>(type).size;
        }

        bool is_image_type(Type const& type) {
            ANTON_ASSERT(type.node_kind == Node_Kind::type_builtin || type.node_kind == Node_Kind::type_user_defined || type.node_kind == Node_Kind::type_array,
                         u8"unknown ast node type");

            if(type.node_kind == Node_Kind::type_user_defined) {
                return false;
            }

            if(type.node_kind == Node_Kind::type_builtin) {
                Type_Builtin const& t = (Type_Builtin const&)type;
                return is_image_glsl_type(t.value);
            }

            Type_Array const& t = (Type_Array const&)type;
            return is_image_type(*t.base);
        }

        // anton::String stringify_type(Allocator* const allocator, Type const& type) {
        //     ANTON_ASSERT(type.node_kind == Node_Kind::type_builtin || type.node_kind == Node_Kind::type_user_defined || type.node_kind == Node_Kind::type_array,
        //                  u8"unknown ast node type");
        //     if(type.node_kind == Node_Kind::type_builtin) {
        //         Type_Builtin const& t = static_cast<Type_Builtin const&>(type);
        //         anton::String_View sv = stringify_glsl_type(t.value);
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

        bool is_opaque_glsl_type(GLSL_Type const type) {
            return static_cast<i32>(type) >= static_cast<i32>(GLSL_Type::glsl_sampler1D);
        }

        bool is_image_glsl_type(GLSL_Type const type) {
            return type == GLSL_Type::glsl_image1D || type == GLSL_Type::glsl_image1DArray || type == GLSL_Type::glsl_image2D ||
                   type == GLSL_Type::glsl_image2DArray || type == GLSL_Type::glsl_image2DMS || type == GLSL_Type::glsl_image2DMSArray ||
                   type == GLSL_Type::glsl_image2DRect || type == GLSL_Type::glsl_image3D || type == GLSL_Type::glsl_imageCube ||
                   type == GLSL_Type::glsl_imageCubeArray || type == GLSL_Type::glsl_imageBuffer || type == GLSL_Type::glsl_iimage1D ||
                   type == GLSL_Type::glsl_iimage1DArray || type == GLSL_Type::glsl_iimage2D || type == GLSL_Type::glsl_iimage2DArray ||
                   type == GLSL_Type::glsl_iimage2DMS || type == GLSL_Type::glsl_iimage2DMSArray || type == GLSL_Type::glsl_iimage2DRect ||
                   type == GLSL_Type::glsl_iimage3D || type == GLSL_Type::glsl_iimageCube || type == GLSL_Type::glsl_iimageCubeArray ||
                   type == GLSL_Type::glsl_iimageBuffer || type == GLSL_Type::glsl_uimage1D || type == GLSL_Type::glsl_uimage1DArray ||
                   type == GLSL_Type::glsl_uimage2D || type == GLSL_Type::glsl_uimage2DArray || type == GLSL_Type::glsl_uimage2DMS ||
                   type == GLSL_Type::glsl_uimage2DMSArray || type == GLSL_Type::glsl_uimage2DRect || type == GLSL_Type::glsl_uimage3D ||
                   type == GLSL_Type::glsl_uimageCube || type == GLSL_Type::glsl_uimageCubeArray || type == GLSL_Type::glsl_uimageBuffer;
        }

        bool is_type(Node const& node) {
            return node.node_kind == Node_Kind::type_builtin || node.node_kind == Node_Kind::type_user_defined || node.node_kind == Node_Kind::type_array;
        }

        bool is_sourced_parameter(Func_Parameter const& parameter) {
            return parameter.source;
        }

        bool is_vertex_input_parameter(Func_Parameter const& parameter) {
            return parameter.source && parameter.source->value == u8"in";
        }

        anton::Optional<GLSL_Type> enumify_glsl_type(anton::String_View const type) {
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

            static constexpr GLSL_Type builtin_types[] = {
                GLSL_Type::glsl_void,
                GLSL_Type::glsl_bool,
                GLSL_Type::glsl_int,
                GLSL_Type::glsl_uint,
                GLSL_Type::glsl_float,
                GLSL_Type::glsl_double,
                GLSL_Type::glsl_vec2,
                GLSL_Type::glsl_vec3,
                GLSL_Type::glsl_vec4,
                GLSL_Type::glsl_dvec2,
                GLSL_Type::glsl_dvec3,
                GLSL_Type::glsl_dvec4,
                GLSL_Type::glsl_bvec2,
                GLSL_Type::glsl_bvec3,
                GLSL_Type::glsl_bvec4,
                GLSL_Type::glsl_ivec2,
                GLSL_Type::glsl_ivec3,
                GLSL_Type::glsl_ivec4,
                GLSL_Type::glsl_uvec2,
                GLSL_Type::glsl_uvec3,
                GLSL_Type::glsl_uvec4,
                GLSL_Type::glsl_mat2,
                GLSL_Type::glsl_mat2,
                GLSL_Type::glsl_mat3,
                GLSL_Type::glsl_mat3,
                GLSL_Type::glsl_mat4,
                GLSL_Type::glsl_mat4,
                GLSL_Type::glsl_mat2x3,
                GLSL_Type::glsl_mat2x4,
                GLSL_Type::glsl_mat3x2,
                GLSL_Type::glsl_mat3x4,
                GLSL_Type::glsl_mat4x2,
                GLSL_Type::glsl_mat4x3,
                GLSL_Type::glsl_dmat2,
                GLSL_Type::glsl_dmat2,
                GLSL_Type::glsl_dmat3,
                GLSL_Type::glsl_dmat3,
                GLSL_Type::glsl_dmat4,
                GLSL_Type::glsl_dmat4,
                GLSL_Type::glsl_dmat2x3,
                GLSL_Type::glsl_dmat2x4,
                GLSL_Type::glsl_dmat3x2,
                GLSL_Type::glsl_dmat3x4,
                GLSL_Type::glsl_dmat4x2,
                GLSL_Type::glsl_dmat4x3,
                GLSL_Type::glsl_sampler1D,
                GLSL_Type::glsl_texture1D,
                GLSL_Type::glsl_image1D,
                GLSL_Type::glsl_sampler1DShadow,
                GLSL_Type::glsl_sampler1DArray,
                GLSL_Type::glsl_texture1DArray,
                GLSL_Type::glsl_image1DArray,
                GLSL_Type::glsl_sampler1DArrayShadow,
                GLSL_Type::glsl_sampler2D,
                GLSL_Type::glsl_texture2D,
                GLSL_Type::glsl_image2D,
                GLSL_Type::glsl_sampler2DShadow,
                GLSL_Type::glsl_sampler2DArray,
                GLSL_Type::glsl_texture2DArray,
                GLSL_Type::glsl_image2DArray,
                GLSL_Type::glsl_sampler2DArrayShadow,
                GLSL_Type::glsl_sampler2DMS,
                GLSL_Type::glsl_texture2DMS,
                GLSL_Type::glsl_image2DMS,
                GLSL_Type::glsl_sampler2DMSArray,
                GLSL_Type::glsl_texture2DMSArray,
                GLSL_Type::glsl_image2DMSArray,
                GLSL_Type::glsl_sampler2DRect,
                GLSL_Type::glsl_texture2DRect,
                GLSL_Type::glsl_image2DRect,
                GLSL_Type::glsl_sampler2DRectShadow,
                GLSL_Type::glsl_sampler3D,
                GLSL_Type::glsl_texture3D,
                GLSL_Type::glsl_image3D,
                GLSL_Type::glsl_samplerCube,
                GLSL_Type::glsl_textureCube,
                GLSL_Type::glsl_imageCube,
                GLSL_Type::glsl_samplerCubeShadow,
                GLSL_Type::glsl_samplerCubeArray,
                GLSL_Type::glsl_textureCubeArray,
                GLSL_Type::glsl_imageCubeArray,
                GLSL_Type::glsl_samplerCubeArrayShadow,
                GLSL_Type::glsl_samplerBuffer,
                GLSL_Type::glsl_textureBuffer,
                GLSL_Type::glsl_imageBuffer,
                GLSL_Type::glsl_subpassInput,
                GLSL_Type::glsl_subpassInputMS,
                GLSL_Type::glsl_isampler1D,
                GLSL_Type::glsl_itexture1D,
                GLSL_Type::glsl_iimage1D,
                GLSL_Type::glsl_isampler1DArray,
                GLSL_Type::glsl_itexture1DArray,
                GLSL_Type::glsl_iimage1DArray,
                GLSL_Type::glsl_isampler2D,
                GLSL_Type::glsl_itexture2D,
                GLSL_Type::glsl_iimage2D,
                GLSL_Type::glsl_isampler2DArray,
                GLSL_Type::glsl_itexture2DArray,
                GLSL_Type::glsl_iimage2DArray,
                GLSL_Type::glsl_isampler2DMS,
                GLSL_Type::glsl_itexture2DMS,
                GLSL_Type::glsl_iimage2DMS,
                GLSL_Type::glsl_isampler2DMSArray,
                GLSL_Type::glsl_itexture2DMSArray,
                GLSL_Type::glsl_iimage2DMSArray,
                GLSL_Type::glsl_isampler2DRect,
                GLSL_Type::glsl_itexture2DRect,
                GLSL_Type::glsl_iimage2DRect,
                GLSL_Type::glsl_isampler3D,
                GLSL_Type::glsl_itexture3D,
                GLSL_Type::glsl_iimage3D,
                GLSL_Type::glsl_isamplerCube,
                GLSL_Type::glsl_itextureCube,
                GLSL_Type::glsl_iimageCube,
                GLSL_Type::glsl_isamplerCubeArray,
                GLSL_Type::glsl_itextureCubeArray,
                GLSL_Type::glsl_iimageCubeArray,
                GLSL_Type::glsl_isamplerBuffer,
                GLSL_Type::glsl_itextureBuffer,
                GLSL_Type::glsl_iimageBuffer,
                GLSL_Type::glsl_isubpassInput,
                GLSL_Type::glsl_isubpassInputMS,
                GLSL_Type::glsl_usampler1D,
                GLSL_Type::glsl_utexture1D,
                GLSL_Type::glsl_uimage1D,
                GLSL_Type::glsl_usampler1DArray,
                GLSL_Type::glsl_utexture1DArray,
                GLSL_Type::glsl_uimage1DArray,
                GLSL_Type::glsl_usampler2D,
                GLSL_Type::glsl_utexture2D,
                GLSL_Type::glsl_uimage2D,
                GLSL_Type::glsl_usampler2DArray,
                GLSL_Type::glsl_utexture2DArray,
                GLSL_Type::glsl_uimage2DArray,
                GLSL_Type::glsl_usampler2DMS,
                GLSL_Type::glsl_utexture2DMS,
                GLSL_Type::glsl_uimage2DMS,
                GLSL_Type::glsl_usampler2DMSArray,
                GLSL_Type::glsl_utexture2DMSArray,
                GLSL_Type::glsl_uimage2DMSArray,
                GLSL_Type::glsl_usampler2DRect,
                GLSL_Type::glsl_utexture2DRect,
                GLSL_Type::glsl_uimage2DRect,
                GLSL_Type::glsl_usampler3D,
                GLSL_Type::glsl_utexture3D,
                GLSL_Type::glsl_uimage3D,
                GLSL_Type::glsl_usamplerCube,
                GLSL_Type::glsl_utextureCube,
                GLSL_Type::glsl_uimageCube,
                GLSL_Type::glsl_usamplerCubeArray,
                GLSL_Type::glsl_utextureCubeArray,
                GLSL_Type::glsl_uimageCubeArray,
                GLSL_Type::glsl_usamplerBuffer,
                GLSL_Type::glsl_utextureBuffer,
                GLSL_Type::glsl_uimageBuffer,
                GLSL_Type::glsl_usubpassInput,
                GLSL_Type::glsl_usubpassInputMS,
                GLSL_Type::glsl_sampler,
                GLSL_Type::glsl_samplerShadow,
            };

            constexpr i64 array_size = sizeof(builtin_types_strings) / sizeof(anton::String_View);
            for(i64 i = 0; i < array_size; ++i) {
                if(type == builtin_types_strings[i]) {
                    return builtin_types[i];
                }
            }

            return anton::null_optional;
        }

        bool compare_types_equal(Type const& lhs, Type const& rhs) {
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
