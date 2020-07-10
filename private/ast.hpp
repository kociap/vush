#pragma once

#include <owning_ptr.hpp>
#include <vush/vush.hpp>

namespace vush {
    enum struct AST_Node_Type {
        identifier,
        builtin_type,
        user_defined_type,
        declaration_list,
        declaration_if,
        import_decl,
        source_definition_property,
        source_definition_decl,
        source_definition_emit_statement,
        source_definition_for_statement,
        sourced_global_decl,
        variable_declaration,
        constant_declaration,
        struct_decl,
        function_param_list,
        function_param_if,
        ordinary_function_param,
        function_declaration,
        sourced_function_param,
        vertex_input_param,
        pass_stage_declaration,
        expression_if,
        identifier_expression,
        assignment_expression,
        arithmetic_assignment_expression,
        elvis_expr,
        logic_or_expr,
        logic_xor_expr,
        logic_and_expr,
        relational_equality_expression,
        relational_expression,
        bit_or_expr,
        bit_xor_expr,
        bit_and_expr,
        lshift_expr,
        rshift_expr,
        add_expr,
        sub_expr,
        mul_expr,
        div_expr,
        mod_expr,
        unary_expression,
        prefix_inc_expr,
        prefix_dec_expr,
        argument_list,
        function_call_expression,
        member_access_expression,
        array_access_expression,
        postfix_inc_expr,
        postfix_dec_expr,
        paren_expr,
        string_literal,
        bool_literal,
        integer_literal,
        float_literal,
        statement_list,
        block_statement,
        if_statement,
        case_statement,
        default_case_statement,
        switch_statement,
        for_statement,
        while_statement,
        do_while_statement,
        return_statement,
        break_statement,
        continue_statement,
        declaration_statement,
        expression_statement,
    };

    struct Source_Info {
        anton::String_View file_path;
        i64 line;
        i64 column;
        i64 file_offset;
    };

    struct AST_Node {
        Source_Info source_info;
        AST_Node_Type node_type;

        AST_Node(Source_Info const& source_info, AST_Node_Type node_type): source_info(source_info), node_type(node_type) {}
        virtual ~AST_Node() = default;
    };

    struct Declaration;
    struct Expression;
    struct Statement;
    struct String_Literal;

    struct Identifier: public AST_Node {
        anton::String value;

        Identifier(anton::String value, Source_Info const& source_info): AST_Node(source_info, AST_Node_Type::identifier), value(anton::move(value)) {}
    };

    struct Type: public AST_Node {
        using AST_Node::AST_Node;
    };

    enum struct Builtin_GLSL_Type : i32 {
        glsl_void,
        glsl_bool,
        glsl_int,
        glsl_uint,
        glsl_float,
        glsl_double,
        glsl_vec2,
        glsl_vec3,
        glsl_vec4,
        glsl_dvec2,
        glsl_dvec3,
        glsl_dvec4,
        glsl_bvec2,
        glsl_bvec3,
        glsl_bvec4,
        glsl_ivec2,
        glsl_ivec3,
        glsl_ivec4,
        glsl_uvec2,
        glsl_uvec3,
        glsl_uvec4,
        glsl_mat2,
        glsl_mat3,
        glsl_mat4,
        glsl_mat2x3,
        glsl_mat2x4,
        glsl_mat3x2,
        glsl_mat3x4,
        glsl_mat4x2,
        glsl_mat4x3,
        glsl_dmat2,
        glsl_dmat3,
        glsl_dmat4,
        glsl_dmat2x3,
        glsl_dmat2x4,
        glsl_dmat3x2,
        glsl_dmat3x4,
        glsl_dmat4x2,
        glsl_dmat4x3,
        glsl_sampler1D,
        glsl_texture1D,
        glsl_image1D,
        glsl_sampler1DShadow,
        glsl_sampler1DArray,
        glsl_texture1DArray,
        glsl_image1DArray,
        glsl_sampler1DArrayShadow,
        glsl_sampler2D,
        glsl_texture2D,
        glsl_image2D,
        glsl_sampler2DShadow,
        glsl_sampler2DArray,
        glsl_texture2DArray,
        glsl_image2DArray,
        glsl_sampler2DArrayShadow,
        glsl_sampler2DMS,
        glsl_texture2DMS,
        glsl_image2DMS,
        glsl_sampler2DMSArray,
        glsl_texture2DMSArray,
        glsl_image2DMSArray,
        glsl_sampler2DRect,
        glsl_texture2DRect,
        glsl_image2DRect,
        glsl_sampler2DRectShadow,
        glsl_sampler3D,
        glsl_texture3D,
        glsl_image3D,
        glsl_samplerCube,
        glsl_textureCube,
        glsl_imageCube,
        glsl_samplerCubeShadow,
        glsl_samplerCubeArray,
        glsl_textureCubeArray,
        glsl_imageCubeArray,
        glsl_samplerCubeArrayShadow,
        glsl_samplerBuffer,
        glsl_textureBuffer,
        glsl_imageBuffer,
        glsl_subpassInput,
        glsl_subpassInputMS,
        glsl_isampler1D,
        glsl_itexture1D,
        glsl_iimage1D,
        glsl_isampler1DArray,
        glsl_itexture1DArray,
        glsl_iimage1DArray,
        glsl_isampler2D,
        glsl_itexture2D,
        glsl_iimage2D,
        glsl_isampler2DArray,
        glsl_itexture2DArray,
        glsl_iimage2DArray,
        glsl_isampler2DMS,
        glsl_itexture2DMS,
        glsl_iimage2DMS,
        glsl_isampler2DMSArray,
        glsl_itexture2DMSArray,
        glsl_iimage2DMSArray,
        glsl_isampler2DRect,
        glsl_itexture2DRect,
        glsl_iimage2DRect,
        glsl_isampler3D,
        glsl_itexture3D,
        glsl_iimage3D,
        glsl_isamplerCube,
        glsl_itextureCube,
        glsl_iimageCube,
        glsl_isamplerCubeArray,
        glsl_itextureCubeArray,
        glsl_iimageCubeArray,
        glsl_isamplerBuffer,
        glsl_itextureBuffer,
        glsl_iimageBuffer,
        glsl_isubpassInput,
        glsl_isubpassInputMS,
        glsl_usampler1D,
        glsl_utexture1D,
        glsl_uimage1D,
        glsl_usampler1DArray,
        glsl_utexture1DArray,
        glsl_uimage1DArray,
        glsl_usampler2D,
        glsl_utexture2D,
        glsl_uimage2D,
        glsl_usampler2DArray,
        glsl_utexture2DArray,
        glsl_uimage2DArray,
        glsl_usampler2DMS,
        glsl_utexture2DMS,
        glsl_uimage2DMS,
        glsl_usampler2DMSArray,
        glsl_utexture2DMSArray,
        glsl_uimage2DMSArray,
        glsl_usampler2DRect,
        glsl_utexture2DRect,
        glsl_uimage2DRect,
        glsl_usampler3D,
        glsl_utexture3D,
        glsl_uimage3D,
        glsl_usamplerCube,
        glsl_utextureCube,
        glsl_uimageCube,
        glsl_usamplerCubeArray,
        glsl_utextureCubeArray,
        glsl_uimageCubeArray,
        glsl_usamplerBuffer,
        glsl_utextureBuffer,
        glsl_uimageBuffer,
        glsl_usubpassInput,
        glsl_usubpassInputMS,
        glsl_sampler,
        glsl_samplerShadow,
    };

    constexpr bool is_opaque_type(Builtin_GLSL_Type type) {
        return static_cast<i32>(type) >= static_cast<i32>(Builtin_GLSL_Type::glsl_sampler1D);
    }

    constexpr anton::String_View stringify(Builtin_GLSL_Type type) {
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

    struct Builtin_Type: public Type {
        Builtin_GLSL_Type type;

        Builtin_Type(Builtin_GLSL_Type type, Source_Info const& source_info): Type(source_info, AST_Node_Type::builtin_type), type(type) {}
    };

    struct User_Defined_Type: public Type {
        anton::String name;

        User_Defined_Type(anton::String name, Source_Info const& source_info): Type(source_info, AST_Node_Type::user_defined_type), name(anton::move(name)) {}
    };

    inline anton::String_View stringify_type(Type const& type) {
        ANTON_ASSERT(type.node_type == AST_Node_Type::builtin_type || type.node_type == AST_Node_Type::user_defined_type, u8"unknown ast node type");
        if(type.node_type == AST_Node_Type::builtin_type) {
            Builtin_Type const& t = static_cast<Builtin_Type const&>(type);
            return stringify(t.type);
        } else {
            User_Defined_Type const& t = static_cast<User_Defined_Type const&>(type);
            return t.name;
        }
    }

    struct Declaration: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Declaration_List: public AST_Node {
        anton::Array<Owning_Ptr<Declaration>> declarations;

        Declaration_List(): AST_Node({}, AST_Node_Type::declaration_list) {}

        void append(Declaration* const declaration) {
            declarations.emplace_back(declaration);
        }

        [[nodiscard]] i64 size() const {
            return declarations.size();
        }
    };

    struct Declaration_If: public Declaration {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Declaration_List> true_declarations;
        Owning_Ptr<Declaration_List> false_declarations;

        Declaration_If(Expression* condition, Declaration_List* true_declarations, Declaration_List* false_declarations, Source_Info const& source_info)
            : Declaration(source_info, AST_Node_Type::declaration_if), condition(condition), true_declarations(true_declarations),
              false_declarations(false_declarations) {}
    };

    struct Import_Decl: public Declaration {
        anton::String path;

        Import_Decl(anton::String path, Source_Info const& source_info): Declaration(source_info, AST_Node_Type::import_decl), path(anton::move(path)) {}
    };

    struct Source_Definition_Statement;

    struct Source_Definition_Property: public AST_Node {
        anton::Array<Owning_Ptr<Source_Definition_Statement>> statements;

        Source_Definition_Property(Source_Info const& source_info): AST_Node(source_info, AST_Node_Type::source_definition_property) {}

        void append(Source_Definition_Statement* statement) {
            statements.emplace_back(statement);
        }
    };

    struct Source_Definition_Decl: public Declaration {
        Owning_Ptr<Identifier> name;
        Owning_Ptr<Source_Definition_Property> decl_prop;
        Owning_Ptr<Source_Definition_Property> bind_prop;

        Source_Definition_Decl(Identifier* name, Source_Info const& source_info): Declaration(source_info, AST_Node_Type::source_definition_decl), name(name) {}
    };

    struct Source_Definition_Statement: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Source_Definition_Emit_Statement: public Source_Definition_Statement {
        Owning_Ptr<String_Literal> string;

        Source_Definition_Emit_Statement(String_Literal* string, Source_Info const& source_info)
            : Source_Definition_Statement(source_info, AST_Node_Type::source_definition_emit_statement), string(string) {}
    };

    struct Source_Definition_For_Statement: public Source_Definition_Statement {
        Owning_Ptr<Identifier> iterator;
        Owning_Ptr<Identifier> range_expr;
        anton::Array<Owning_Ptr<Source_Definition_Statement>> statements;

        Source_Definition_For_Statement(Identifier* iterator, Identifier* range_expr, Source_Info const& source_info)
            : Source_Definition_Statement(source_info, AST_Node_Type::source_definition_for_statement), iterator(iterator), range_expr(range_expr) {}

        void append(Source_Definition_Statement* statement) {
            statements.emplace_back(statement);
        }
    };

    struct Sourced_Global_Decl: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> name;
        Owning_Ptr<Identifier> source;

        Sourced_Global_Decl(Type* type, Identifier* name, Identifier* source, Source_Info const& source_info)
            : Declaration(source_info, AST_Node_Type::sourced_global_decl), type(type), name(name), source(source) {}
    };

    struct Variable_Declaration: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Expression> initializer;

        Variable_Declaration(Type* type, Identifier* identifier, Expression* initializer, Source_Info const& source_info)
            : Declaration(source_info, AST_Node_Type::variable_declaration), type(type), identifier(identifier), initializer(initializer) {}
    };

    struct Constant_Declaration: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Expression> initializer;

        Constant_Declaration(Type* type, Identifier* identifier, Expression* initializer, Source_Info const& source_info)
            : Declaration(source_info, AST_Node_Type::constant_declaration), type(type), identifier(identifier), initializer(initializer) {}
    };

    struct Struct_Decl: public Declaration {
        Owning_Ptr<Identifier> name;
        anton::Array<Owning_Ptr<Variable_Declaration>> members;

        Struct_Decl(Identifier* name, Source_Info const& source_info): Declaration(source_info, AST_Node_Type::struct_decl), name(name) {}

        void append(Variable_Declaration* decl) {
            members.emplace_back(decl);
        }

        i64 size() const {
            return members.size();
        }
    };

    struct Statement_List;

    struct Function_Param: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Function_Param_List: public AST_Node {
        anton::Array<Owning_Ptr<Function_Param>> params;

        Function_Param_List(): AST_Node({}, AST_Node_Type::function_param_list) {}

        void append_parameter(Function_Param* parameter) {
            params.push_back(parameter);
        }

        i64 get_parameter_count() const {
            return params.size();
        }
    };

    struct Function_Param_If: public Function_Param {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Function_Param> true_param;
        Owning_Ptr<Function_Param> false_param;

        Function_Param_If(Expression* condition, Function_Param* true_param, Function_Param* false_param, Source_Info const& source_info)
            : Function_Param(source_info, AST_Node_Type::function_param_if), condition(condition), true_param(true_param), false_param(false_param) {}
    };

    struct Ordinary_Function_Param: public Function_Param {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> type;

        Ordinary_Function_Param(Identifier* identifier, Type* type, Source_Info const& source_info)
            : Function_Param(source_info, AST_Node_Type::ordinary_function_param), identifier(identifier), type(type) {}
    };

    struct Function_Declaration: public Declaration {
        Owning_Ptr<Identifier> name;
        Owning_Ptr<Function_Param_List> param_list;
        Owning_Ptr<Type> return_type;
        Owning_Ptr<Statement_List> body;

        Function_Declaration(Identifier* name, Function_Param_List* function_param_list, Type* return_type, Statement_List* body,
                             Source_Info const& source_info)
            : Declaration(source_info, AST_Node_Type::function_declaration), name(name), param_list(function_param_list), return_type(return_type), body(body) {
        }
    };

    struct Sourced_Function_Param: public Function_Param {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> source;

        Sourced_Function_Param(Identifier* identifier, Type* type, Identifier* source, Source_Info const& source_info)
            : Function_Param(source_info, AST_Node_Type::sourced_function_param), identifier(identifier), type(type), source(source) {}
    };

    struct Vertex_Input_Param: public Function_Param {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> type;

        Vertex_Input_Param(Identifier* identifier, Type* type, Source_Info const& source_info)
            : Function_Param(source_info, AST_Node_Type::vertex_input_param), identifier(identifier), type(type) {}
    };

    enum struct Pass_Stage_Type {
        vertex,
        fragment,
        compute,
    };

    constexpr anton::String_View stringify(Pass_Stage_Type type) {
        switch(type) {
            case Pass_Stage_Type::vertex:
                return u8"vertex";
            case Pass_Stage_Type::fragment:
                return u8"fragment";
            case Pass_Stage_Type::compute:
                return u8"compute";
        }
    }

    struct Pass_Stage_Declaration: public Declaration {
        Owning_Ptr<Identifier> pass;
        Pass_Stage_Type stage;
        Owning_Ptr<Function_Param_List> param_list;
        Owning_Ptr<Type> return_type;
        Owning_Ptr<Statement_List> body;

        Pass_Stage_Declaration(Identifier* pass, Pass_Stage_Type stage, Function_Param_List* parameter_list, Type* return_type, Statement_List* body,
                               Source_Info const& source_info)
            : Declaration(source_info, AST_Node_Type::pass_stage_declaration), pass(pass), stage(stage), param_list(parameter_list), return_type(return_type),
              body(body) {}
    };

    struct Expression: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Expression_If: public Expression {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> true_expr;
        Owning_Ptr<Expression> false_expr;

        Expression_If(Expression* condition, Expression* true_expr, Expression* false_expr, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::expression_if), condition(condition), true_expr(true_expr), false_expr(false_expr) {}
    };

    struct Identifier_Expression: public Expression {
        Owning_Ptr<Identifier> identifier;

        Identifier_Expression(Identifier* identifier, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::identifier_expression), identifier(identifier) {}
    };

    struct Assignment_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Assignment_Expression(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::assignment_expression), lhs(lhs), rhs(rhs) {}
    };

    enum struct Arithmetic_Assignment_Type {
        plus,
        minus,
        multiply,
        divide,
        remainder,
        lshift,
        rshift,
        bit_and,
        bit_or,
        bit_xor,
    };

    struct Arithmetic_Assignment_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        Arithmetic_Assignment_Type type;

        Arithmetic_Assignment_Expression(Arithmetic_Assignment_Type type, Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::arithmetic_assignment_expression), lhs(lhs), rhs(rhs), type(type) {}
    };

    struct Elvis_Expr: public Expression {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> true_expr;
        Owning_Ptr<Expression> false_expr;

        Elvis_Expr(Expression* condition, Expression* true_expr, Expression* false_expr, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::elvis_expr), condition(condition), true_expr(true_expr), false_expr(false_expr) {}
    };

    struct Logic_Or_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_Or_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::logic_or_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Logic_Xor_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_Xor_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::logic_xor_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Logic_And_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_And_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::logic_and_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Relational_Equality_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        bool is_equality;

        Relational_Equality_Expression(bool is_equality, Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::relational_equality_expression), lhs(lhs), rhs(rhs), is_equality(is_equality) {}
    };

    enum struct Relational_Type {
        greater_than,
        less_than,
        greater_equal,
        less_equal,
    };

    struct Relational_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        Relational_Type type;

        Relational_Expression(Relational_Type type, Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::relational_expression), lhs(lhs), rhs(rhs), type(type) {}
    };

    struct Bit_Or_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_Or_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::bit_or_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Bit_Xor_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_Xor_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::bit_xor_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Bit_And_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_And_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::bit_and_expr), lhs(lhs), rhs(rhs) {}
    };

    struct LShift_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        LShift_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::lshift_expr), lhs(lhs), rhs(rhs) {}
    };

    struct RShift_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        RShift_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::rshift_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Add_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Add_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info): Expression(source_info, AST_Node_Type::add_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Sub_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Sub_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info): Expression(source_info, AST_Node_Type::sub_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Mul_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Mul_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info): Expression(source_info, AST_Node_Type::mul_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Div_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Div_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info): Expression(source_info, AST_Node_Type::div_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Mod_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Mod_Expr(Expression* lhs, Expression* rhs, Source_Info const& source_info): Expression(source_info, AST_Node_Type::mod_expr), lhs(lhs), rhs(rhs) {}
    };

    enum struct Unary_Type {
        plus,
        minus,
        bit_not,
        logic_not,
    };

    struct Unary_Expression: public Expression {
        Owning_Ptr<Expression> expression;
        Unary_Type type;

        Unary_Expression(Unary_Type type, Expression* expression, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::unary_expression), expression(expression), type(type) {}
    };

    struct Prefix_Inc_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Prefix_Inc_Expr(Expression* expression, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::prefix_inc_expr), expression(expression) {}
    };

    struct Prefix_Dec_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Prefix_Dec_Expr(Expression* expression, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::prefix_dec_expr), expression(expression) {}
    };

    struct Argument_List: public AST_Node {
        anton::Array<Owning_Ptr<Expression>> arguments;

        Argument_List(): AST_Node({}, AST_Node_Type::argument_list) {}

        void append(Expression* argument) {
            arguments.emplace_back(argument);
        }

        i64 size() const {
            return arguments.size();
        }
    };

    struct Function_Call_Expression: public Expression {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Argument_List> arg_list;

        Function_Call_Expression(Identifier* identifier, Argument_List* arg_list, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::function_call_expression), identifier(identifier), arg_list(arg_list) {}
    };

    struct Member_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Identifier> member;

        Member_Access_Expression(Expression* base, Identifier* member, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::member_access_expression), base(base), member(member) {}
    };

    struct Array_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Expression> index;

        Array_Access_Expression(Expression* base, Expression* index, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::array_access_expression), base(base), index(index) {}
    };

    struct Postfix_Inc_Expr: public Expression {
        Owning_Ptr<Expression> base;

        Postfix_Inc_Expr(Expression* base, Source_Info const& source_info): Expression(source_info, AST_Node_Type::postfix_inc_expr), base(base) {}
    };

    struct Postfix_Dec_Expr: public Expression {
        Owning_Ptr<Expression> base;

        Postfix_Dec_Expr(Expression* base, Source_Info const& source_info): Expression(source_info, AST_Node_Type::postfix_dec_expr), base(base) {}
    };

    struct Paren_Expr: public Expression {
        Owning_Ptr<Expression> expr;

        Paren_Expr(Expression* expr, Source_Info const& source_info): Expression(source_info, AST_Node_Type::paren_expr), expr(expr) {}
    };

    struct String_Literal: public Expression {
        anton::String value;

        String_Literal(anton::String value, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::string_literal), value(anton::move(value)) {}
    };

    struct Bool_Literal: public Expression {
        bool value;

        Bool_Literal(bool value, Source_Info const& source_info): Expression(source_info, AST_Node_Type::bool_literal), value(value) {}
    };

    struct Integer_Literal: public Expression {
        anton::String value;

        Integer_Literal(anton::String value, Source_Info const& source_info)
            : Expression(source_info, AST_Node_Type::integer_literal), value(anton::move(value)) {}
    };

    struct Float_Literal: public Expression {
        anton::String value;

        Float_Literal(anton::String value, Source_Info const& source_info): Expression(source_info, AST_Node_Type::float_literal), value(anton::move(value)) {}
    };

    struct Statement: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Statement_List: public AST_Node {
        anton::Array<Owning_Ptr<Statement>> statements;

        Statement_List(): AST_Node({}, AST_Node_Type::statement_list) {}

        void append(Statement* const statement) {
            statements.emplace_back(statement);
        }

        [[nodiscard]] i64 size() const {
            return statements.size();
        }
    };

    struct Block_Statement: public Statement {
        Owning_Ptr<Statement_List> statements;

        Block_Statement(Statement_List* statements, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::block_statement), statements(statements) {}
    };

    struct If_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Statement> true_statement;
        Owning_Ptr<Statement> false_statement;

        If_Statement(Expression* condition, Statement* true_statement, Statement* false_statement, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::if_statement), condition(condition), true_statement(true_statement), false_statement(false_statement) {}
    };

    struct Case_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Statement_List> statements;

        Case_Statement(Expression* condition, Statement_List* statements, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::case_statement), condition(condition), statements(statements) {}
    };

    struct Default_Case_Statement: public Statement {
        Owning_Ptr<Statement_List> statements;

        Default_Case_Statement(Statement_List* statements, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::default_case_statement), statements(statements) {}
    };

    struct Switch_Statement: public Statement {
        anton::Array<Owning_Ptr<Statement>> cases;
        Owning_Ptr<Expression> match_expr;

        Switch_Statement(Expression* match_expr, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::switch_statement), match_expr(match_expr) {}

        void append(Case_Statement* case_stmt) {
            cases.emplace_back(case_stmt);
        }

        void append(Default_Case_Statement* case_stmt) {
            cases.emplace_back(case_stmt);
        }

        i64 case_count() const {
            return cases.size();
        }
    };

    struct For_Statement: public Statement {
        Owning_Ptr<Variable_Declaration> declaration;
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> post_expression;
        Owning_Ptr<Block_Statement> block;

        For_Statement(Variable_Declaration* declaration, Expression* condition, Expression* post_expression, Block_Statement* block,
                      Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::for_statement), declaration(declaration), condition(condition), post_expression(post_expression),
              block(block) {}
    };

    struct While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Block_Statement> block;

        While_Statement(Expression* condition, Block_Statement* block, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::while_statement), condition(condition), block(block) {}
    };

    struct Do_While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Block_Statement> block;

        Do_While_Statement(Expression* condition, Block_Statement* block, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::do_while_statement), condition(condition), block(block) {}
    };

    struct Return_Statement: public Statement {
        Owning_Ptr<Expression> return_expr;

        Return_Statement(Expression* return_expr, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::return_statement), return_expr(return_expr) {}
    };

    struct Break_Statement: public Statement {
        Break_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::break_statement) {}
    };

    struct Continue_Statement: public Statement {
        Continue_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::continue_statement) {}
    };

    struct Declaration_Statement: public Statement {
        Owning_Ptr<Declaration> declaration;

        Declaration_Statement(Declaration* declaration, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::declaration_statement), declaration(declaration) {}
    };

    struct Expression_Statement: public Statement {
        Owning_Ptr<Expression> expr;

        Expression_Statement(Expression* expression, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::expression_statement), expr(expression) {}
    };
} // namespace vush
