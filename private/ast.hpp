#pragma once

#include <anton/optional.hpp>
#include <ast_fwd.hpp>
#include <either.hpp>
#include <owning_ptr.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Context;

    struct Source_Info {
        anton::String_View source_path;
        i64 line = 0;
        i64 column = 0;
        // The offset into the source at which the matched node starts.
        i64 offset = 0;
        i64 end_line = 0;
        i64 end_column = 0;
        // The offset into the source at which the matched node ends.
        i64 end_offset = 0;
    };

    // Syntax_Node_Type
    // Overlaps with lexer's Token_Type.
    //
    enum struct Syntax_Node_Type {
        identifier,
        comment,
        whitespace,
        // keywords
        kw_if,
        kw_else,
        kw_switch,
        kw_case,
        kw_default,
        kw_for,
        kw_while,
        kw_do,
        kw_return,
        kw_break,
        kw_continue,
        kw_discard,
        kw_from,
        kw_struct,
        kw_import,
        kw_const,
        kw_settings,
        kw_reinterpret,
        kw_invariant,
        kw_smooth,
        kw_flat,
        kw_noperspective,
        // separators
        tk_brace_open,
        tk_brace_close,
        tk_lbracket,
        tk_rbracket,
        tk_lparen,
        tk_rparen,
        tk_langle,
        tk_rangle,
        tk_semicolon,
        tk_colon,
        tk_comma,
        tk_dot,
        tk_double_quote,
        tk_plus,
        tk_minus,
        tk_asterisk,
        tk_slash,
        tk_percent,
        tk_amp,
        tk_pipe,
        tk_hat,
        tk_bang,
        tk_tilde,
        tk_equals,
        // literals
        lt_bin_integer,
        lt_oct_integer,
        lt_dec_integer,
        lt_hex_integer,
        lt_float,
        lt_string,
        lt_bool,

        // compound tokens
        tk_plus2, // ++
        tk_minus2, // --
        tk_amp2, // &&
        tk_pipe2, // ||
        tk_hat2, // ^^
        tk_shl, // <<
        tk_shr, // >>
        tk_eq2, // ==
        tk_neq, // !=
        tk_lteq, // <=
        tk_gteq, // >=
        tk_pluseq, // +=
        tk_minuseq, // -=
        tk_asteriskeq, // *=
        tk_slasheq, // /=
        tk_percenteq, // %=
        tk_ampeq, // &=
        tk_pipeeq, // |=
        tk_hateq, // ^=
        tk_shleq, // <<=
        tk_shreq, // >>=
        tk_thick_arrow, // =>
        tk_thin_arrow, // ->
        tk_colon2, // ::

        // tk_setting_string
        // A special type of string only used by setting key and values.
        // This token contains anything other than comments, whitespace,
        // colons, left braces or right braces.
        //
        tk_setting_string,

        type_builtin,
        type_user_defined,
        type_array,

        decl_block,
        decl_if,
        decl_import,
        decl_constant,
        decl_struct_member,
        decl_struct_member_block,
        decl_struct,
        decl_settings,
        decl_function,
        decl_pass_stage,

        func_parameter_if,
        func_parameter,
        func_parameter_list,

        attr_workgroup,
        // attribute_list
        // List of attributes.
        //
        attribute_list,

        setting_block,
        setting_keyval,

        expr_block,
        expr_if,
        expr_identifier,
        expr_binary,
        expr_prefix,
        expr_postfix,
        expr_member_access,
        expr_array_access,
        expr_parentheses,
        expr_reinterpret,
        expr_call,
        expr_literal,
        // expr_default
        // The 'default' label in stmt_case.
        //
        expr_default,

        call_arg_list,

        stmt_block,
        stmt_if,
        stmt_case,
        stmt_switch,
        stmt_for,
        stmt_while,
        stmt_do_while,
        stmt_return,
        stmt_break,
        stmt_continue,
        stmt_discard,
        stmt_variable,
        stmt_expression,
        stmt_empty,

        switch_case_list,
        switch_case,
    };

    struct Syntax_Token;
    struct Syntax_Node;

    // Syntax Node Or Token
    using SNOT = Either<Syntax_Node, Syntax_Token>;

    struct Syntax_Token {
        anton::String value;
        Source_Info source_info;
        Syntax_Node_Type type;

        Syntax_Token(Syntax_Node_Type type, anton::String value, Source_Info const& source_info);
    };

    // Syntax_Node
    // Untyped syntax node containing syntax information.
    //
    struct Syntax_Node {
        Array<SNOT> children;
        Source_Info source_info;
        Syntax_Node_Type type;

        Syntax_Node(Syntax_Node_Type type, Array<SNOT> array, Source_Info const& source_info);
    };

    // transform_syntax_tree_to_ast
    //
    [[nodiscard]] anton::Expected<Declaration_List, Error> transform_syntax_tree_to_ast(Context const& ctx, Array<SNOT> const& syntax);

    enum struct AST_Node_Type {
        identifier,
        builtin_type,
        user_defined_type,
        array_type,
        declaration_if,
        import_declaration,
        variable_declaration,
        constant_declaration,
        struct_member,
        struct_declaration,
        settings_declaration,
        workgroup_attribute,
        function_param_if,
        image_layout_qualifier,
        function_parameter,
        function_declaration,
        overloaded_function_declaration,
        pass_stage_declaration,
        expression_if,
        identifier_expression,
        binary_expression,
        prefix_expression,
        function_call_expression,
        member_access_expression,
        array_access_expression,
        postfix_expression,
        parenthesised_expression,
        reinterpret_expression,
        default_expression,
        string_literal,
        bool_literal,
        integer_literal,
        float_literal,
        block_statement,
        if_statement,
        case_statement,
        switch_statement,
        for_statement,
        while_statement,
        do_while_statement,
        return_statement,
        break_statement,
        continue_statement,
        discard_statement,
        declaration_statement,
        expression_statement,
    };

    // AST_Node
    // Base class of all typed syntax nodes.
    //
    struct AST_Node {
        Source_Info source_info;
        AST_Node_Type node_type;

        AST_Node(Source_Info const& source_info, AST_Node_Type node_type);
        AST_Node(AST_Node const&) = delete;
        virtual ~AST_Node() = default;

        [[nodiscard]] Owning_Ptr<AST_Node> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual AST_Node* _clone(Allocator* allocator) const = 0;
    };

    struct Identifier: public AST_Node {
        anton::String value;

        Identifier(anton::String value, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Identifier> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Identifier* _clone(Allocator* allocator) const override;
    };

    struct Type: public AST_Node {
        using AST_Node::AST_Node;

        [[nodiscard]] Owning_Ptr<Type> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Type* _clone(Allocator* allocator) const override = 0;
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

    [[nodiscard]] bool is_opaque_type(Builtin_GLSL_Type type);
    [[nodiscard]] bool is_image_type(Builtin_GLSL_Type type);
    [[nodiscard]] anton::Optional<Builtin_GLSL_Type> enumify_builtin_glsl_type(anton::String_View type);
    [[nodiscard]] anton::String_View stringify(Builtin_GLSL_Type type);

    struct Builtin_Type: public Type {
        Builtin_GLSL_Type type;

        Builtin_Type(Builtin_GLSL_Type type, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Builtin_Type> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Builtin_Type* _clone(Allocator* allocator) const override;
    };

    struct User_Defined_Type: public Type {
        anton::String identifier;

        User_Defined_Type(anton::String identifier, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<User_Defined_Type> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual User_Defined_Type* _clone(Allocator* allocator) const override;
    };

    struct Array_Type: public Type {
        Owning_Ptr<Type> base;
        // nullptr when the array type is unsized
        Owning_Ptr<Integer_Literal> size;

        Array_Type(Owning_Ptr<Type> base, Owning_Ptr<Integer_Literal> size, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Array_Type> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Array_Type* _clone(Allocator* allocator) const override;
    };

    [[nodiscard]] bool operator==(Type const& lhs, Type const& rhs);
    [[nodiscard]] bool operator!=(Type const& lhs, Type const& rhs);
    [[nodiscard]] bool is_void(Type const& type);
    [[nodiscard]] bool is_opaque_type(Type const& type);
    [[nodiscard]] bool is_unsized_array(Type const& type);
    [[nodiscard]] bool is_sized_array(Type const& type);
    [[nodiscard]] bool is_image_type(Type const& type);
    [[nodiscard]] anton::String stringify_type(Type const& type);

    struct Declaration: public AST_Node {
        using AST_Node::AST_Node;

        [[nodiscard]] Owning_Ptr<Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Declaration* _clone(Allocator* allocator) const override = 0;
    };

    struct Declaration_If: public Declaration {
        Owning_Ptr<Expression> condition;
        Declaration_List true_declarations;
        Declaration_List false_declarations;

        Declaration_If(Owning_Ptr<Expression> condition, Declaration_List true_declarations, Declaration_List false_declarations,
                       Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Declaration_If> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Declaration_If* _clone(Allocator* allocator) const override;
    };

    struct Import_Declaration: public Declaration {
        Owning_Ptr<String_Literal> path;

        Import_Declaration(Owning_Ptr<String_Literal> path, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Import_Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Import_Declaration* _clone(Allocator* allocator) const override;
    };

    enum struct Interpolation {
        none,
        smooth,
        flat,
        noperspective,
    };

    [[nodiscard]] constexpr anton::String_View stringify(Interpolation const interpolation) {
        switch(interpolation) {
            case Interpolation::none:
                return u8"";
            case Interpolation::smooth:
                return u8"smooth";
            case Interpolation::flat:
                return u8"flat";
            case Interpolation::noperspective:
                return u8"noperspective";
        }
    }

    struct Variable_Declaration: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        // nullptr when the declaration does not have an initializer.
        Owning_Ptr<Expression> initializer;

        Variable_Declaration(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Variable_Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Variable_Declaration* _clone(Allocator* allocator) const override;
    };

    struct Constant_Declaration: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        // nullptr when the declaration does not have an initializer.
        Owning_Ptr<Expression> initializer;

        Constant_Declaration(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Constant_Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Constant_Declaration* _clone(Allocator* allocator) const override;
    };

    struct Struct_Member: public AST_Node {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        // nullptr when the member does not have an initializer.
        Owning_Ptr<Expression> initializer;
        // TODO: Turn interpolation and invariant into actual AST nodes to preserve source information.
        Interpolation interpolation;
        // Whether the member is qualified with 'invariant'.
        bool invariant;

        Struct_Member(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer, Interpolation interpolation, bool invariant,
                      Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Struct_Member> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Struct_Member* _clone(Allocator* allocator) const override;
    };

    struct Struct_Declaration: public Declaration {
        Array<Owning_Ptr<Struct_Member>> members;
        Owning_Ptr<Identifier> identifier;

        Struct_Declaration(Owning_Ptr<Identifier> identifier, Array<Owning_Ptr<Struct_Member>> members, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Struct_Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Struct_Declaration* _clone(Allocator* allocator) const override;
    };

    struct Settings_Declaration: public Declaration {
        Owning_Ptr<Identifier> pass_name;
        Array<Setting_Key_Value> settings;

        Settings_Declaration(Owning_Ptr<Identifier> pass_name, Array<Setting_Key_Value> settings, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Settings_Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Settings_Declaration* _clone(Allocator* allocator) const override;
    };

    struct Attribute: public AST_Node {
        using AST_Node::AST_Node;

        [[nodiscard]] Owning_Ptr<Attribute> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Attribute* _clone(Allocator* allocator) const override = 0;
    };

    struct Workgroup_Attribute: public Attribute {
        Owning_Ptr<Integer_Literal> x;
        Owning_Ptr<Integer_Literal> y;
        Owning_Ptr<Integer_Literal> z;

        Workgroup_Attribute(Owning_Ptr<Integer_Literal> x, Owning_Ptr<Integer_Literal> y, Owning_Ptr<Integer_Literal> z, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Workgroup_Attribute> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Workgroup_Attribute* _clone(Allocator* allocator) const override;
    };

    struct Function_Parameter_Node: public AST_Node {
        using AST_Node::AST_Node;

        [[nodiscard]] Owning_Ptr<Function_Parameter_Node> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Function_Parameter_Node* _clone(Allocator* allocator) const override = 0;
    };

    struct Function_Param_If: public Function_Parameter_Node {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Function_Parameter_Node> true_param;
        // nullptr when else branch is not defined
        Owning_Ptr<Function_Parameter_Node> false_param;

        Function_Param_If(Owning_Ptr<Expression> condition, Owning_Ptr<Function_Parameter_Node> true_param, Owning_Ptr<Function_Parameter_Node> false_param,
                          Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Function_Param_If> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Function_Param_If* _clone(Allocator* allocator) const override;
    };

    struct Layout_Qualifier: public AST_Node {
        using AST_Node::AST_Node;

        [[nodiscard]] Owning_Ptr<Layout_Qualifier> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Layout_Qualifier* _clone(Allocator* allocator) const override = 0;
    };

    enum struct Image_Layout_Type {
        rgba32f,
        rgba16f,
        rg32f,
        rg16f,
        r11f_g11f_b10f,
        r32f,
        r16f,
        rgba16,
        rgb10_a2,
        rgba8,
        rg16,
        rg8,
        r16,
        r8,
        rgba16_snorm,
        rgba8_snorm,
        rg16_snorm,
        rg8_snorm,
        r16_snorm,
        r8_snorm,
        rgba32i,
        rgba16i,
        rgba8i,
        rg32i,
        rg16i,
        rg8i,
        r32i,
        r16i,
        r8i,
        rgba32ui,
        rgba16ui,
        rgb10_a2ui,
        rgba8ui,
        rg32ui,
        rg16ui,
        rg8ui,
        r32ui,
        r16ui,
        r8ui,
    };

    [[nodiscard]] anton::String_View stringify(Image_Layout_Type type);

    struct Image_Layout_Qualifier: public Layout_Qualifier {
        Image_Layout_Type type;

        Image_Layout_Qualifier(Image_Layout_Type type, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Image_Layout_Qualifier> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Image_Layout_Qualifier* _clone(Allocator* allocator) const override;
    };

    struct Function_Parameter: public Function_Parameter_Node {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        // nullptr when the parameter has no source.
        // "in" when the parameter is a vertex input parameter.
        Owning_Ptr<Identifier> source;
        // nullptr if the qualifier is not present
        Owning_Ptr<Image_Layout_Qualifier> image_layout;

        Function_Parameter(Owning_Ptr<Identifier> identifier, Owning_Ptr<Type> type, Owning_Ptr<Identifier> source,
                           Owning_Ptr<Image_Layout_Qualifier> image_layout, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Function_Parameter> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Function_Parameter* _clone(Allocator* allocator) const override;
    };

    [[nodiscard]] bool is_sourced_parameter(Function_Parameter const& parameter);
    [[nodiscard]] bool is_vertex_input_parameter(Function_Parameter const& parameter);

    struct Function_Declaration: public Declaration {
        Attribute_List attributes;
        Parameter_List parameters;
        Statement_List body;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> return_type;
        // Whether the function is a builtin function.
        bool builtin = false;

        Function_Declaration(Attribute_List attributes, Owning_Ptr<Type> return_type, Owning_Ptr<Identifier> identifier, Parameter_List parameters,
                             Statement_List body, bool builtin, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Function_Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Function_Declaration* _clone(Allocator* allocator) const override;
    };

    struct Overloaded_Function_Declaration: public Declaration {
        Owning_Ptr<Identifier> identifier;
        Array<Owning_Ptr<Function_Declaration>> overloads;
        // Whether the function is a builtin function.
        bool builtin = false;

        Overloaded_Function_Declaration(Owning_Ptr<Identifier> identifier, bool builtin, Source_Info const& source_info);
        Overloaded_Function_Declaration(Owning_Ptr<Identifier> identifier, Array<Owning_Ptr<Function_Declaration>> overloads, bool builtin,
                                        Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Overloaded_Function_Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Overloaded_Function_Declaration* _clone(Allocator* allocator) const override;
    };

    constexpr anton::String_View stringify(Stage_Type type) {
        switch(type) {
            case Stage_Type::vertex:
                return u8"vertex";
            case Stage_Type::fragment:
                return u8"fragment";
            case Stage_Type::compute:
                return u8"compute";
        }
    }

    struct Pass_Stage_Declaration: public Declaration {
        Attribute_List attributes;
        Parameter_List parameters;
        Statement_List body;
        Owning_Ptr<Identifier> pass_name;
        Owning_Ptr<Type> return_type;
        Stage_Type stage_type;

        Pass_Stage_Declaration(Attribute_List attributes, Owning_Ptr<Type> return_type, Owning_Ptr<Identifier> pass_name, Stage_Type stage_type,
                               Parameter_List parameters, Statement_List body, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Pass_Stage_Declaration> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Pass_Stage_Declaration* _clone(Allocator* allocator) const override;
    };

    struct Expression: public AST_Node {
        using AST_Node::AST_Node;

        [[nodiscard]] Owning_Ptr<Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Expression* _clone(Allocator* allocator) const override = 0;
    };

    struct Expression_If: public Expression {
        Owning_Ptr<Expression> condition;
        // Never nullptr
        Owning_Ptr<Expression> true_expression;
        // Never nullptr
        Owning_Ptr<Expression> false_expression;

        Expression_If(Owning_Ptr<Expression> condition, Owning_Ptr<Expression> true_expression, Owning_Ptr<Expression> false_expression,
                      Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Expression_If> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Expression_If* _clone(Allocator* allocator) const override;
    };

    struct Identifier_Expression: public Expression {
        anton::String value;

        Identifier_Expression(anton::String value, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Identifier_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Identifier_Expression* _clone(Allocator* allocator) const override;
    };

    enum struct Binary_Expression_Type {
        assign,
        add_assign,
        sub_assign,
        mul_assign,
        div_assign,
        mod_assign,
        shl_assign,
        shr_assign,
        band_assign,
        bor_assign,
        bxor_assign,
        lor,
        lxor,
        land,
        eq,
        neq,
        gt,
        lt,
        gteq,
        lteq,
        bor,
        bxor,
        band,
        shl,
        shr,
        add,
        sub,
        mul,
        div,
        mod
    };

    struct Binary_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        Binary_Expression_Type type;

        Binary_Expression(Binary_Expression_Type type, Owning_Ptr<Expression> lhs, Owning_Ptr<Expression> rhs, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Binary_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Binary_Expression* _clone(Allocator* allocator) const override;
    };

    enum struct Prefix_Expression_Type { plus, minus, bnot, lnot, inc, dec };

    struct Prefix_Expression: public Expression {
        Owning_Ptr<Expression> expression;
        Prefix_Expression_Type type;

        Prefix_Expression(Prefix_Expression_Type type, Owning_Ptr<Expression> expression, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Prefix_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Prefix_Expression* _clone(Allocator* allocator) const override;
    };

    struct Function_Call_Expression: public Expression {
        Expression_List arguments;
        Owning_Ptr<Identifier> identifier;

        Function_Call_Expression(Owning_Ptr<Identifier> identifier, Expression_List arguments, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Function_Call_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Function_Call_Expression* _clone(Allocator* allocator) const override;
    };

    struct Member_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Identifier> member;

        Member_Access_Expression(Owning_Ptr<Expression> base, Owning_Ptr<Identifier> member, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Member_Access_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Member_Access_Expression* _clone(Allocator* allocator) const override;
    };

    struct Array_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Expression> index;

        Array_Access_Expression(Owning_Ptr<Expression> base, Owning_Ptr<Expression> index, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Array_Access_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Array_Access_Expression* _clone(Allocator* allocator) const override;
    };

    enum struct Postfix_Expression_Type { inc, dec };

    struct Postfix_Expression: public Expression {
        Owning_Ptr<Expression> expression;
        Postfix_Expression_Type type;

        Postfix_Expression(Postfix_Expression_Type type, Owning_Ptr<Expression> expression, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Postfix_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Postfix_Expression* _clone(Allocator* allocator) const override;
    };

    struct Parenthesised_Expression: public Expression {
        Owning_Ptr<Expression> expression;

        Parenthesised_Expression(Owning_Ptr<Expression> expression, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Parenthesised_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Parenthesised_Expression* _clone(Allocator* allocator) const override;
    };

    struct Reinterpret_Expression: public Expression {
        Owning_Ptr<Type> target_type;
        Owning_Ptr<Expression> source;
        Owning_Ptr<Expression> index;

        Reinterpret_Expression(Owning_Ptr<Type> target_type, Owning_Ptr<Expression> source, Owning_Ptr<Expression> index, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Reinterpret_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Reinterpret_Expression* _clone(Allocator* allocator) const override;
    };

    // Default_Expression
    // Used as switch statement's label
    //
    struct Default_Expression: public Expression {
        Default_Expression(Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Default_Expression> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Default_Expression* _clone(Allocator* allocator) const override;
    };

    struct String_Literal: public Expression {
        anton::String value;

        String_Literal(anton::String value, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<String_Literal> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual String_Literal* _clone(Allocator* allocator) const override;
    };

    struct Bool_Literal: public Expression {
        bool value;

        Bool_Literal(bool value, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Bool_Literal> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Bool_Literal* _clone(Allocator* allocator) const override;
    };

    enum struct Integer_Literal_Type { i32, u32 };
    enum struct Integer_Literal_Base { dec = 10, bin = 2, oct = 8, hex = 16 };

    struct Integer_Literal: public Expression {
        anton::String value;
        Integer_Literal_Type type;
        Integer_Literal_Base base;

        Integer_Literal(anton::String value, Integer_Literal_Type type, Integer_Literal_Base base, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Integer_Literal> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Integer_Literal* _clone(Allocator* allocator) const override;
    };

    enum struct Float_Literal_Type { f32, f64 };

    struct Float_Literal: public Expression {
        anton::String value;
        Float_Literal_Type type;

        Float_Literal(anton::String value, Float_Literal_Type type, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Float_Literal> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Float_Literal* _clone(Allocator* allocator) const override;
    };

    struct Statement: public AST_Node {
        using AST_Node::AST_Node;

        [[nodiscard]] Owning_Ptr<Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Statement* _clone(Allocator* allocator) const override = 0;
    };

    struct Block_Statement: public Statement {
        Statement_List statements;

        Block_Statement(Statement_List statements, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Block_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Block_Statement* _clone(Allocator* allocator) const override;
    };

    struct If_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Statement_List true_statements;
        Statement_List false_statements;

        If_Statement(Owning_Ptr<Expression> condition, Statement_List true_statements, Statement_List false_statements, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<If_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual If_Statement* _clone(Allocator* allocator) const override;
    };

    struct Case_Statement: public Statement {
        Expression_List labels;
        Statement_List statements;

        Case_Statement(Expression_List labels, Statement_List statements, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Case_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Case_Statement* _clone(Allocator* allocator) const override;
    };

    struct Switch_Statement: public Statement {
        Array<Owning_Ptr<Case_Statement>> cases;
        Owning_Ptr<Expression> match_expression;

        Switch_Statement(Owning_Ptr<Expression> match_expression, Array<Owning_Ptr<Case_Statement>> cases, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Switch_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Switch_Statement* _clone(Allocator* allocator) const override;
    };

    struct For_Statement: public Statement {
        Owning_Ptr<Variable_Declaration> declaration;
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> post_expression;
        Statement_List statements;

        For_Statement(Owning_Ptr<Variable_Declaration> declaration, Owning_Ptr<Expression> condition, Owning_Ptr<Expression> post_expression,
                      Statement_List statements, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<For_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual For_Statement* _clone(Allocator* allocator) const override;
    };

    struct While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Statement_List statements;

        While_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<While_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual While_Statement* _clone(Allocator* allocator) const override;
    };

    struct Do_While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Statement_List statements;

        Do_While_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Do_While_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Do_While_Statement* _clone(Allocator* allocator) const override;
    };

    struct Return_Statement: public Statement {
        // May be nullptr when the return statement doesn't return anything
        Owning_Ptr<Expression> return_expression;

        Return_Statement(Owning_Ptr<Expression> return_expression, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Return_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Return_Statement* _clone(Allocator* allocator) const override;
    };

    struct Break_Statement: public Statement {
        Break_Statement(Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Break_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Break_Statement* _clone(Allocator* allocator) const override;
    };

    struct Continue_Statement: public Statement {
        Continue_Statement(Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Continue_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Continue_Statement* _clone(Allocator* allocator) const override;
    };

    struct Discard_Statement: public Statement {
        Discard_Statement(Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Discard_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Discard_Statement* _clone(Allocator* allocator) const override;
    };

    struct Declaration_Statement: public Statement {
        Owning_Ptr<Declaration> declaration;

        Declaration_Statement(Owning_Ptr<Declaration> declaration, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Declaration_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Declaration_Statement* _clone(Allocator* allocator) const override;
    };

    struct Expression_Statement: public Statement {
        Owning_Ptr<Expression> expression;

        Expression_Statement(Owning_Ptr<Expression> expression, Source_Info const& source_info);

        [[nodiscard]] Owning_Ptr<Expression_Statement> clone(Allocator* allocator) const;

    private:
        [[nodiscard]] virtual Expression_Statement* _clone(Allocator* allocator) const override;
    };
} // namespace vush
