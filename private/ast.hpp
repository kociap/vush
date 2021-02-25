#pragma once

#include <anton/optional.hpp>
#include <ast_fwd.hpp>
#include <owning_ptr.hpp>
#include <vush/vush.hpp>

namespace vush {
    enum struct AST_Node_Type {
        identifier,
        builtin_type,
        user_defined_type,
        array_type,
        declaration_if,
        import_decl,
        variable_declaration,
        constant_declaration,
        struct_member,
        struct_decl,
        settings_decl,
        workgroup_attribute,
        function_param_if,
        ordinary_function_param,
        sourced_function_param,
        vertex_input_param,
        function_declaration,
        pass_stage_declaration,
        expression_if,
        identifier_expression,
        assignment_expression,
        arithmetic_assignment_expression,
        elvis_expr,
        binary_expr,
        unary_expression,
        prefix_inc_expr,
        prefix_dec_expr,
        function_call_expression,
        member_access_expression,
        array_access_expression,
        postfix_inc_expr,
        postfix_dec_expr,
        paren_expr,
        reinterpret_expr,
        string_literal,
        bool_literal,
        integer_literal,
        float_literal,
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
        discard_statement,
        declaration_statement,
        expression_statement,
    };

    struct Source_Info {
        anton::String_View file_path;
        i64 line = 0;
        i64 column = 0;
        // The offset into the source at which the matched node starts
        i64 start_offset = 0;
        i64 end_line = 0;
        i64 end_column = 0;
        // The offset into the source at which the matched node ends
        i64 end_offset = 0;
    };

    struct AST_Node {
        Source_Info source_info;
        AST_Node_Type node_type;

        AST_Node(Source_Info const& source_info, AST_Node_Type node_type);
        AST_Node(AST_Node const&) = delete;
        virtual ~AST_Node() = default;

        Owning_Ptr<AST_Node> clone() const;

    private:
        virtual AST_Node* _clone() const = 0;
    };

    struct Identifier: public AST_Node {
        anton::String value;

        Identifier(anton::String value, Source_Info const& source_info);

        Owning_Ptr<Identifier> clone() const;

    private:
        virtual Identifier* _clone() const override;
    };

    struct Type: public AST_Node {
        using AST_Node::AST_Node;

        Owning_Ptr<Type> clone() const;

    private:
        virtual Type* _clone() const override = 0;
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
    [[nodiscard]] anton::Optional<Builtin_GLSL_Type> enumify_builtin_glsl_type(anton::String_View type);
    [[nodiscard]] anton::String_View stringify(Builtin_GLSL_Type type);

    struct Builtin_Type: public Type {
        Builtin_GLSL_Type type;

        Builtin_Type(Builtin_GLSL_Type type, Source_Info const& source_info);

        Owning_Ptr<Builtin_Type> clone() const;

    private:
        virtual Builtin_Type* _clone() const override;
    };

    struct User_Defined_Type: public Type {
        anton::String name;

        User_Defined_Type(anton::String name, Source_Info const& source_info);

        Owning_Ptr<User_Defined_Type> clone() const;

    private:
        virtual User_Defined_Type* _clone() const override;
    };

    struct Array_Type: public Type {
        Owning_Ptr<Type> base;
        // nullptr when the array type is unsized
        Owning_Ptr<Integer_Literal> size;

        Array_Type(Owning_Ptr<Type> base, Owning_Ptr<Integer_Literal> size, Source_Info const& source_info);

        Owning_Ptr<Array_Type> clone() const;

    private:
        virtual Array_Type* _clone() const override;
    };

    [[nodiscard]] bool is_opaque_type(Type const& type);
    [[nodiscard]] anton::String stringify_type(Type const& type);
    [[nodiscard]] bool is_unsized_array(Type const& type);
    [[nodiscard]] bool is_sized_array(Type const& type);

    struct Declaration: public AST_Node {
        using AST_Node::AST_Node;

        Owning_Ptr<Declaration> clone() const;

    private:
        virtual Declaration* _clone() const override = 0;
    };

    struct Declaration_If: public Declaration {
        Owning_Ptr<Expression> condition;
        Declaration_List true_declarations;
        Declaration_List false_declarations;

        Declaration_If(Owning_Ptr<Expression> condition, Declaration_List true_declarations, Declaration_List false_declarations,
                       Source_Info const& source_info);

        Owning_Ptr<Declaration_If> clone() const;

    private:
        virtual Declaration_If* _clone() const override;
    };

    struct Import_Decl: public Declaration {
        Owning_Ptr<String_Literal> path;

        Import_Decl(Owning_Ptr<String_Literal> path, Source_Info const& source_info);

        Owning_Ptr<Import_Decl> clone() const;

    private:
        virtual Import_Decl* _clone() const override;
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
        // nullptr when the declaration does not have an initializer
        Owning_Ptr<Expression> initializer;

        Variable_Declaration(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer, Source_Info const& source_info);

        Owning_Ptr<Variable_Declaration> clone() const;

    private:
        virtual Variable_Declaration* _clone() const override;
    };

    struct Constant_Declaration: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        // nullptr when the declaration does not have an initializer
        Owning_Ptr<Expression> initializer;

        Constant_Declaration(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer, Source_Info const& source_info);

        Owning_Ptr<Constant_Declaration> clone() const;

    private:
        virtual Constant_Declaration* _clone() const override;
    };

    struct Struct_Member: public AST_Node {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Expression> initializer;
        Interpolation interpolation;
        // Whether the member is qualified with 'invariant'
        bool invariant;

        Struct_Member(Owning_Ptr<Type> type, Owning_Ptr<Identifier> identifier, Owning_Ptr<Expression> initializer, Interpolation interpolation, bool invariant,
                      Source_Info const& source_info);

        Owning_Ptr<Struct_Member> clone() const;

    private:
        virtual Struct_Member* _clone() const override;
    };

    struct Struct_Decl: public Declaration {
        anton::Array<Owning_Ptr<Struct_Member>> members;
        Owning_Ptr<Identifier> identifier;

        Struct_Decl(Owning_Ptr<Identifier> identifier, anton::Array<Owning_Ptr<Struct_Member>> members, Source_Info const& source_info);

        Owning_Ptr<Struct_Decl> clone() const;

    private:
        virtual Struct_Decl* _clone() const override;
    };

    struct Settings_Decl: public Declaration {
        Owning_Ptr<Identifier> pass_name;
        anton::Array<Setting_Key_Value> settings;

        Settings_Decl(Owning_Ptr<Identifier> pass_name, Source_Info const& source_info);
        Settings_Decl(Owning_Ptr<Identifier> pass_name, anton::Array<Setting_Key_Value> settings, Source_Info const& source_info);

        Owning_Ptr<Settings_Decl> clone() const;

    private:
        virtual Settings_Decl* _clone() const override;
    };

    struct Function_Attribute: public AST_Node {
        using AST_Node::AST_Node;

        Owning_Ptr<Function_Attribute> clone() const;

    private:
        virtual Function_Attribute* _clone() const override = 0;
    };

    struct Workgroup_Attribute: public Function_Attribute {
        Owning_Ptr<Integer_Literal> x;
        Owning_Ptr<Integer_Literal> y;
        Owning_Ptr<Integer_Literal> z;

        Workgroup_Attribute(Owning_Ptr<Integer_Literal> x, Owning_Ptr<Integer_Literal> y, Owning_Ptr<Integer_Literal> z, Source_Info const& source_info);

        Owning_Ptr<Workgroup_Attribute> clone() const;

    private:
        virtual Workgroup_Attribute* _clone() const override;
    };

    struct Function_Param: public AST_Node {
        using AST_Node::AST_Node;

        Owning_Ptr<Function_Param> clone() const;

    private:
        virtual Function_Param* _clone() const override = 0;
    };

    struct Function_Param_If: public Function_Param {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Function_Param> true_param;
        // nullptr when else branch is not defined
        Owning_Ptr<Function_Param> false_param;

        Function_Param_If(Owning_Ptr<Expression> condition, Owning_Ptr<Function_Param> true_param, Owning_Ptr<Function_Param> false_param,
                          Source_Info const& source_info);

        Owning_Ptr<Function_Param_If> clone() const;

    private:
        virtual Function_Param_If* _clone() const override;
    };

    struct Ordinary_Function_Param: public Function_Param {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;

        Ordinary_Function_Param(Owning_Ptr<Identifier> identifier, Owning_Ptr<Type> type, Source_Info const& source_info);

        Owning_Ptr<Ordinary_Function_Param> clone() const;

    private:
        virtual Ordinary_Function_Param* _clone() const override;
    };

    struct Sourced_Function_Param: public Function_Param {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Identifier> source;

        Sourced_Function_Param(Owning_Ptr<Identifier> identifier, Owning_Ptr<Type> type, Owning_Ptr<Identifier> source, Source_Info const& source_info);

        Owning_Ptr<Sourced_Function_Param> clone() const;

    private:
        virtual Sourced_Function_Param* _clone() const override;
    };

    struct Vertex_Input_Param: public Function_Param {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;

        Vertex_Input_Param(Owning_Ptr<Identifier> identifier, Owning_Ptr<Type> type, Source_Info const& source_info);

        Owning_Ptr<Vertex_Input_Param> clone() const;

    private:
        virtual Vertex_Input_Param* _clone() const override;
    };

    struct Function_Declaration: public Declaration {
        anton::Array<Owning_Ptr<Function_Param>> params;
        anton::Array<Owning_Ptr<Function_Attribute>> attributes;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> return_type;
        Statement_List body;

        Function_Declaration(anton::Array<Owning_Ptr<Function_Attribute>> attributes, Owning_Ptr<Type> return_type, Owning_Ptr<Identifier> identifier,
                             anton::Array<Owning_Ptr<Function_Param>> params, Statement_List body, Source_Info const& source_info);

        Owning_Ptr<Function_Declaration> clone() const;

    private:
        virtual Function_Declaration* _clone() const override;
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
        anton::Array<Owning_Ptr<Function_Param>> params;
        anton::Array<Owning_Ptr<Function_Attribute>> attributes;
        Statement_List body;
        Owning_Ptr<Identifier> pass;
        Owning_Ptr<Type> return_type;
        Stage_Type stage;

        Pass_Stage_Declaration(anton::Array<Owning_Ptr<Function_Attribute>> attributes, Owning_Ptr<Type> return_type, Owning_Ptr<Identifier> pass,
                               Stage_Type stage, anton::Array<Owning_Ptr<Function_Param>> params, Statement_List body, Source_Info const& source_info);

        Owning_Ptr<Pass_Stage_Declaration> clone() const;

    private:
        virtual Pass_Stage_Declaration* _clone() const override;
    };

    struct Expression: public AST_Node {
        using AST_Node::AST_Node;

        Owning_Ptr<Expression> clone() const;

    private:
        virtual Expression* _clone() const override = 0;
    };

    // Both true_expr and false_expr are never nullptr
    struct Expression_If: public Expression {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> true_expr;
        Owning_Ptr<Expression> false_expr;

        Expression_If(Owning_Ptr<Expression> condition, Owning_Ptr<Expression> true_expr, Owning_Ptr<Expression> false_expr, Source_Info const& source_info);

        Owning_Ptr<Expression_If> clone() const;

    private:
        virtual Expression_If* _clone() const override;
    };

    struct Identifier_Expression: public Expression {
        Owning_Ptr<Identifier> identifier;

        Identifier_Expression(Owning_Ptr<Identifier> identifier, Source_Info const& source_info);

        Owning_Ptr<Identifier_Expression> clone() const;

    private:
        virtual Identifier_Expression* _clone() const override;
    };

    struct Assignment_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Assignment_Expression(Owning_Ptr<Expression> lhs, Owning_Ptr<Expression> rhs, Source_Info const& source_info);

        Owning_Ptr<Assignment_Expression> clone() const;

    private:
        virtual Assignment_Expression* _clone() const override;
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

        Arithmetic_Assignment_Expression(Arithmetic_Assignment_Type type, Owning_Ptr<Expression> lhs, Owning_Ptr<Expression> rhs,
                                         Source_Info const& source_info);

        Owning_Ptr<Arithmetic_Assignment_Expression> clone() const;

    private:
        virtual Arithmetic_Assignment_Expression* _clone() const override;
    };

    struct Elvis_Expr: public Expression {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> true_expr;
        Owning_Ptr<Expression> false_expr;

        Elvis_Expr(Owning_Ptr<Expression> condition, Owning_Ptr<Expression> true_expr, Owning_Ptr<Expression> false_expr, Source_Info const& source_info);

        Owning_Ptr<Elvis_Expr> clone() const;

    private:
        virtual Elvis_Expr* _clone() const override;
    };

    enum struct Binary_Expr_Type {
        logic_or,
        logic_xor,
        logic_and,
        equal,
        unequal,
        greater_than,
        less_than,
        greater_equal,
        less_equal,
        bit_or,
        bit_xor,
        bit_and,
        lshift,
        rshift,
        add,
        sub,
        mul,
        div,
        mod,
    };

    struct Binary_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        Binary_Expr_Type type;

        Binary_Expr(Binary_Expr_Type type, Owning_Ptr<Expression> lhs, Owning_Ptr<Expression> rhs, Source_Info const& source_info);

        Owning_Ptr<Binary_Expr> clone() const;

    private:
        virtual Binary_Expr* _clone() const override;
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

        Unary_Expression(Unary_Type type, Owning_Ptr<Expression> expression, Source_Info const& source_info);

        Owning_Ptr<Unary_Expression> clone() const;

    private:
        virtual Unary_Expression* _clone() const override;
    };

    struct Prefix_Inc_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Prefix_Inc_Expr(Owning_Ptr<Expression> expression, Source_Info const& source_info);

        Owning_Ptr<Prefix_Inc_Expr> clone() const;

    private:
        virtual Prefix_Inc_Expr* _clone() const override;
    };

    struct Prefix_Dec_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Prefix_Dec_Expr(Owning_Ptr<Expression> expression, Source_Info const& source_info);

        Owning_Ptr<Prefix_Dec_Expr> clone() const;

    private:
        virtual Prefix_Dec_Expr* _clone() const override;
    };

    struct Function_Call_Expression: public Expression {
        Owning_Ptr<Identifier> identifier;
        anton::Array<Owning_Ptr<Expression>> arguments;

        Function_Call_Expression(Owning_Ptr<Identifier> identifier, anton::Array<Owning_Ptr<Expression>> arguments, Source_Info const& source_info);

        Owning_Ptr<Function_Call_Expression> clone() const;

    private:
        virtual Function_Call_Expression* _clone() const override;
    };

    struct Member_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Identifier> member;

        Member_Access_Expression(Owning_Ptr<Expression> base, Owning_Ptr<Identifier> member, Source_Info const& source_info);

        Owning_Ptr<Member_Access_Expression> clone() const;

    private:
        virtual Member_Access_Expression* _clone() const override;
    };

    struct Array_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Expression> index;

        Array_Access_Expression(Owning_Ptr<Expression> base, Owning_Ptr<Expression> index, Source_Info const& source_info);

        Owning_Ptr<Array_Access_Expression> clone() const;

    private:
        virtual Array_Access_Expression* _clone() const override;
    };

    struct Postfix_Inc_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Postfix_Inc_Expr(Owning_Ptr<Expression> expression, Source_Info const& source_info);

        Owning_Ptr<Postfix_Inc_Expr> clone() const;

    private:
        virtual Postfix_Inc_Expr* _clone() const override;
    };

    struct Postfix_Dec_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Postfix_Dec_Expr(Owning_Ptr<Expression> expression, Source_Info const& source_info);

        Owning_Ptr<Postfix_Dec_Expr> clone() const;

    private:
        virtual Postfix_Dec_Expr* _clone() const override;
    };

    struct Paren_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Paren_Expr(Owning_Ptr<Expression> expression, Source_Info const& source_info);

        Owning_Ptr<Paren_Expr> clone() const;

    private:
        virtual Paren_Expr* _clone() const override;
    };

    struct Reinterpret_Expr: public Expression {
        Owning_Ptr<Type> target_type;
        Owning_Ptr<Expression> source;
        Owning_Ptr<Expression> index;

        Reinterpret_Expr(Owning_Ptr<Type> target_type, Owning_Ptr<Expression> source, Owning_Ptr<Expression> index, Source_Info const& source_info);

        Owning_Ptr<Reinterpret_Expr> clone() const;

    private:
        virtual Reinterpret_Expr* _clone() const override;
    };

    struct String_Literal: public Expression {
        anton::String value;

        String_Literal(anton::String value, Source_Info const& source_info);

        Owning_Ptr<String_Literal> clone() const;

    private:
        virtual String_Literal* _clone() const override;
    };

    struct Bool_Literal: public Expression {
        bool value;

        Bool_Literal(bool value, Source_Info const& source_info);

        Owning_Ptr<Bool_Literal> clone() const;

    private:
        virtual Bool_Literal* _clone() const override;
    };

    enum struct Integer_Literal_Type { i32, u32 };
    enum struct Integer_Literal_Base { hex = 16, oct = 8, dec = 10 };

    struct Integer_Literal: public Expression {
        anton::String value;
        Integer_Literal_Type type;
        Integer_Literal_Base base;

        Integer_Literal(anton::String value, Integer_Literal_Type type, Integer_Literal_Base base, Source_Info const& source_info);

        Owning_Ptr<Integer_Literal> clone() const;

    private:
        virtual Integer_Literal* _clone() const override;
    };

    enum struct Float_Literal_Type { f32, f64 };

    struct Float_Literal: public Expression {
        anton::String value;
        Float_Literal_Type type;

        Float_Literal(anton::String value, Float_Literal_Type type, Source_Info const& source_info);

        Owning_Ptr<Float_Literal> clone() const;

    private:
        virtual Float_Literal* _clone() const override;
    };

    struct Statement: public AST_Node {
        using AST_Node::AST_Node;

        Owning_Ptr<Statement> clone() const;

    private:
        virtual Statement* _clone() const override = 0;
    };

    struct Block_Statement: public Statement {
        Statement_List statements;

        Block_Statement(Statement_List statements, Source_Info const& source_info);

        Owning_Ptr<Block_Statement> clone() const;

    private:
        virtual Block_Statement* _clone() const override;
    };

    struct If_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Statement_List true_statements;
        Statement_List false_statements;

        If_Statement(Owning_Ptr<Expression> condition, Statement_List true_statements, Statement_List false_statements, Source_Info const& source_info);

        Owning_Ptr<If_Statement> clone() const;

    private:
        virtual If_Statement* _clone() const override;
    };

    struct Case_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Statement_List statements;

        Case_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info);

        Owning_Ptr<Case_Statement> clone() const;

    private:
        virtual Case_Statement* _clone() const override;
    };

    struct Default_Case_Statement: public Statement {
        Statement_List statements;

        Default_Case_Statement(Statement_List statements, Source_Info const& source_info);

        Owning_Ptr<Default_Case_Statement> clone() const;

    private:
        virtual Default_Case_Statement* _clone() const override;
    };

    struct Switch_Statement: public Statement {
        Statement_List cases;
        Owning_Ptr<Expression> match_expr;

        Switch_Statement(Owning_Ptr<Expression> match_expr, Statement_List cases, Source_Info const& source_info);

        Owning_Ptr<Switch_Statement> clone() const;

    private:
        virtual Switch_Statement* _clone() const override;
    };

    struct For_Statement: public Statement {
        Owning_Ptr<Variable_Declaration> declaration;
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> post_expression;
        Statement_List statements;

        For_Statement(Owning_Ptr<Variable_Declaration> declaration, Owning_Ptr<Expression> condition, Owning_Ptr<Expression> post_expression,
                      Statement_List statements, Source_Info const& source_info);

        Owning_Ptr<For_Statement> clone() const;

    private:
        virtual For_Statement* _clone() const override;
    };

    struct While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Statement_List statements;

        While_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info);

        Owning_Ptr<While_Statement> clone() const;

    private:
        virtual While_Statement* _clone() const override;
    };

    struct Do_While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Statement_List statements;

        Do_While_Statement(Owning_Ptr<Expression> condition, Statement_List statements, Source_Info const& source_info);

        Owning_Ptr<Do_While_Statement> clone() const;

    private:
        virtual Do_While_Statement* _clone() const override;
    };

    // return_expr may be nullptr
    struct Return_Statement: public Statement {
        Owning_Ptr<Expression> return_expr;

        Return_Statement(Owning_Ptr<Expression> return_expr, Source_Info const& source_info);

        Owning_Ptr<Return_Statement> clone() const;

    private:
        virtual Return_Statement* _clone() const override;
    };

    struct Break_Statement: public Statement {
        Break_Statement(Source_Info const& source_info);

        Owning_Ptr<Break_Statement> clone() const;

    private:
        virtual Break_Statement* _clone() const override;
    };

    struct Continue_Statement: public Statement {
        Continue_Statement(Source_Info const& source_info);

        Owning_Ptr<Continue_Statement> clone() const;

    private:
        virtual Continue_Statement* _clone() const override;
    };

    struct Discard_Statement: public Statement {
        Discard_Statement(Source_Info const& source_info);

        Owning_Ptr<Discard_Statement> clone() const;

    private:
        virtual Discard_Statement* _clone() const override;
    };

    struct Declaration_Statement: public Statement {
        Owning_Ptr<Declaration> declaration;

        Declaration_Statement(Owning_Ptr<Declaration> declaration, Source_Info const& source_info);

        Owning_Ptr<Declaration_Statement> clone() const;

    private:
        virtual Declaration_Statement* _clone() const override;
    };

    struct Expression_Statement: public Statement {
        Owning_Ptr<Expression> expression;

        Expression_Statement(Owning_Ptr<Expression> expression, Source_Info const& source_info);

        Owning_Ptr<Expression_Statement> clone() const;

    private:
        virtual Expression_Statement* _clone() const override;
    };
} // namespace vush
