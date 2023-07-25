#pragma once

#include <anton/optional.hpp>

#include <ast_fwd.hpp>
#include <either.hpp>
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

    // Syntax_Node_Kind
    // Overlaps with lexer's Token_Kind.
    //
    enum struct Syntax_Node_Kind {
        identifier,
        comment,
        whitespace,
        // keywords
        kw_if,
        kw_else,
        kw_switch,
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
        kw_var,
        kw_mut,
        kw_settings,
        kw_reinterpret,
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
        tk_at,
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
        lt_dec_integer,
        lt_hex_integer,
        lt_float,
        lt_string,
        lt_bool,

        // compound tokens
        tk_amp2, // &&
        tk_pipe2, // ||
        tk_hat2, // ^^
        tk_shl, // <<
        tk_shr, // >>
        tk_eq2, // ==
        tk_neq, // !=
        tk_lteq, // <=
        tk_gteq, // >=
        // TODO: Reintroduce.
        // tk_pluseq, // +=
        // tk_minuseq, // -=
        // tk_asteriskeq, // *=
        // tk_slasheq, // /=
        // tk_percenteq, // %=
        // tk_ampeq, // &=
        // tk_pipeeq, // |=
        // tk_hateq, // ^=
        // tk_shleq, // <<=
        // tk_shreq, // >>=
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

        // The base type of array.
        type_array_base,
        type_array_size,

        variable,

        struct_member,
        struct_member_block,

        func_parameter_if,
        func_parameter,
        func_parameter_list,

        attribute,
        attribute_list,
        attribute_parameter_keyed,
        attribute_parameter_positional,
        attribute_parameter_list,

        decl_block,
        decl_if,
        decl_import,
        decl_struct,
        decl_settings,
        decl_function,
        decl_stage_function,

        setting_block,
        setting_keyval,

        named_initializer,
        indexed_initializer,
        basic_initializer,

        init_initializer_list,
        call_arg_list,

        expr_block,
        expr_if,
        expr_identifier,
        expr_binary,
        expr_prefix,
        expr_member_access,
        expr_array_access,
        expr_parentheses,
        expr_reinterpret,
        expr_init,
        expr_call,
        expr_lt_bool,
        expr_lt_integer,
        expr_lt_float,
        expr_lt_string,
        // expr_default
        // The 'default' label in stmt_case.
        //
        expr_default,

        stmt_block,
        stmt_if,
        stmt_switch,
        stmt_for,
        stmt_while,
        stmt_do_while,
        stmt_return,
        stmt_break,
        stmt_continue,
        stmt_discard,
        stmt_expression,
        stmt_empty,

        switch_arm_list,
        switch_arm,
        switch_arm_label,
        return_expression,
        for_variable,
        for_condition,
        for_expression,
    };

    struct Syntax_Token;
    struct Syntax_Node;

    // Syntax Node Or Token
    using SNOT = Either<Syntax_Node, Syntax_Token>;

    struct Syntax_Token {
        anton::String value;
        Source_Info source_info;
        Syntax_Node_Kind type;

        Syntax_Token(Syntax_Node_Kind type, anton::String value, Source_Info const& source_info);
    };

    // Syntax_Node
    // Untyped syntax node containing syntax information.
    //
    struct Syntax_Node {
        Array<SNOT> children;
        Source_Info source_info;
        Syntax_Node_Kind type;

        Syntax_Node(Syntax_Node_Kind type, Array<SNOT> array, Source_Info const& source_info);
    };

    // transform_syntax_tree_to_ast
    //
    [[nodiscard]] anton::Expected<ast::Node_List, Error> transform_syntax_tree_to_ast(Context& ctx, Array<SNOT> const& syntax);

    // import_source_code
    // Imports, parses and transforms source code into abstract syntax tree (AST).
    // Resolves declaration ifs. Recursively imports sources required by any
    // imported sources and splices them into the AST.
    //
    // Parameters:
    //         ctx - vush context.
    // source_name - name of the source to be imported.
    // source_info - source information of the import declaration.
    //
    // Returns:
    // ast::Node_List containing ast::Node's of the source or Error.
    // The list will be empty if the source has already been imported before.
    //
    [[nodiscard]] anton::Expected<ast::Node_List, Error> import_source_code(Context& ctx, anton::String_View const source_name,
                                                                            anton::Optional<Source_Info> source_info = anton::null_optional);

    namespace ast {
        template<typename T>
        struct With_Source {
            T value;
            Source_Info source_info;
        };

        enum struct Node_Kind : u8 {
            identifier,

            type_builtin,
            type_user_defined,
            type_array,

            attribute,
            variable,

            func_parameter,
            struct_member,

            decl_struct,
            decl_function,
            decl_overloaded_function,
            decl_stage_function,

            named_initializer,
            indexed_initializer,
            basic_initializer,

            expr_if,
            expr_identifier,
            expr_assignment,
            expr_init,
            expr_call,
            expr_member_access,
            expr_array_access,
            expr_parentheses,
            expr_reinterpret,
            expr_default,
            lt_bool,
            lt_integer,
            lt_float,

            switch_arm,

            stmt_block,
            stmt_if,
            stmt_switch,
            stmt_loop,
            stmt_return,
            stmt_break,
            stmt_continue,
            stmt_discard,
            stmt_expression,
        };

        // Node
        //
        //
        struct Node {
            Source_Info source_info;
            Node_Kind node_kind;

            constexpr Node(Source_Info const& source_info, Node_Kind node_kind): source_info(source_info), node_kind(node_kind) {}
        };

        struct Identifier: public Node {
            anton::String_View value;

            constexpr Identifier(anton::String_View value, Source_Info const& source_info): Node(source_info, Node_Kind::identifier), value(value) {}
        };

        struct Qualifiers {
            bool mut = false;
        };

        struct Type: public Node {
            Qualifiers qualifiers;

            constexpr Type(Source_Info const& source_info, Node_Kind node_kind): Node(source_info, node_kind) {}
            constexpr Type(Qualifiers qualifiers, Source_Info const& source_info, Node_Kind node_kind): Node(source_info, node_kind), qualifiers(qualifiers) {}
        };

        [[nodiscard]] bool compare_types_equal(Type const& lhs, Type const& rhs);
        [[nodiscard]] bool is_void(Type const& type);
        [[nodiscard]] bool is_integer(Type const& type);
        [[nodiscard]] bool is_matrix(Type const& type);
        [[nodiscard]] bool is_vector(Type const& type);
        [[nodiscard]] bool is_opaque_type(Type const& type);
        [[nodiscard]] bool is_array(Type const& type);
        [[nodiscard]] bool is_unsized_array(Type const& type);
        [[nodiscard]] bool is_sized_array(Type const& type);
        [[nodiscard]] bool is_image_type(Type const& type);
        [[nodiscard]] anton::String stringify_type(Allocator* const allocator, Type const& type);

        // TODO: Rename to Builtin_Type_Kind.
        enum struct GLSL_Type : i32 {
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

        [[nodiscard]] bool is_opaque_glsl_type(GLSL_Type type);
        [[nodiscard]] bool is_image_glsl_type(GLSL_Type type);
        [[nodiscard]] anton::Optional<GLSL_Type> enumify_glsl_type(anton::String_View type);
        [[nodiscard]] anton::String_View stringify_glsl_type(GLSL_Type type);

        struct Type_Builtin: public Type {
            GLSL_Type value;

            constexpr Type_Builtin(GLSL_Type value, Source_Info const& source_info): Type(source_info, Node_Kind::type_builtin), value(value) {}
            constexpr Type_Builtin(Qualifiers qualifiers, GLSL_Type value, Source_Info const& source_info)
                : Type(qualifiers, source_info, Node_Kind::type_builtin), value(value) {}
        };

        struct Type_User_Defined: public Type {
            // The identifier value, that is the name of the type.
            anton::String_View value;

            constexpr Type_User_Defined(anton::String_View value, Source_Info const& source_info)
                : Type(source_info, Node_Kind::type_user_defined), value(value) {}
            constexpr Type_User_Defined(Qualifiers qualifiers, anton::String_View value, Source_Info const& source_info)
                : Type(qualifiers, source_info, Node_Kind::type_user_defined), value(value) {}
        };

        struct Type_Array: public Type {
            Type const* base;
            // nullptr when the array is unsized.
            Lt_Integer const* size;

            constexpr Type_Array(Type const* base, Lt_Integer const* size, Source_Info const& source_info)
                : Type(source_info, Node_Kind::type_array), base(base), size(size) {}
            constexpr Type_Array(Qualifiers qualifiers, Type const* base, Lt_Integer const* size, Source_Info const& source_info)
                : Type(qualifiers, source_info, Node_Kind::type_array), base(base), size(size) {}
        };

        [[nodiscard]] bool is_type(Node const& node);

        struct Attribute_Parameter {
            // nullptr if the parameters positional.
            Identifier const* key;
            Expr const* value;
        };

        struct Attribute: public Node {
            Identifier const* identifier;
            anton::Slice<Attribute_Parameter const> parameters;

            constexpr Attribute(Identifier const* identifier, anton::Slice<Attribute_Parameter const> parameters, Source_Info const& source_info)
                : Node(source_info, Node_Kind::attribute), identifier(identifier), parameters(parameters) {}
        };

        struct Variable: public Node {
            Type const* type;
            Identifier const* identifier;
            // nullptr when Variable does not have an initializer.
            Expr const* initializer;

            constexpr Variable(Type const* type, Identifier const* identifier, Expr const* initializer, Source_Info const& source_info)
                : Node(source_info, Node_Kind::variable), type(type), identifier(identifier), initializer(initializer) {}
        };

        struct Struct_Member: public Node {
            Attr_List attributes;
            Identifier const* identifier;
            Type const* type;
            // nullptr when the member does not have an initializer.
            Expr const* initializer;

            constexpr Struct_Member(Attr_List attributes, Identifier const* identifier, Type const* type, Expr const* initializer,
                                    Source_Info const& source_info)
                : Node(source_info, Node_Kind::struct_member), attributes(attributes), identifier(identifier), type(type), initializer(initializer) {}
        };

        struct Decl_Struct: public Node {
            Identifier const* identifier;
            Member_List members;

            constexpr Decl_Struct(Identifier const* identifier, Member_List members, Source_Info const& source_info)
                : Node(source_info, Node_Kind::decl_struct), identifier(identifier), members(members) {}
        };

        struct Func_Parameter: public Node {
            Identifier const* identifier;
            Type const* type;
            // nullptr when the parameter has no source.
            // "in" when the parameter is a vertex input parameter.
            Identifier const* source;

            constexpr Func_Parameter(Identifier const* identifier, Type const* type, Identifier const* source, Source_Info const& source_info)
                : Node(source_info, Node_Kind::func_parameter), identifier(identifier), type(type), source(source) {}
        };

        [[nodiscard]] bool is_sourced_parameter(Func_Parameter const& parameter);
        [[nodiscard]] bool is_vertex_input_parameter(Func_Parameter const& parameter);

        struct Decl_Function: public Node {
            Attr_List attributes;
            Identifier const* identifier;
            Func_Parameter_List parameters;
            Type const* return_type;
            Node_List body;
            // Whether the function is a builtin function.
            bool builtin;

            constexpr Decl_Function(Attr_List attributes, Identifier const* identifier, Func_Parameter_List parameters, Type const* return_type, Node_List body,
                                    bool builtin, Source_Info const& source_info)
                : Node(source_info, Node_Kind::decl_function), attributes(attributes), identifier(identifier), parameters(parameters), return_type(return_type),
                  body(body), builtin(builtin) {}
        };

        // Decl_Overloaded_Function
        // A semantic node representing overloaded function. This node is not backed by syntax
        // and acts as a binding node of all functions with identical identifiers (overloads).
        // Due to being a collection of references to other nodes rather than a standalone
        // semantic node, this node does not have source information.
        //
        struct Decl_Overloaded_Function: public Node {
            anton::String_View identifier;
            anton::Slice<Decl_Function const* const> overloads;

            constexpr Decl_Overloaded_Function(anton::String_View identifier, anton::Slice<Decl_Function const* const> overloads)
                : Node(Source_Info{}, Node_Kind::decl_overloaded_function), identifier(identifier), overloads(overloads) {}
        };

        struct Decl_Stage_Function: public Node {
            Attr_List attributes;
            Identifier const* pass;
            With_Source<Stage_Kind> stage;
            Func_Parameter_List parameters;
            Type const* return_type;
            Node_List body;

            constexpr Decl_Stage_Function(Attr_List attributes, Identifier const* pass, With_Source<Stage_Kind> stage, Func_Parameter_List parameters,
                                          Type const* return_type, Node_List body, Source_Info const& source_info)
                : Node(source_info, Node_Kind::decl_stage_function), attributes(attributes), pass(pass), stage(stage), parameters(parameters),
                  return_type(return_type), body(body) {}
        };

        struct Expr: public Node {
            using Node::Node;
        };

        struct Expr_If: public Expr {
            Expr const* condition;
            Expr const* then_branch;
            Expr const* else_branch;

            constexpr Expr_If(Expr const* condition, Expr const* then_branch, Expr const* else_branch, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::expr_if), condition(condition), then_branch(then_branch), else_branch(else_branch) {}
        };

        struct Expr_Identifier: public Expr {
            anton::String_View value;

            constexpr Expr_Identifier(anton::String_View value, Source_Info const& source_info): Expr(source_info, Node_Kind::expr_identifier), value(value) {}
        };

        struct Expr_Assignment: public Expr {
            Expr const* lhs;
            Expr const* rhs;

            constexpr Expr_Assignment(Expr const* lhs, Expr const* rhs, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::expr_assignment), lhs(lhs), rhs(rhs) {}
        };

        struct Initializer: public Node {
            using Node::Node;
        };

        struct Named_Initializer: public Initializer {
            Identifier const* identifier;
            Expr const* expression;

            constexpr Named_Initializer(Identifier const* identifier, Expr const* expression, Source_Info const& source_info)
                : Initializer(source_info, Node_Kind::named_initializer), identifier(identifier), expression(expression) {}
        };

        struct Indexed_Initializer: public Initializer {
            Lt_Integer const* index;
            Expr const* expression;

            constexpr Indexed_Initializer(Lt_Integer const* index, Expr const* expression, Source_Info const& source_info)
                : Initializer(source_info, Node_Kind::indexed_initializer), index(index), expression(expression) {}
        };

        struct Basic_Initializer: public Initializer {
            Expr const* expression;

            constexpr Basic_Initializer(Expr const* expression, Source_Info const& source_info)
                : Initializer(source_info, Node_Kind::basic_initializer), expression(expression) {}
        };

        struct Expr_Init: public Expr {
            Type const* type;
            Initializer_List initializers;

            constexpr Expr_Init(Type const* type, Initializer_List initializers, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::expr_init), type(type), initializers(initializers) {}
        };

        struct Expr_Call: public Expr {
            Identifier const* identifier;
            Expr_List arguments;

            constexpr Expr_Call(Identifier const* identifier, Expr_List arguments, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::expr_call), identifier(identifier), arguments(arguments) {}
        };

        struct Expr_Member_Access: public Expr {
            Expr const* base;
            Identifier const* member;

            constexpr Expr_Member_Access(Expr const* base, Identifier const* member, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::expr_member_access), base(base), member(member) {}
        };

        struct Expr_Array_Access: public Expr {
            Expr const* base;
            Expr const* index;

            constexpr Expr_Array_Access(Expr const* base, Expr const* index, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::expr_array_access), base(base), index(index) {}
        };

        struct Expr_Parentheses: public Expr {
            Expr const* expression;

            constexpr Expr_Parentheses(Expr const* expression, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::expr_parentheses), expression(expression) {}
        };

        struct Expr_Reinterpret: public Expr {
            Type const* target_type;
            Expr const* source;
            Expr const* index;

            constexpr Expr_Reinterpret(Type const* target_type, Expr const* source, Expr const* index, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::expr_reinterpret), target_type(target_type), source(source), index(index) {}
        };

        struct Expr_Default: public Expr {
            constexpr Expr_Default(Source_Info const& source_info): Expr(source_info, Node_Kind::expr_default) {}
        };

        struct Lt_Bool: public Expr {
            bool value;

            constexpr Lt_Bool(bool value, Source_Info const& source_info): Expr(source_info, Node_Kind::lt_bool), value(value) {}
        };

        enum struct Lt_Integer_Kind { i32, u32 };
        enum struct Lt_Integer_Base { dec = 10, bin = 2, hex = 16 };

        struct Lt_Integer: public Expr {
            anton::String_View value;
            Lt_Integer_Kind kind;
            Lt_Integer_Base base;

            constexpr Lt_Integer(anton::String_View value, Lt_Integer_Kind kind, Lt_Integer_Base base, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::lt_integer), value(value), kind(kind), base(base) {}
        };

        enum struct Lt_Float_Kind { f32, f64 };

        struct Lt_Float: public Expr {
            anton::String_View value;
            Lt_Float_Kind kind;

            constexpr Lt_Float(anton::String_View value, Lt_Float_Kind kind, Source_Info const& source_info)
                : Expr(source_info, Node_Kind::lt_float), value(value), kind(kind) {}
        };

        struct Switch_Arm: public Node {
            Expr_List labels;
            Node_List statements;

            constexpr Switch_Arm(Expr_List labels, Node_List statements, Source_Info const& source_info)
                : Node(source_info, Node_Kind::switch_arm), labels(labels), statements(statements) {}
        };

        struct Stmt_Block: public Node {
            Node_List statements;

            constexpr Stmt_Block(Node_List statements, Source_Info const& source_info): Node(source_info, Node_Kind::stmt_block), statements(statements) {}
        };

        struct Stmt_If: public Node {
            Expr const* condition;
            Node_List then_branch;
            Node_List else_branch;

            constexpr Stmt_If(Expr const* condition, Node_List then_branch, Node_List else_branch, Source_Info const& source_info)
                : Node(source_info, Node_Kind::stmt_if), condition(condition), then_branch(then_branch), else_branch(else_branch) {}
        };

        struct Stmt_Switch: public Node {
            Expr const* expression;
            anton::Slice<Switch_Arm const* const> arms;

            constexpr Stmt_Switch(Expr const* const expression, anton::Slice<Switch_Arm const* const> arms, Source_Info const& source_info)
                : Node(source_info, Node_Kind::stmt_switch), expression(expression), arms(arms) {}
        };

        struct Stmt_Loop: public Node {
            // nullptr if the loop does not have a condition.
            Expr const* condition;
            // continue statements are not allowed within the continuation block.
            Node_List continuation;
            Node_List statements;

            constexpr Stmt_Loop(Expr const* condition, Node_List continuation, Node_List statements, Source_Info const& source_info)
                : Node(source_info, Node_Kind::stmt_loop), condition(condition), continuation(continuation), statements(statements) {}
        };

        struct Stmt_Return: public Node {
            // nullptr if expression not present.
            Expr const* expression;

            constexpr Stmt_Return(Expr const* expression, Source_Info const& source_info): Node(source_info, Node_Kind::stmt_return), expression(expression) {}
        };

        struct Stmt_Break: public Node {
            constexpr Stmt_Break(Source_Info const& source_info): Node(source_info, Node_Kind::stmt_break) {}
        };

        struct Stmt_Continue: public Node {
            constexpr Stmt_Continue(Source_Info const& source_info): Node(source_info, Node_Kind::stmt_continue) {}
        };

        struct Stmt_Discard: public Node {
            constexpr Stmt_Discard(Source_Info const& source_info): Node(source_info, Node_Kind::stmt_discard) {}
        };

        struct Stmt_Expression: public Node {
            Expr const* expression;

            constexpr Stmt_Expression(Expr const* expression, Source_Info const& source_info)
                : Node(source_info, Node_Kind::stmt_expression), expression(expression) {}
        };
    } // namespace ast
} // namespace vush
