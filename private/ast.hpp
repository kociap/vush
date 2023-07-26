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
    tk_lbrace,
    tk_rbrace,
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
    // A special type of string only used by setting key and values. This token contains anything
    // other than comments, whitespace, colons, left braces or right braces.
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

    fn_parameter_if,
    fn_parameter,
    fn_parameter_list,

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
    expr_field,
    expr_index,
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
    Syntax_Node_Kind kind;

    Syntax_Token(Syntax_Node_Kind kind, anton::String value, Source_Info const& source_info);
  };

  // Syntax_Node
  // Untyped syntax node containing syntax information.
  //
  struct Syntax_Node {
    Array<SNOT> children;
    Source_Info source_info;
    Syntax_Node_Kind kind;

    Syntax_Node(Syntax_Node_Kind kind, Array<SNOT> array, Source_Info const& source_info);
  };

  // transform_syntax_tree_to_ast
  //
  [[nodiscard]] anton::Expected<ast::Node_List, Error>
  transform_syntax_tree_to_ast(Context& ctx, Array<SNOT> const& syntax);

  // import_source_code
  // Imports, parses and transforms source code into abstract syntax tree (AST). Resolves
  // declaration ifs. Recursively imports sources required by any imported sources and splices them
  // into the AST.
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
  [[nodiscard]] anton::Expected<ast::Node_List, Error>
  import_source_code(Context& ctx, anton::String_View const source_name,
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

      fn_parameter,
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
      expr_field,
      expr_index,
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

      constexpr Node(Source_Info const& source_info, Node_Kind node_kind)
        : source_info(source_info), node_kind(node_kind)
      {
      }
    };

    struct Identifier: public Node {
      anton::String_View value;

      constexpr Identifier(anton::String_View value, Source_Info const& source_info)
        : Node(source_info, Node_Kind::identifier), value(value)
      {
      }
    };

    struct Qualifiers {
      bool mut = false;
    };

    struct Type: public Node {
      Qualifiers qualifiers;

      constexpr Type(Source_Info const& source_info, Node_Kind node_kind)
        : Node(source_info, node_kind)
      {
      }
      constexpr Type(Qualifiers qualifiers, Source_Info const& source_info, Node_Kind node_kind)
        : Node(source_info, node_kind), qualifiers(qualifiers)
      {
      }
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

    enum struct Type_Builtin_Kind : i32 {
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
      e_sampler1D,
      e_texture1D,
      e_image1D,
      e_sampler1DShadow,
      e_sampler1DArray,
      e_texture1DArray,
      e_image1DArray,
      e_sampler1DArrayShadow,
      e_sampler2D,
      e_texture2D,
      e_image2D,
      e_sampler2DShadow,
      e_sampler2DArray,
      e_texture2DArray,
      e_image2DArray,
      e_sampler2DArrayShadow,
      e_sampler2DMS,
      e_texture2DMS,
      e_image2DMS,
      e_sampler2DMSArray,
      e_texture2DMSArray,
      e_image2DMSArray,
      e_sampler2DRect,
      e_texture2DRect,
      e_image2DRect,
      e_sampler2DRectShadow,
      e_sampler3D,
      e_texture3D,
      e_image3D,
      e_samplerCube,
      e_textureCube,
      e_imageCube,
      e_samplerCubeShadow,
      e_samplerCubeArray,
      e_textureCubeArray,
      e_imageCubeArray,
      e_samplerCubeArrayShadow,
      e_samplerBuffer,
      e_textureBuffer,
      e_imageBuffer,
      e_subpassInput,
      e_subpassInputMS,
      e_isampler1D,
      e_itexture1D,
      e_iimage1D,
      e_isampler1DArray,
      e_itexture1DArray,
      e_iimage1DArray,
      e_isampler2D,
      e_itexture2D,
      e_iimage2D,
      e_isampler2DArray,
      e_itexture2DArray,
      e_iimage2DArray,
      e_isampler2DMS,
      e_itexture2DMS,
      e_iimage2DMS,
      e_isampler2DMSArray,
      e_itexture2DMSArray,
      e_iimage2DMSArray,
      e_isampler2DRect,
      e_itexture2DRect,
      e_iimage2DRect,
      e_isampler3D,
      e_itexture3D,
      e_iimage3D,
      e_isamplerCube,
      e_itextureCube,
      e_iimageCube,
      e_isamplerCubeArray,
      e_itextureCubeArray,
      e_iimageCubeArray,
      e_isamplerBuffer,
      e_itextureBuffer,
      e_iimageBuffer,
      e_isubpassInput,
      e_isubpassInputMS,
      e_usampler1D,
      e_utexture1D,
      e_uimage1D,
      e_usampler1DArray,
      e_utexture1DArray,
      e_uimage1DArray,
      e_usampler2D,
      e_utexture2D,
      e_uimage2D,
      e_usampler2DArray,
      e_utexture2DArray,
      e_uimage2DArray,
      e_usampler2DMS,
      e_utexture2DMS,
      e_uimage2DMS,
      e_usampler2DMSArray,
      e_utexture2DMSArray,
      e_uimage2DMSArray,
      e_usampler2DRect,
      e_utexture2DRect,
      e_uimage2DRect,
      e_usampler3D,
      e_utexture3D,
      e_uimage3D,
      e_usamplerCube,
      e_utextureCube,
      e_uimageCube,
      e_usamplerCubeArray,
      e_utextureCubeArray,
      e_uimageCubeArray,
      e_usamplerBuffer,
      e_utextureBuffer,
      e_uimageBuffer,
      e_usubpassInput,
      e_usubpassInputMS,
      e_sampler,
      e_samplerShadow,
    };

    [[nodiscard]] bool is_opaque_builtin_type_kind(Type_Builtin_Kind type);
    [[nodiscard]] bool is_image_builtin_type_kind(Type_Builtin_Kind type);
    [[nodiscard]] anton::Optional<Type_Builtin_Kind>
    enumify_builtin_type_kind(anton::String_View type);
    [[nodiscard]] anton::String_View stringify_builtin_type_kind(Type_Builtin_Kind type);

    struct Type_Builtin: public Type {
      Type_Builtin_Kind value;

      constexpr Type_Builtin(Type_Builtin_Kind value, Source_Info const& source_info)
        : Type(source_info, Node_Kind::type_builtin), value(value)
      {
      }
      constexpr Type_Builtin(Qualifiers qualifiers, Type_Builtin_Kind value,
                             Source_Info const& source_info)
        : Type(qualifiers, source_info, Node_Kind::type_builtin), value(value)
      {
      }
    };

    struct Type_User_Defined: public Type {
      // The identifier value, that is the name of the type.
      anton::String_View value;

      constexpr Type_User_Defined(anton::String_View value, Source_Info const& source_info)
        : Type(source_info, Node_Kind::type_user_defined), value(value)
      {
      }
      constexpr Type_User_Defined(Qualifiers qualifiers, anton::String_View value,
                                  Source_Info const& source_info)
        : Type(qualifiers, source_info, Node_Kind::type_user_defined), value(value)
      {
      }
    };

    struct Type_Array: public Type {
      Type const* base;
      // nullptr when the array is unsized.
      Lt_Integer const* size;

      constexpr Type_Array(Type const* base, Lt_Integer const* size, Source_Info const& source_info)
        : Type(source_info, Node_Kind::type_array), base(base), size(size)
      {
      }
      constexpr Type_Array(Qualifiers qualifiers, Type const* base, Lt_Integer const* size,
                           Source_Info const& source_info)
        : Type(qualifiers, source_info, Node_Kind::type_array), base(base), size(size)
      {
      }
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

      constexpr Attribute(Identifier const* identifier,
                          anton::Slice<Attribute_Parameter const> parameters,
                          Source_Info const& source_info)
        : Node(source_info, Node_Kind::attribute), identifier(identifier), parameters(parameters)
      {
      }
    };

    struct Variable: public Node {
      Type const* type;
      Identifier const* identifier;
      // nullptr when Variable does not have an initializer.
      Expr const* initializer;

      constexpr Variable(Type const* type, Identifier const* identifier, Expr const* initializer,
                         Source_Info const& source_info)
        : Node(source_info, Node_Kind::variable), type(type), identifier(identifier),
          initializer(initializer)
      {
      }
    };

    struct Struct_Member: public Node {
      Attr_List attributes;
      Identifier const* identifier;
      Type const* type;
      // nullptr when the member does not have an initializer.
      Expr const* initializer;

      constexpr Struct_Member(Attr_List attributes, Identifier const* identifier, Type const* type,
                              Expr const* initializer, Source_Info const& source_info)
        : Node(source_info, Node_Kind::struct_member), attributes(attributes),
          identifier(identifier), type(type), initializer(initializer)
      {
      }
    };

    struct Decl_Struct: public Node {
      Identifier const* identifier;
      Member_List members;

      constexpr Decl_Struct(Identifier const* identifier, Member_List members,
                            Source_Info const& source_info)
        : Node(source_info, Node_Kind::decl_struct), identifier(identifier), members(members)
      {
      }
    };

    struct Fn_Parameter: public Node {
      Identifier const* identifier;
      Type const* type;
      // nullptr when the parameter has no source.
      // "in" when the parameter is a vertex input parameter.
      Identifier const* source;

      constexpr Fn_Parameter(Identifier const* identifier, Type const* type,
                             Identifier const* source, Source_Info const& source_info)
        : Node(source_info, Node_Kind::fn_parameter), identifier(identifier), type(type),
          source(source)
      {
      }
    };

    [[nodiscard]] bool is_sourced_parameter(Fn_Parameter const& parameter);
    [[nodiscard]] bool is_vertex_input_parameter(Fn_Parameter const& parameter);

    struct Decl_Function: public Node {
      Attr_List attributes;
      Identifier const* identifier;
      Fn_Parameter_List parameters;
      Type const* return_type;
      Node_List body;
      // Whether the function is a builtin function.
      bool builtin;

      constexpr Decl_Function(Attr_List attributes, Identifier const* identifier,
                              Fn_Parameter_List parameters, Type const* return_type, Node_List body,
                              bool builtin, Source_Info const& source_info)
        : Node(source_info, Node_Kind::decl_function), attributes(attributes),
          identifier(identifier), parameters(parameters), return_type(return_type), body(body),
          builtin(builtin)
      {
      }
    };

    // Decl_Overloaded_Function
    // A semantic node representing overloaded function. This node is not backed by syntax and acts
    // as a binding node of all functions with identical identifiers (overloads). Due to being a
    // collection of references to other nodes rather than a standalone semantic node, this node
    // does not have source information.
    //
    struct Decl_Overloaded_Function: public Node {
      anton::String_View identifier;
      anton::Slice<Decl_Function const* const> overloads;

      constexpr Decl_Overloaded_Function(anton::String_View identifier,
                                         anton::Slice<Decl_Function const* const> overloads)
        : Node(Source_Info{}, Node_Kind::decl_overloaded_function), identifier(identifier),
          overloads(overloads)
      {
      }
    };

    struct Decl_Stage_Function: public Node {
      Attr_List attributes;
      Identifier const* pass;
      With_Source<Stage_Kind> stage;
      Fn_Parameter_List parameters;
      Type const* return_type;
      Node_List body;

      constexpr Decl_Stage_Function(Attr_List attributes, Identifier const* pass,
                                    With_Source<Stage_Kind> stage, Fn_Parameter_List parameters,
                                    Type const* return_type, Node_List body,
                                    Source_Info const& source_info)
        : Node(source_info, Node_Kind::decl_stage_function), attributes(attributes), pass(pass),
          stage(stage), parameters(parameters), return_type(return_type), body(body)
      {
      }
    };

    struct Expr: public Node {
      using Node::Node;
    };

    struct Expr_If: public Expr {
      Expr const* condition;
      Expr const* then_branch;
      Expr const* else_branch;

      constexpr Expr_If(Expr const* condition, Expr const* then_branch, Expr const* else_branch,
                        Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_if), condition(condition), then_branch(then_branch),
          else_branch(else_branch)
      {
      }
    };

    struct Expr_Identifier: public Expr {
      anton::String_View value;

      constexpr Expr_Identifier(anton::String_View value, Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_identifier), value(value)
      {
      }
    };

    struct Expr_Assignment: public Expr {
      Expr const* lhs;
      Expr const* rhs;

      constexpr Expr_Assignment(Expr const* lhs, Expr const* rhs, Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_assignment), lhs(lhs), rhs(rhs)
      {
      }
    };

    struct Initializer: public Node {
      using Node::Node;
    };

    struct Named_Initializer: public Initializer {
      Identifier const* identifier;
      Expr const* expression;

      constexpr Named_Initializer(Identifier const* identifier, Expr const* expression,
                                  Source_Info const& source_info)
        : Initializer(source_info, Node_Kind::named_initializer), identifier(identifier),
          expression(expression)
      {
      }
    };

    struct Indexed_Initializer: public Initializer {
      Lt_Integer const* index;
      Expr const* expression;

      constexpr Indexed_Initializer(Lt_Integer const* index, Expr const* expression,
                                    Source_Info const& source_info)
        : Initializer(source_info, Node_Kind::indexed_initializer), index(index),
          expression(expression)
      {
      }
    };

    struct Basic_Initializer: public Initializer {
      Expr const* expression;

      constexpr Basic_Initializer(Expr const* expression, Source_Info const& source_info)
        : Initializer(source_info, Node_Kind::basic_initializer), expression(expression)
      {
      }
    };

    struct Expr_Init: public Expr {
      Type const* type;
      Initializer_List initializers;

      constexpr Expr_Init(Type const* type, Initializer_List initializers,
                          Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_init), type(type), initializers(initializers)
      {
      }
    };

    struct Expr_Call: public Expr {
      Identifier const* identifier;
      Expr_List arguments;

      constexpr Expr_Call(Identifier const* identifier, Expr_List arguments,
                          Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_call), identifier(identifier), arguments(arguments)
      {
      }
    };

    struct Expr_Field: public Expr {
      Expr const* base;
      Identifier const* member;

      constexpr Expr_Field(Expr const* base, Identifier const* member,
                           Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_field), base(base), member(member)
      {
      }
    };

    struct Expr_Index: public Expr {
      Expr const* base;
      Expr const* index;

      constexpr Expr_Index(Expr const* base, Expr const* index, Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_index), base(base), index(index)
      {
      }
    };

    struct Expr_Parentheses: public Expr {
      Expr const* expression;

      constexpr Expr_Parentheses(Expr const* expression, Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_parentheses), expression(expression)
      {
      }
    };

    struct Expr_Reinterpret: public Expr {
      Type const* target_type;
      Expr const* source;
      Expr const* index;

      constexpr Expr_Reinterpret(Type const* target_type, Expr const* source, Expr const* index,
                                 Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_reinterpret), target_type(target_type), source(source),
          index(index)
      {
      }
    };

    struct Expr_Default: public Expr {
      constexpr Expr_Default(Source_Info const& source_info)
        : Expr(source_info, Node_Kind::expr_default)
      {
      }
    };

    struct Lt_Bool: public Expr {
      bool value;

      constexpr Lt_Bool(bool value, Source_Info const& source_info)
        : Expr(source_info, Node_Kind::lt_bool), value(value)
      {
      }
    };

    enum struct Lt_Integer_Kind { i32, u32 };
    enum struct Lt_Integer_Base { dec = 10, bin = 2, hex = 16 };

    struct Lt_Integer: public Expr {
      anton::String_View value;
      Lt_Integer_Kind kind;
      Lt_Integer_Base base;

      constexpr Lt_Integer(anton::String_View value, Lt_Integer_Kind kind, Lt_Integer_Base base,
                           Source_Info const& source_info)
        : Expr(source_info, Node_Kind::lt_integer), value(value), kind(kind), base(base)
      {
      }
    };

    enum struct Lt_Float_Kind { f32, f64 };

    struct Lt_Float: public Expr {
      anton::String_View value;
      Lt_Float_Kind kind;

      constexpr Lt_Float(anton::String_View value, Lt_Float_Kind kind,
                         Source_Info const& source_info)
        : Expr(source_info, Node_Kind::lt_float), value(value), kind(kind)
      {
      }
    };

    struct Switch_Arm: public Node {
      Expr_List labels;
      Node_List statements;

      constexpr Switch_Arm(Expr_List labels, Node_List statements, Source_Info const& source_info)
        : Node(source_info, Node_Kind::switch_arm), labels(labels), statements(statements)
      {
      }
    };

    struct Stmt_Block: public Node {
      Node_List statements;

      constexpr Stmt_Block(Node_List statements, Source_Info const& source_info)
        : Node(source_info, Node_Kind::stmt_block), statements(statements)
      {
      }
    };

    struct Stmt_If: public Node {
      Expr const* condition;
      Node_List then_branch;
      Node_List else_branch;

      constexpr Stmt_If(Expr const* condition, Node_List then_branch, Node_List else_branch,
                        Source_Info const& source_info)
        : Node(source_info, Node_Kind::stmt_if), condition(condition), then_branch(then_branch),
          else_branch(else_branch)
      {
      }
    };

    struct Stmt_Switch: public Node {
      Expr const* expression;
      anton::Slice<Switch_Arm const* const> arms;

      constexpr Stmt_Switch(Expr const* const expression,
                            anton::Slice<Switch_Arm const* const> arms,
                            Source_Info const& source_info)
        : Node(source_info, Node_Kind::stmt_switch), expression(expression), arms(arms)
      {
      }
    };

    struct Stmt_Loop: public Node {
      // nullptr if the loop does not have a condition.
      Expr const* condition;
      // continue statements are not allowed within the continuation block.
      Node_List continuation;
      Node_List statements;

      constexpr Stmt_Loop(Expr const* condition, Node_List continuation, Node_List statements,
                          Source_Info const& source_info)
        : Node(source_info, Node_Kind::stmt_loop), condition(condition), continuation(continuation),
          statements(statements)
      {
      }
    };

    struct Stmt_Return: public Node {
      // nullptr if expression not present.
      Expr const* expression;

      constexpr Stmt_Return(Expr const* expression, Source_Info const& source_info)
        : Node(source_info, Node_Kind::stmt_return), expression(expression)
      {
      }
    };

    struct Stmt_Break: public Node {
      constexpr Stmt_Break(Source_Info const& source_info): Node(source_info, Node_Kind::stmt_break)
      {
      }
    };

    struct Stmt_Continue: public Node {
      constexpr Stmt_Continue(Source_Info const& source_info)
        : Node(source_info, Node_Kind::stmt_continue)
      {
      }
    };

    struct Stmt_Discard: public Node {
      constexpr Stmt_Discard(Source_Info const& source_info)
        : Node(source_info, Node_Kind::stmt_discard)
      {
      }
    };

    struct Stmt_Expression: public Node {
      Expr const* expression;

      constexpr Stmt_Expression(Expr const* expression, Source_Info const& source_info)
        : Node(source_info, Node_Kind::stmt_expression), expression(expression)
      {
      }
    };
  } // namespace ast
} // namespace vush
