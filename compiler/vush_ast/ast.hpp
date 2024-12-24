#pragma once

#include <anton/optional.hpp>
#include <anton/ordering.hpp>
#include <anton/string.hpp>

#include <vush_ast/ast_fwd.hpp>
#include <vush_core/source_info.hpp>
#include <vush_core/types.hpp>

namespace vush {
  struct Context;
}

namespace vush::ast {
  enum struct Node_Kind : u8 {
    type,

    attribute,
    variable,

    decl_struct,
    decl_buffer,

    fn_parameter,
    decl_function,
    decl_stage_function,

    field_initializer,
    index_initializer,
    basic_initializer,

    expr_if,
    expr_identifier,
    expr_init,
    expr_call,
    expr_field,
    expr_index,
    expr_reinterpret,
    expr_default,
    lt_bool,
    lt_integer,
    lt_float,

    stmt_block,
    stmt_assignment,
    stmt_if,
    stmt_switch,
    stmt_while,
    stmt_do_while,
    stmt_for,
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

  template<typename T>
  [[nodiscard]] bool instanceof(Node const& node);

  template<typename T>
  [[nodiscard]] bool instanceof(Node const* node)
  {
    return instanceof<T>(*node);
  }

  struct Qualifiers {
    bool mut = false;
  };

  enum struct Type_Kind : u8 {
    type_builtin,
    type_struct,
    type_array,
  };

  struct Type: public Node {
    Qualifiers qualifiers;
    Type_Kind type_kind;

    constexpr Type(Source_Info const& source_info, Type_Kind type_kind)
      : Node(source_info, Node_Kind::type), type_kind(type_kind)
    {
    }

    constexpr Type(Source_Info const& source_info, Type_Kind type_kind,
                   Qualifiers qualifiers)
      : Node(source_info, Node_Kind::type), qualifiers(qualifiers),
        type_kind(type_kind)
    {
    }
  };

  [[nodiscard]] bool compare_types_equal(Type const& lhs, Type const& rhs);

  // is_integer_based
  // Check whether the type's fundamental operations operate on integer
  // numbers.
  //
  [[nodiscard]] bool is_integer_based(Type const& type);

  // is_signed_integer_based
  // Check whether the type's fundamental operations operate on signed integer
  // numbers.
  //
  [[nodiscard]] bool is_signed_integer_based(Type const& type);

  // is_unsigned_integer_based
  // Check whether the type's fundamental operations operate on unsigned integer
  // numbers.
  //
  [[nodiscard]] bool is_unsigned_integer_based(Type const& type);

  // is_fp_based
  // Check whether the type's fundamental operations operate on floating point
  // numbers.
  //
  [[nodiscard]] bool is_fp_based(Type const& type);

  [[nodiscard]] bool is_void(Type const& type);
  [[nodiscard]] bool is_scalar(Type const& type);
  [[nodiscard]] bool is_integer(Type const& type);
  [[nodiscard]] bool is_fp(Type const& type);
  [[nodiscard]] bool is_signed_integer(Type const& type);
  [[nodiscard]] bool is_unsigned_integer(Type const& type);
  [[nodiscard]] bool is_vector(Type const& type);
  [[nodiscard]] bool is_vector2(Type const& type);
  [[nodiscard]] bool is_vector3(Type const& type);
  [[nodiscard]] bool is_vector4(Type const& type);
  [[nodiscard]] bool is_bool_vector(Type const& type);
  [[nodiscard]] bool is_integer_vector(Type const& type);
  [[nodiscard]] bool is_signed_integer_vector(Type const& type);
  [[nodiscard]] bool is_unsigned_integer_vector(Type const& type);
  [[nodiscard]] bool is_fp_vector(Type const& type);
  [[nodiscard]] bool is_i32_vector(Type const& type);
  [[nodiscard]] bool is_u32_vector(Type const& type);
  [[nodiscard]] bool is_f32_vector(Type const& type);
  [[nodiscard]] bool is_f64_vector(Type const& type);
  [[nodiscard]] bool is_matrix(Type const& type);
  [[nodiscard]] bool is_f32_matrix(Type const& type);
  [[nodiscard]] bool is_f64_matrix(Type const& type);

  // get_vector_size
  // Calculate the size (number of elements) of a vector type.
  //
  // Returns:
  // Size or -1 if not a vector type.
  //
  [[nodiscard]] i64 get_vector_size(Type const& type);

  // get_matrix_rows
  // Calculate the number of matrix rows.
  //
  // Returns:
  // Number of rows or -1 if not a matrix type.
  //
  [[nodiscard]] i64 get_matrix_rows(Type const& type);

  // get_matrix_columns
  // Calculate the number of matrix columns.
  //
  // Returns:
  // Number of columns or -1 if not a matrix type.
  //
  [[nodiscard]] i64 get_matrix_columns(Type const& type);

  // is_opaque_type
  // Check whether a type is opaque, i.e. is an opaque builtin type, a struct
  // containing an opaque type or an array of opaque types.
  //
  // If a struct does not have a corresponding definition, returns true.
  //
  [[nodiscard]] bool is_opaque_type(Type const& type);

  // is_arithmetic_type
  // Check whether type is a type that supports arithmetic operations, e.g.
  // addition.
  //
  [[nodiscard]] bool is_arithmetic_type(Type const& type);

  [[nodiscard]] bool is_array(Type const& type);
  [[nodiscard]] bool is_unsized_array(Type const& type);
  [[nodiscard]] bool is_sized_array(Type const& type);
  [[nodiscard]] bool is_image_type(Type const& type);

  enum struct Type_Builtin_Kind : u8 {
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
    // Image types.
    e_image1D,
    e_image1DArray,
    e_image2D,
    e_image2DArray,
    e_image2DMS,
    e_image2DMSArray,
    e_image2DRect,
    e_image3D,
    e_imageBuffer,
    e_imageCube,
    e_imageCubeArray,
    e_iimage1D,
    e_iimage1DArray,
    e_iimage2D,
    e_iimage2DArray,
    e_iimage2DMS,
    e_iimage2DMSArray,
    e_iimage2DRect,
    e_iimage3D,
    e_iimageBuffer,
    e_iimageCube,
    e_iimageCubeArray,
    e_uimage1D,
    e_uimage1DArray,
    e_uimage2D,
    e_uimage2DArray,
    e_uimage2DMS,
    e_uimage2DMSArray,
    e_uimage2DRect,
    e_uimage3D,
    e_uimageBuffer,
    e_uimageCube,
    e_uimageCubeArray,
    // Sampler types.
    e_sampler,
    e_sampler1D,
    e_sampler1DArray,
    e_sampler1DArrayShadow,
    e_sampler1DShadow,
    e_sampler2D,
    e_sampler2DArray,
    e_sampler2DArrayShadow,
    e_sampler2DMS,
    e_sampler2DMSArray,
    e_sampler2DRect,
    e_sampler2DRectShadow,
    e_sampler2DShadow,
    e_sampler3D,
    e_samplerBuffer,
    e_samplerCube,
    e_samplerCubeArray,
    e_samplerCubeArrayShadow,
    e_samplerCubeShadow,
    e_isampler1D,
    e_isampler1DArray,
    e_isampler2D,
    e_isampler2DArray,
    e_isampler2DMS,
    e_isampler2DMSArray,
    e_isampler2DRect,
    e_isampler3D,
    e_isamplerBuffer,
    e_isamplerCube,
    e_isamplerCubeArray,
    e_usampler1D,
    e_usampler1DArray,
    e_usampler2D,
    e_usampler2DArray,
    e_usampler2DMS,
    e_usampler2DMSArray,
    e_usampler2DRect,
    e_usampler3D,
    e_usamplerBuffer,
    e_usamplerCube,
    e_usamplerCubeArray,
    // Texture types.
    e_texture1D,
    e_texture1DArray,
    e_texture2D,
    e_texture2DArray,
    e_texture2DMS,
    e_texture2DMSArray,
    e_texture2DRect,
    e_texture3D,
    e_textureBuffer,
    e_textureCube,
    e_textureCubeArray,
    e_itexture1D,
    e_itexture1DArray,
    e_itexture2D,
    e_itexture2DArray,
    e_itexture2DMS,
    e_itexture2DMSArray,
    e_itexture2DRect,
    e_itexture3D,
    e_itextureBuffer,
    e_itextureCube,
    e_itextureCubeArray,
    e_utexture1D,
    e_utexture1DArray,
    e_utexture2D,
    e_utexture2DArray,
    e_utexture2DMS,
    e_utexture2DMSArray,
    e_utexture2DRect,
    e_utexture3D,
    e_utextureBuffer,
    e_utextureCube,
    e_utextureCubeArray,
    // Subpass types.
    e_subpassInput,
    e_subpassInputMS,
    e_isubpassInput,
    e_isubpassInputMS,
    e_usubpassInput,
    e_usubpassInputMS,
  };

  [[nodiscard]] anton::Optional<Type_Builtin_Kind>
  enumify_builtin_type_kind(anton::String_View type);

  struct Type_Builtin: public Type {
    Type_Builtin_Kind value;

    constexpr Type_Builtin(Source_Info const& source_info,
                           Type_Builtin_Kind value)
      : Type(source_info, Type_Kind::type_builtin), value(value)
    {
    }

    constexpr Type_Builtin(Source_Info const& source_info,
                           Qualifiers qualifiers, Type_Builtin_Kind value)
      : Type(source_info, Type_Kind::type_builtin, qualifiers), value(value)
    {
    }
  };

  struct Type_Struct: public Type {
    // The identifier value, that is the name of the type.
    anton::String value;
    Decl_Struct* definition = nullptr;

    Type_Struct(Source_Info const& source_info, anton::String&& value)
      : Type(source_info, Type_Kind::type_struct), value(ANTON_MOV(value))
    {
    }

    Type_Struct(Source_Info const& source_info, Qualifiers qualifiers,
                anton::String&& value)
      : Type(source_info, Type_Kind::type_struct, qualifiers),
        value(ANTON_MOV(value))
    {
    }
  };

  struct Type_Array: public Type {
    Type* base;
    // nullptr when the array is unsized.
    Lt_Integer* size;

    constexpr Type_Array(Source_Info const& source_info, Type* base,
                         Lt_Integer* size)
      : Type(source_info, Type_Kind::type_array), base(base), size(size)
    {
    }

    constexpr Type_Array(Source_Info const& source_info, Qualifiers qualifiers,
                         Type* base, Lt_Integer* size)
      : Type(source_info, Type_Kind::type_array, qualifiers), base(base),
        size(size)
    {
    }
  };

  struct Attribute_Parameter {
    // empty if the parameter is positional.
    Identifier key;
    Expr* value;
  };

  struct Attribute: public Node {
    Identifier identifier;
    anton::Slice<Attribute_Parameter> parameters;

    constexpr Attribute(Identifier identifier,
                        anton::Slice<Attribute_Parameter> parameters,
                        Source_Info const& source_info)
      : Node(source_info, Node_Kind::attribute), identifier(identifier),
        parameters(parameters)
    {
    }
  };

  struct Variable: public Node {
    // Allowed on variables which are declarations (globals).
    Attr_List attributes;
    Type* type;
    Identifier identifier;
    // nullptr when Variable does not have an initializer.
    Expr* initializer;

    constexpr Variable(Attr_List attributes, Type* type, Identifier identifier,
                       Expr* initializer, Source_Info const& source_info)
      : Node(source_info, Node_Kind::variable), attributes(attributes),
        type(type), identifier(identifier), initializer(initializer)
    {
    }
  };

  struct Struct_Field {
    Attr_List attributes;
    Identifier identifier;
    Type* type;
    // nullptr when the field does not have an initializer.
    Expr* initializer;

    constexpr Struct_Field(Attr_List attributes, Identifier identifier,
                           Type* type, Expr* initializer)
      : attributes(attributes), identifier(identifier), type(type),
        initializer(initializer)
    {
    }
  };

  struct Decl_Struct: public Node {
    Attr_List attributes;
    Identifier identifier;
    Struct_Field_List fields;

    constexpr Decl_Struct(Attr_List attributes, Identifier identifier,
                          Struct_Field_List fields,
                          Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_struct), attributes(attributes),
        identifier(identifier), fields(fields)
    {
    }
  };

  struct Buffer_Field {
    Attr_List attributes;
    Identifier identifier;
    Type* type;

    constexpr Buffer_Field(Attr_List attributes, Identifier identifier,
                           Type* type)
      : attributes(attributes), identifier(identifier), type(type)
    {
    }
  };

  struct Decl_Buffer: public Node {
    Attr_List attributes;
    Identifier pass;
    Identifier identifier;
    Buffer_Field_List fields;

    constexpr Decl_Buffer(Attr_List attributes, Identifier pass,
                          Identifier identifier, Buffer_Field_List fields,
                          Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_buffer), attributes(attributes),
        pass(pass), identifier(identifier), fields(fields)
    {
    }
  };

  [[nodiscard]] bool is_buffer(Decl_Buffer const* buffer);
  [[nodiscard]] bool is_uniform(Decl_Buffer const* buffer);
  [[nodiscard]] bool is_push_constant(Decl_Buffer const* buffer);

  struct Fn_Parameter: public Node {
    Attr_List attributes;
    Identifier identifier;
    Type* type;
    // Empty when the parameter has no source.
    // "in" when the parameter is an input parameter.
    // "out" when the parameter is an output parameter.
    // "images" when the parameter is an image parameter.
    Identifier source;
    Decl_Buffer* buffer = nullptr;

    constexpr Fn_Parameter(Attr_List attributes, Identifier identifier,
                           Type* type, Identifier source,
                           Source_Info const& source_info)
      : Node(source_info, Node_Kind::fn_parameter), attributes(attributes),
        identifier(identifier), type(type), source(source)
    {
    }
  };

  [[nodiscard]] bool is_sourced_parameter(Fn_Parameter const& parameter);
  [[nodiscard]] bool is_input_parameter(Fn_Parameter const& parameter);
  [[nodiscard]] bool is_output_parameter(Fn_Parameter const& parameter);
  [[nodiscard]] bool is_image_parameter(Fn_Parameter const& parameter);

  struct Decl_Function: public Node {
    Attr_List attributes;
    Identifier identifier;
    Fn_Parameter_List parameters;
    Type* return_type;
    Node_List body;
    // Whether the function is a builtin function.
    bool builtin;
    Overload_Group* overload_group = nullptr;

    constexpr Decl_Function(Attr_List attributes, Identifier identifier,
                            Fn_Parameter_List parameters, Type* return_type,
                            Node_List body, bool builtin,
                            Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_function), attributes(attributes),
        identifier(identifier), parameters(parameters),
        return_type(return_type), body(body), builtin(builtin)
    {
    }
  };

  // Overload_Group
  // A semantic node representing an overload group of functions.
  //
  struct Overload_Group {
    anton::String identifier;
    Array<Decl_Function*> overloads;

    Overload_Group(Allocator* allocator, anton::String_View identifier)
      : identifier(identifier, allocator), overloads(allocator)
    {
    }
  };

  struct Decl_Stage_Function: public Node {
    Attr_List attributes;
    Identifier pass;
    With_Source<Stage_Kind> stage;
    Fn_Parameter_List parameters;
    Node_List body;

    constexpr Decl_Stage_Function(Attr_List attributes, Identifier pass,
                                  With_Source<Stage_Kind> stage,
                                  Fn_Parameter_List parameters, Node_List body,
                                  Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_stage_function),
        attributes(attributes), pass(pass), stage(stage),
        parameters(parameters), body(body)
    {
    }
  };

  struct Expr: public Node {
    using Node::Node;

    // The type of the result of the expression.
    Type const* evaluated_type = nullptr;
  };

  struct Expr_If: public Expr {
    Expr* condition;
    Expr* then_branch;
    Expr* else_branch;

    constexpr Expr_If(Expr* condition, Expr* then_branch, Expr* else_branch,
                      Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_if), condition(condition),
        then_branch(then_branch), else_branch(else_branch)
    {
    }
  };

  struct Expr_Identifier: public Expr {
    anton::String_View value;
    Node* definition = nullptr;

    constexpr Expr_Identifier(anton::String_View value,
                              Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_identifier), value(value)
    {
    }
  };

  struct Initializer: public Node {
    using Node::Node;
  };

  struct Field_Initializer: public Initializer {
    Identifier identifier;
    Expr* expression;

    constexpr Field_Initializer(Identifier identifier, Expr* expression,
                                Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::field_initializer),
        identifier(identifier), expression(expression)
    {
    }
  };

  struct Index_Initializer: public Initializer {
    Lt_Integer* index;
    Expr* expression;

    constexpr Index_Initializer(Lt_Integer* index, Expr* expression,
                                Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::index_initializer), index(index),
        expression(expression)
    {
    }
  };

  struct Basic_Initializer: public Initializer {
    Expr* expression;

    constexpr Basic_Initializer(Expr* expression,
                                Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::basic_initializer),
        expression(expression)
    {
    }
  };

  struct Expr_Init: public Expr {
    Type* type;
    Initializer_List initializers;

    constexpr Expr_Init(Type* type, Initializer_List initializers,
                        Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_init), type(type),
        initializers(initializers)
    {
    }
  };

  struct Expr_Call: public Expr {
    Identifier identifier;
    Expr_List arguments;
    Overload_Group* overload_group = nullptr;
    Decl_Function* function = nullptr;

    constexpr Expr_Call(Identifier identifier, Expr_List arguments,
                        Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_call), identifier(identifier),
        arguments(arguments)
    {
    }

    [[nodiscard]] bool is_unary() const
    {
      return arguments.size() == 1;
    }

    [[nodiscard]] bool is_binary() const
    {
      return arguments.size() == 2;
    }
  };

  struct Expr_Field: public Expr {
    Expr* base;
    Identifier field;

    constexpr Expr_Field(Expr* base, Identifier field,
                         Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_field), base(base), field(field)
    {
    }
  };

  // vector_swizzle_char_to_index
  // Convert swizzle character to index. Allowed swizzle characters are
  //   x, y, z, w, r, g, b, a, s, t, u, v
  // Other chars return -1, but are considered invalid.
  //
  [[nodiscard]] i32 vector_swizzle_char_to_index(char8 c);

  struct Expr_Index: public Expr {
    Expr* base;
    Expr* index;

    constexpr Expr_Index(Expr* base, Expr* index,
                         Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_index), base(base), index(index)
    {
    }
  };

  struct Expr_Reinterpret: public Expr {
    Type* target_type;
    Expr* source;
    Expr* index;

    constexpr Expr_Reinterpret(Type* target_type, Expr* source, Expr* index,
                               Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_reinterpret),
        target_type(target_type), source(source), index(index)
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

  enum struct Lt_Integer_Kind : u8 { i32, u32 };

  namespace detail {
    template<Lt_Integer_Kind Value>
    struct Lt_Integer_Tag {
      explicit constexpr Lt_Integer_Tag() = default;
    };
  } // namespace detail

  constexpr detail::Lt_Integer_Tag<Lt_Integer_Kind::i32> lt_integer_i32;
  constexpr detail::Lt_Integer_Tag<Lt_Integer_Kind::u32> lt_integer_u32;

  struct Lt_Integer: public Expr {
    union {
      i32 i32_value;
      u32 u32_value;
    };
    Lt_Integer_Kind kind;

    constexpr Lt_Integer(detail::Lt_Integer_Tag<Lt_Integer_Kind::i32>,
                         i32 value, Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_integer), i32_value(value),
        kind(Lt_Integer_Kind::i32)
    {
    }

    constexpr Lt_Integer(detail::Lt_Integer_Tag<Lt_Integer_Kind::u32>,
                         u32 value, Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_integer), u32_value(value),
        kind(Lt_Integer_Kind::u32)
    {
    }
  };

  u32 get_lt_integer_value_as_u32(Lt_Integer const& integer);

  // compare_integer_literals
  // Three-way compare integer literals' value regardless of their base. The
  // literal values must be trimmed to contain only plus or minus and digits.
  //
  // Returns:
  // Ordering of the numeric values of the literals.
  //
  [[nodiscard]] anton::Strong_Ordering
  compare_integer_literals(Lt_Integer const& lhs, Lt_Integer const& rhs);

  enum struct Lt_Float_Kind : u8 { f32, f64 };

  namespace detail {
    template<Lt_Float_Kind Value>
    struct Lt_Float_Tag {
      explicit constexpr Lt_Float_Tag() = default;
    };
  } // namespace detail

  constexpr detail::Lt_Float_Tag<Lt_Float_Kind::f32> lt_float_f32;
  constexpr detail::Lt_Float_Tag<Lt_Float_Kind::f64> lt_float_f64;

  struct Lt_Float: public Expr {
    union {
      f32 f32_value;
      f64 f64_value;
    };
    Lt_Float_Kind kind;

    constexpr Lt_Float(detail::Lt_Float_Tag<Lt_Float_Kind::f32>, f32 value,
                       Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_float), f32_value(value),
        kind(Lt_Float_Kind::f32)
    {
    }

    constexpr Lt_Float(detail::Lt_Float_Tag<Lt_Float_Kind::f64>, f64 value,
                       Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_float), f32_value(value),
        kind(Lt_Float_Kind::f64)
    {
    }
  };

  struct Switch_Arm {
    Expr_List labels;
    Node_List statements;
    bool has_default = false;

    constexpr Switch_Arm(Expr_List labels, Node_List statements)
      : labels(labels), statements(statements)
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

  enum struct Assignment_Kind {
    e_assign,
    e_add,
    e_sub,
    e_mul,
    e_div,
    e_mod,
    e_and,
    e_or,
    e_xor,
    e_shl,
    e_shr,
  };

  struct Stmt_Assignment: public Node {
    Expr* lhs;
    Expr* rhs;
    Assignment_Kind kind;

    constexpr Stmt_Assignment(Assignment_Kind kind, Expr* lhs, Expr* rhs,
                              Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_assignment), lhs(lhs), rhs(rhs),
        kind(kind)
    {
    }
  };

  [[nodiscard]] bool
  is_assignment_arithmetic(Stmt_Assignment const* assignment);

  struct Stmt_If: public Node {
    Expr* condition;
    Node_List then_branch;
    Node_List else_branch;

    constexpr Stmt_If(Expr* condition, Node_List then_branch,
                      Node_List else_branch, Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_if), condition(condition),
        then_branch(then_branch), else_branch(else_branch)
    {
    }
  };

  struct Stmt_Switch: public Node {
    Expr* expression;
    anton::Slice<Switch_Arm* const> arms;

    constexpr Stmt_Switch(Expr* const expression,
                          anton::Slice<Switch_Arm* const> arms,
                          Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_switch), expression(expression),
        arms(arms)
    {
    }
  };

  struct Stmt_While: public Node {
    Expr* condition;
    Node_List statements;

    constexpr Stmt_While(Expr* condition, Node_List statements,
                         Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_while), condition(condition),
        statements(statements)
    {
    }
  };

  struct Stmt_Do_While: public Node {
    Expr* condition;
    Node_List statements;

    constexpr Stmt_Do_While(Expr* condition, Node_List statements,
                            Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_do_while), condition(condition),
        statements(statements)
    {
    }
  };

  struct Stmt_For: public Node {
    // nullptr if the loop does not have a condition.
    Expr* condition;
    Variable_List declarations;
    Expr_List actions;
    Node_List statements;

    constexpr Stmt_For(Expr* condition, Variable_List declarations,
                       Expr_List actions, Node_List statements,
                       Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_for), condition(condition),
        declarations(declarations), actions(actions), statements(statements)
    {
    }
  };

  struct Stmt_Return: public Node {
    // nullptr if expression not present.
    Expr* expression;

    constexpr Stmt_Return(Expr* expression, Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_return), expression(expression)
    {
    }
  };

  struct Stmt_Break: public Node {
    constexpr Stmt_Break(Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_break)
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
    // Never nullptr.
    Expr* expression;

    constexpr Stmt_Expression(Expr* expression, Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_expression), expression(expression)
    {
    }
  };
} // namespace vush::ast
