#pragma once

#include <anton/optional.hpp>
#include <anton/ordering.hpp>
#include <anton/string.hpp>

#include <vush_ast/ast_fwd.hpp>
#include <vush_core/either.hpp>
#include <vush_core/source_info.hpp>
#include <vush_core/types.hpp>

namespace vush {
  struct Context;
}

namespace vush::ast {
  enum struct Node_Kind : u8 {
    type_builtin,
    type_struct,
    type_array,

    attribute,
    variable,

    fn_parameter,
    struct_field,

    decl_struct,
    decl_function,
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
  [[nodiscard]] bool is_vector(Type const& type);
  [[nodiscard]] bool is_vector2(Type const& type);
  [[nodiscard]] bool is_vector3(Type const& type);
  [[nodiscard]] bool is_vector4(Type const& type);
  [[nodiscard]] bool is_bool_vector(Type const& type);
  [[nodiscard]] bool is_i32_vector(Type const& type);
  [[nodiscard]] bool is_u32_vector(Type const& type);
  [[nodiscard]] bool is_f32_vector(Type const& type);
  [[nodiscard]] bool is_f64_vector(Type const& type);
  [[nodiscard]] bool is_matrix(Type const& type);
  [[nodiscard]] bool is_f32_matrix(Type const& type);
  [[nodiscard]] bool is_f64_matrix(Type const& type);
  [[nodiscard]] bool is_opaque_type(Type const& type);
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

  [[nodiscard]] anton::Optional<Type_Builtin_Kind>
  enumify_builtin_type_kind(anton::String_View type);

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

  struct Type_Struct: public Type {
    // The identifier value, that is the name of the type.
    anton::String_View value;
    Decl_Struct* definition = nullptr;

    constexpr Type_Struct(anton::String_View value, Source_Info const& source_info)
      : Type(source_info, Node_Kind::type_struct), value(value)
    {
    }

    constexpr Type_Struct(Qualifiers qualifiers, anton::String_View value,
                          Source_Info const& source_info)
      : Type(qualifiers, source_info, Node_Kind::type_struct), value(value)
    {
    }
  };

  struct Type_Array: public Type {
    Type* base;
    // nullptr when the array is unsized.
    Lt_Integer* size;

    constexpr Type_Array(Type* base, Lt_Integer* size, Source_Info const& source_info)
      : Type(source_info, Node_Kind::type_array), base(base), size(size)
    {
    }

    constexpr Type_Array(Qualifiers qualifiers, Type* base, Lt_Integer* size,
                         Source_Info const& source_info)
      : Type(qualifiers, source_info, Node_Kind::type_array), base(base), size(size)
    {
    }
  };

  [[nodiscard]] bool is_type(Node const& node);

  struct Attribute_Parameter {
    // empty if the parameters positional.
    Identifier key;
    Expr* value;
  };

  struct Attribute: public Node {
    Identifier identifier;
    anton::Slice<Attribute_Parameter> parameters;

    constexpr Attribute(Identifier identifier, anton::Slice<Attribute_Parameter> parameters,
                        Source_Info const& source_info)
      : Node(source_info, Node_Kind::attribute), identifier(identifier), parameters(parameters)
    {
    }
  };

  struct Variable: public Node {
    Type* type;
    Identifier identifier;
    // nullptr when Variable does not have an initializer.
    Expr* initializer;

    constexpr Variable(Type* type, Identifier identifier, Expr* initializer,
                       Source_Info const& source_info)
      : Node(source_info, Node_Kind::variable), type(type), identifier(identifier),
        initializer(initializer)
    {
    }
  };

  struct Struct_Field: public Node {
    Attr_List attributes;
    Identifier identifier;
    Type* type;
    // nullptr when the field does not have an initializer.
    Expr* initializer;

    constexpr Struct_Field(Attr_List attributes, Identifier identifier, Type* type,
                           Expr* initializer, Source_Info const& source_info)
      : Node(source_info, Node_Kind::struct_field), attributes(attributes), identifier(identifier),
        type(type), initializer(initializer)
    {
    }
  };

  struct Decl_Struct: public Node {
    Identifier identifier;
    Field_List fields;

    constexpr Decl_Struct(Identifier identifier, Field_List fields, Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_struct), identifier(identifier), fields(fields)
    {
    }
  };

  struct Fn_Parameter: public Node {
    Identifier identifier;
    Type* type;
    // Empty when the parameter has no source.
    // "in" when the parameter is a vertex input parameter.
    Identifier source;

    constexpr Fn_Parameter(Identifier identifier, Type* type, Identifier source,
                           Source_Info const& source_info)
      : Node(source_info, Node_Kind::fn_parameter), identifier(identifier), type(type),
        source(source)
    {
    }
  };

  [[nodiscard]] bool is_sourced_parameter(Fn_Parameter const& parameter);
  [[nodiscard]] bool is_vertex_input_parameter(Fn_Parameter const& parameter);

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
                            Fn_Parameter_List parameters, Type* return_type, Node_List body,
                            bool builtin, Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_function), attributes(attributes), identifier(identifier),
        parameters(parameters), return_type(return_type), body(body), builtin(builtin)
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
    Type* return_type;
    Node_List body;

    constexpr Decl_Stage_Function(Attr_List attributes, Identifier pass,
                                  With_Source<Stage_Kind> stage, Fn_Parameter_List parameters,
                                  Type* return_type, Node_List body, Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_stage_function), attributes(attributes), pass(pass),
        stage(stage), parameters(parameters), return_type(return_type), body(body)
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
      : Expr(source_info, Node_Kind::expr_if), condition(condition), then_branch(then_branch),
        else_branch(else_branch)
    {
    }
  };

  struct Expr_Identifier: public Expr {
    anton::String_View value;
    Node* definition = nullptr;

    constexpr Expr_Identifier(anton::String_View value, Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_identifier), value(value)
    {
    }
  };

  struct Expr_Assignment: public Expr {
    Expr* lhs;
    Expr* rhs;

    constexpr Expr_Assignment(Expr* lhs, Expr* rhs, Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_assignment), lhs(lhs), rhs(rhs)
    {
    }
  };

  struct Initializer: public Node {
    using Node::Node;
  };

  struct Named_Initializer: public Initializer {
    Identifier identifier;
    Expr* expression;

    constexpr Named_Initializer(Identifier identifier, Expr* expression,
                                Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::named_initializer), identifier(identifier),
        expression(expression)
    {
    }
  };

  struct Indexed_Initializer: public Initializer {
    Lt_Integer* index;
    Expr* expression;

    constexpr Indexed_Initializer(Lt_Integer* index, Expr* expression,
                                  Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::indexed_initializer), index(index),
        expression(expression)
    {
    }
  };

  struct Basic_Initializer: public Initializer {
    Expr* expression;

    constexpr Basic_Initializer(Expr* expression, Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::basic_initializer), expression(expression)
    {
    }
  };

  struct Expr_Init: public Expr {
    Type* type;
    Initializer_List initializers;

    constexpr Expr_Init(Type* type, Initializer_List initializers, Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_init), type(type), initializers(initializers)
    {
    }
  };

  struct Expr_Call: public Expr {
    Identifier identifier;
    Expr_List arguments;
    Overload_Group* overload_group = nullptr;
    Decl_Function* function = nullptr;

    constexpr Expr_Call(Identifier identifier, Expr_List arguments, Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_call), identifier(identifier), arguments(arguments)
    {
    }
  };

  struct Expr_Field: public Expr {
    Expr* base;
    Identifier field;

    constexpr Expr_Field(Expr* base, Identifier field, Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_field), base(base), field(field)
    {
    }
  };

  struct Expr_Index: public Expr {
    Expr* base;
    Expr* index;

    constexpr Expr_Index(Expr* base, Expr* index, Source_Info const& source_info)
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

    constexpr Lt_Integer(detail::Lt_Integer_Tag<Lt_Integer_Kind::i32>, i32 value,
                         Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_integer), i32_value(value), kind(Lt_Integer_Kind::i32)
    {
    }

    constexpr Lt_Integer(detail::Lt_Integer_Tag<Lt_Integer_Kind::u32>, u32 value,
                         Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_integer), u32_value(value), kind(Lt_Integer_Kind::u32)
    {
    }
  };

  // compare_integer_literals
  // Three-way compare integer literals' value regardless of their base. The literal values must be
  // trimmed to contain only plus or minus and digits.
  //
  // Returns:
  // Ordering of the numeric values of the literals.
  //
  [[nodiscard]] anton::Strong_Ordering compare_integer_literals(Lt_Integer const& lhs,
                                                                Lt_Integer const& rhs);

  enum struct Lt_Float_Kind { f32, f64 };

  struct Lt_Float: public Expr {
    anton::String_View value;
    Lt_Float_Kind kind;

    constexpr Lt_Float(anton::String_View value, Lt_Float_Kind kind, Source_Info const& source_info)
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
    Expr* condition;
    Node_List then_branch;
    Node_List else_branch;

    constexpr Stmt_If(Expr* condition, Node_List then_branch, Node_List else_branch,
                      Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_if), condition(condition), then_branch(then_branch),
        else_branch(else_branch)
    {
    }
  };

  struct Stmt_Switch: public Node {
    Expr* expression;
    anton::Slice<Switch_Arm* const> arms;

    constexpr Stmt_Switch(Expr* const expression, anton::Slice<Switch_Arm* const> arms,
                          Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_switch), expression(expression), arms(arms)
    {
    }
  };

  struct Stmt_Loop: public Node {
    // nullptr if the loop does not have a condition.
    Expr* condition;
    // continue statements are not allowed within the continuation block.
    Node_List continuation;
    Node_List statements;

    constexpr Stmt_Loop(Expr* condition, Node_List continuation, Node_List statements,
                        Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_loop), condition(condition), continuation(continuation),
        statements(statements)
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
    Expr* expression;

    constexpr Stmt_Expression(Expr* expression, Source_Info const& source_info)
      : Node(source_info, Node_Kind::stmt_expression), expression(expression)
    {
    }
  };
} // namespace vush::ast
