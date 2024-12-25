#pragma once

#include <anton/ilist.hpp>
#include <anton/optional.hpp>
#include <anton/ordering.hpp>
#include <anton/string.hpp>

#include <vush_ast/fwd.hpp>
#include <vush_ast/types.hpp>
#include <vush_core/source_info.hpp>
#include <vush_core/types.hpp>

namespace vush {
  struct Context;
}

namespace vush::ast {
  enum struct Node_Kind : u8 {
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
  struct Node: public anton::IList_DNode {
    Source_Info source_info;
    Node_Kind node_kind;

    Node(Source_Info const& source_info, Node_Kind node_kind)
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

  struct Attribute_Parameter: public anton::IList_DNode {
    // empty if the parameter is positional.
    Identifier key;
    Expr* value;

    Attribute_Parameter(Identifier key, Expr* value): key(key), value(value) {}
  };

  struct Attribute: public Node {
    Identifier identifier;
    anton::IList<Attribute_Parameter> parameters;

    Attribute(Identifier identifier,
              anton::IList<Attribute_Parameter> parameters,
              Source_Info const& source_info)
      : Node(source_info, Node_Kind::attribute), identifier(identifier),
        parameters(ANTON_MOV(parameters))
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

    Variable(Attr_List&& attributes, Type* type, Identifier identifier,
             Expr* initializer, Source_Info const& source_info)
      : Node(source_info, Node_Kind::variable),
        attributes(ANTON_MOV(attributes)), type(type), identifier(identifier),
        initializer(initializer)
    {
    }
  };

  struct Struct_Field: public anton::IList_DNode {
    Attr_List attributes;
    Identifier identifier;
    Type* type;
    // nullptr when the field does not have an initializer.
    Expr* initializer;

    Struct_Field(Attr_List&& attributes, Identifier identifier, Type* type,
                 Expr* initializer)
      : attributes(ANTON_MOV(attributes)), identifier(identifier), type(type),
        initializer(initializer)
    {
    }
  };

  struct Decl_Struct: public Node {
    Attr_List attributes;
    Identifier identifier;
    Struct_Field_List fields;

    Decl_Struct(Attr_List&& attributes, Identifier identifier,
                Struct_Field_List&& fields, Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_struct),
        attributes(ANTON_MOV(attributes)), identifier(identifier),
        fields(ANTON_MOV(fields))
    {
    }
  };

  struct Buffer_Field: public anton::IList_DNode {
    Attr_List attributes;
    Identifier identifier;
    Type* type;

    Buffer_Field(Attr_List&& attributes, Identifier identifier, Type* type)
      : attributes(ANTON_MOV(attributes)), identifier(identifier), type(type)
    {
    }
  };

  struct Decl_Buffer: public Node {
    Attr_List attributes;
    Identifier pass;
    Identifier identifier;
    Buffer_Field_List fields;

    Decl_Buffer(Attr_List&& attributes, Identifier pass, Identifier identifier,
                Buffer_Field_List&& fields, Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_buffer),
        attributes(ANTON_MOV(attributes)), pass(pass), identifier(identifier),
        fields(ANTON_MOV(fields))
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

    Fn_Parameter(Attr_List&& attributes, Identifier identifier, Type* type,
                 Identifier source, Source_Info const& source_info)
      : Node(source_info, Node_Kind::fn_parameter),
        attributes(ANTON_MOV(attributes)), identifier(identifier), type(type),
        source(source)
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
    Stmt_List body;
    // Whether the function is a builtin function.
    bool builtin;
    Overload_Group* overload_group = nullptr;

    Decl_Function(Attr_List&& attributes, Identifier identifier,
                  Fn_Parameter_List&& parameters, Type* return_type,
                  Stmt_List&& body, bool builtin,
                  Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_function),
        attributes(ANTON_MOV(attributes)), identifier(identifier),
        parameters(ANTON_MOV(parameters)), return_type(return_type),
        body(ANTON_MOV(body)), builtin(builtin)
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
    Stmt_List body;

    Decl_Stage_Function(Attr_List&& attributes, Identifier pass,
                        With_Source<Stage_Kind> stage,
                        Fn_Parameter_List&& parameters, Stmt_List&& body,
                        Source_Info const& source_info)
      : Node(source_info, Node_Kind::decl_stage_function),
        attributes(ANTON_MOV(attributes)), pass(pass), stage(stage),
        parameters(ANTON_MOV(parameters)), body(ANTON_MOV(body))
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

    Expr_If(Expr* condition, Expr* then_branch, Expr* else_branch,
            Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_if), condition(condition),
        then_branch(then_branch), else_branch(else_branch)
    {
    }
  };

  struct Expr_Identifier: public Expr {
    anton::String_View value;
    Node* definition = nullptr;

    Expr_Identifier(anton::String_View value, Source_Info const& source_info)
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

    Field_Initializer(Identifier identifier, Expr* expression,
                      Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::field_initializer),
        identifier(identifier), expression(expression)
    {
    }
  };

  struct Index_Initializer: public Initializer {
    Lt_Integer* index;
    Expr* expression;

    Index_Initializer(Lt_Integer* index, Expr* expression,
                      Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::index_initializer), index(index),
        expression(expression)
    {
    }
  };

  struct Basic_Initializer: public Initializer {
    Expr* expression;

    Basic_Initializer(Expr* expression, Source_Info const& source_info)
      : Initializer(source_info, Node_Kind::basic_initializer),
        expression(expression)
    {
    }
  };

  struct Expr_Init: public Expr {
    Type* type;
    Initializer_List initializers;

    Expr_Init(Type* type, Initializer_List&& initializers,
              Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_init), type(type),
        initializers(ANTON_MOV(initializers))
    {
    }
  };

  struct Expr_Call: public Expr {
    Identifier identifier;
    Expr_List arguments;
    Overload_Group* overload_group = nullptr;
    Decl_Function* function = nullptr;

    Expr_Call(Identifier identifier, Expr_List&& arguments,
              Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_call), identifier(identifier),
        arguments(ANTON_MOV(arguments))
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

    Expr_Field(Expr* base, Identifier field, Source_Info const& source_info)
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

    Expr_Index(Expr* base, Expr* index, Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_index), base(base), index(index)
    {
    }
  };

  struct Expr_Reinterpret: public Expr {
    Type* target_type;
    Expr* source;
    Expr* index;

    Expr_Reinterpret(Type* target_type, Expr* source, Expr* index,
                     Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_reinterpret),
        target_type(target_type), source(source), index(index)
    {
    }
  };

  struct Expr_Default: public Expr {
    Expr_Default(Source_Info const& source_info)
      : Expr(source_info, Node_Kind::expr_default)
    {
    }
  };

  struct Lt_Bool: public Expr {
    bool value;

    Lt_Bool(bool value, Source_Info const& source_info)
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

    Lt_Integer(detail::Lt_Integer_Tag<Lt_Integer_Kind::i32>, i32 value,
               Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_integer), i32_value(value),
        kind(Lt_Integer_Kind::i32)
    {
    }

    Lt_Integer(detail::Lt_Integer_Tag<Lt_Integer_Kind::u32>, u32 value,
               Source_Info const& source_info)
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

    Lt_Float(detail::Lt_Float_Tag<Lt_Float_Kind::f32>, f32 value,
             Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_float), f32_value(value),
        kind(Lt_Float_Kind::f32)
    {
    }

    Lt_Float(detail::Lt_Float_Tag<Lt_Float_Kind::f64>, f64 value,
             Source_Info const& source_info)
      : Expr(source_info, Node_Kind::lt_float), f32_value(value),
        kind(Lt_Float_Kind::f64)
    {
    }
  };

  struct Switch_Arm: public anton::IList_DNode {
    Expr_List labels;
    Stmt_List statements;
    bool has_default = false;

    Switch_Arm(Expr_List&& labels, Stmt_List&& statements)
      : labels(ANTON_MOV(labels)), statements(ANTON_MOV(statements))
    {
    }
  };

  struct Stmt: public Node {
    using Node::Node;
  };

  struct Stmt_Block: public Stmt {
    anton::IList<Stmt> statements;

    Stmt_Block(anton::IList<Stmt>&& statements, Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_block),
        statements(ANTON_MOV(statements))
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

  struct Stmt_Assignment: public Stmt {
    Expr* lhs;
    Expr* rhs;
    Assignment_Kind kind;

    Stmt_Assignment(Assignment_Kind kind, Expr* lhs, Expr* rhs,
                    Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_assignment), lhs(lhs), rhs(rhs),
        kind(kind)
    {
    }
  };

  [[nodiscard]] bool
  is_assignment_arithmetic(Stmt_Assignment const* assignment);

  struct Stmt_If: public Stmt {
    Expr* condition;
    Stmt_List then_branch;
    Stmt_List else_branch;

    Stmt_If(Expr* condition, Stmt_List&& then_branch, Stmt_List&& else_branch,
            Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_if), condition(condition),
        then_branch(ANTON_MOV(then_branch)), else_branch(ANTON_MOV(else_branch))
    {
    }
  };

  struct Stmt_Switch: public Stmt {
    Expr* expression;
    anton::IList<Switch_Arm> arms;

    Stmt_Switch(Expr* const expression, anton::IList<Switch_Arm>&& arms,
                Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_switch), expression(expression),
        arms(ANTON_MOV(arms))
    {
    }
  };

  struct Stmt_While: public Stmt {
    Expr* condition;
    Stmt_List statements;

    Stmt_While(Expr* condition, Stmt_List&& statements,
               Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_while), condition(condition),
        statements(ANTON_MOV(statements))
    {
    }
  };

  struct Stmt_Do_While: public Stmt {
    Expr* condition;
    Stmt_List statements;

    Stmt_Do_While(Expr* condition, Stmt_List&& statements,
                  Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_do_while), condition(condition),
        statements(ANTON_MOV(statements))
    {
    }
  };

  struct Stmt_For: public Stmt {
    // nullptr if the loop does not have a condition.
    Expr* condition;
    Variable_List declarations;
    Expr_List actions;
    Stmt_List statements;

    Stmt_For(Expr* condition, Variable_List&& declarations, Expr_List&& actions,
             Stmt_List&& statements, Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_for), condition(condition),
        declarations(ANTON_MOV(declarations)), actions(ANTON_MOV(actions)),
        statements(ANTON_MOV(statements))
    {
    }
  };

  struct Stmt_Return: public Stmt {
    // nullptr if expression not present.
    Expr* expression;

    Stmt_Return(Expr* expression, Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_return), expression(expression)
    {
    }
  };

  struct Stmt_Break: public Stmt {
    Stmt_Break(Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_break)
    {
    }
  };

  struct Stmt_Continue: public Stmt {
    Stmt_Continue(Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_continue)
    {
    }
  };

  struct Stmt_Discard: public Stmt {
    Stmt_Discard(Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_discard)
    {
    }
  };

  struct Stmt_Expression: public Stmt {
    // Never nullptr.
    Expr* expression;

    Stmt_Expression(Expr* expression, Source_Info const& source_info)
      : Stmt(source_info, Node_Kind::stmt_expression), expression(expression)
    {
    }
  };
} // namespace vush::ast
