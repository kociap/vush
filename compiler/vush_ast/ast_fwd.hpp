#pragma once

#include <anton/array.hpp>
#include <anton/slice.hpp>

#include <vush_core/types.hpp>

namespace vush::ast {
  enum struct Node_Kind : u8;

  struct Node;

  struct Identifier;

  enum struct Type_Builtin_Kind : u8;

  struct Type;
  struct Type_Builtin;
  struct Type_Struct;
  struct Type_Array;

  struct Attribute;
  struct Variable;

  struct Fn_Parameter;
  struct Struct_Member;

  struct Decl_Struct;
  struct Decl_Function;
  struct Decl_Overloaded_Function;
  struct Decl_Stage_Function;

  struct Initializer;
  struct Named_Initializer;
  struct Indexed_Initializer;
  struct Basic_Initializer;

  struct Expr;
  struct Expr_If;
  struct Expr_Identifier;
  struct Expr_Assignment;
  struct Expr_Init;
  struct Expr_Call;
  struct Expr_Field;
  struct Expr_Index;
  struct Expr_Parentheses;
  struct Expr_Reinterpret;
  struct Expr_Default;
  struct Lt_Bool;
  struct Lt_Integer;
  struct Lt_Float;

  struct Switch_Arm;

  struct Stmt_Block;
  struct Stmt_If;
  struct Stmt_Switch;
  struct Stmt_Loop;
  struct Stmt_Return;
  struct Stmt_Break;
  struct Stmt_Continue;
  struct Stmt_Discard;
  struct Stmt_Expression;

  using Node_List = anton::Slice<Node const* const>;
  using Attr_List = anton::Slice<Attribute const* const>;
  using Expr_List = anton::Slice<Expr const* const>;
  using Fn_Parameter_List = anton::Slice<Fn_Parameter const* const>;
  using Initializer_List = anton::Slice<Initializer const* const>;
  using Member_List = anton::Slice<Struct_Member const* const>;
} // namespace vush::ast