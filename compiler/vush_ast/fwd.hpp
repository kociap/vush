#pragma once

#include <anton/ilist.hpp>

#include <vush_core/source_info.hpp>
#include <vush_core/types.hpp>

namespace vush::ast {
  template<typename T>
  struct With_Source {
    T value;
    Source_Info source_info;
  };

  using Identifier = With_Source<anton::String_View>;

  enum struct Node_Kind : u8;

  struct Node;

  enum struct Type_Builtin_Kind : u8;

  struct Type;
  struct Type_Builtin;
  struct Type_Struct;
  struct Type_Array;

  struct Attribute;
  struct Variable;

  struct Struct_Field;
  struct Decl_Struct;
  struct Buffer_Field;
  struct Decl_Buffer;
  struct Fn_Parameter;
  struct Decl_Function;
  struct Decl_Stage_Function;
  struct Overload_Group;

  struct Initializer;
  struct Field_Initializer;
  struct Index_Initializer;
  struct Basic_Initializer;

  struct Expr;
  struct Expr_If;
  struct Expr_Identifier;
  struct Expr_Init;
  struct Expr_Call;
  struct Expr_Field;
  struct Expr_Index;
  struct Expr_Reinterpret;
  struct Expr_Default;
  struct Lt_Bool;
  struct Lt_Integer;
  struct Lt_Float;

  struct Switch_Arm;

  struct Stmt;
  struct Stmt_Block;
  struct Stmt_Assignment;
  struct Stmt_If;
  struct Stmt_Switch;
  struct Stmt_While;
  struct Stmt_Do_While;
  struct Stmt_For;
  struct Stmt_Return;
  struct Stmt_Break;
  struct Stmt_Continue;
  struct Stmt_Discard;
  struct Stmt_Expression;

  using Node_List = anton::IList<Node>;
  using Stmt_List = anton::IList<Stmt>;
  using Attr_List = anton::IList<Attribute>;
  using Expr_List = anton::IList<Expr>;
  using Fn_Parameter_List = anton::IList<Fn_Parameter>;
  using Initializer_List = anton::IList<Initializer>;
  using Struct_Field_List = anton::IList<Struct_Field>;
  using Buffer_Field_List = anton::IList<Buffer_Field>;
  using Variable_List = anton::IList<Variable>;
} // namespace vush::ast
