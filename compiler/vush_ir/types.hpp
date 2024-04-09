#pragma once

#include <anton/string.hpp>

#include <vush_core/types.hpp>

namespace vush::ir {
  enum struct Type_Kind {
    e_void,
    e_bool,
    e_int8,
    e_int16,
    e_int32,
    e_fp16,
    e_fp32,
    e_fp64,
    e_ptr,
    e_composite,
    e_array,
    e_vec,
    e_mat,
  };

  struct Type {
    Type_Kind kind;
  };

  [[nodiscard]] bool compare_types_equal(Type const& lhs, Type const& rhs);

  [[nodiscard]] Type* get_type_void();
  [[nodiscard]] Type* get_type_bool();
  [[nodiscard]] Type* get_type_int8();
  [[nodiscard]] Type* get_type_int16();
  [[nodiscard]] Type* get_type_int32();
  [[nodiscard]] Type* get_type_fp16();
  [[nodiscard]] Type* get_type_fp32();
  [[nodiscard]] Type* get_type_fp64();
  [[nodiscard]] Type* get_type_fp64();
  [[nodiscard]] Type* get_type_ptr();

  [[nodiscard]] bool is_int_type(Type const& type);
  [[nodiscard]] bool is_int_based_type(Type const& type);
  [[nodiscard]] bool is_fp_type(Type const& type);
  [[nodiscard]] bool is_fp_based_type(Type const& type);

  struct Type_Array: public Type {
    Type* element_type;
    i64 size;

    Type_Array(Type* element_type, i64 size)
      : Type{Type_Kind::e_array}, element_type(element_type), size(size)
    {
    }
  };

  struct Type_Composite: public Type {
    anton::Array<Type*> elements;
    anton::String identifier;

    Type_Composite(): Type{Type_Kind::e_composite} {}
  };

  struct Type_Vec: public Type {
    Type_Kind element_kind;
    i64 rows;

    Type_Vec(Type_Kind element_kind, i64 rows)
      : Type{Type_Kind::e_vec}, element_kind(element_kind), rows(rows)
    {
    }
  };

  struct Type_Mat: public Type {
    Type_Kind element_kind;
    i64 rows;
    i64 columns;

    Type_Mat(Type_Kind element_kind, i64 rows, i64 columns)
      : Type{Type_Kind::e_mat}, element_kind(element_kind), rows(rows),
        columns(columns)
    {
    }
  };
} // namespace vush::ir
