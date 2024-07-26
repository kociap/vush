#include <anton/ranges.hpp>
#include <vush_ir/types.hpp>

namespace vush::ir {
  bool compare_types_equal(Type const& lhs, Type const& rhs)
  {
    if(lhs.kind != rhs.kind) {
      return false;
    }
    switch(lhs.kind) {
    case Type_Kind::e_vec: {
      auto const v_lhs = static_cast<Type_Vec const&>(lhs);
      auto const v_rhs = static_cast<Type_Vec const&>(rhs);
      return v_lhs.element_kind == v_rhs.element_kind &&
             v_lhs.rows == v_rhs.rows;
    } break;

    case Type_Kind::e_mat: {
      auto const v_lhs = static_cast<Type_Mat const&>(lhs);
      auto const v_rhs = static_cast<Type_Mat const&>(rhs);
      return v_lhs.element_kind == v_rhs.element_kind &&
             v_lhs.rows == v_rhs.rows && v_lhs.columns == v_rhs.columns;
    } break;

    case Type_Kind::e_composite: {
      auto const v_lhs = static_cast<Type_Composite const&>(lhs);
      auto const v_rhs = static_cast<Type_Composite const&>(rhs);
      if(v_lhs.elements.size() != v_rhs.elements.size()) {
        return false;
      }

      for(auto const [lhs_type, rhs_type]:
          anton::zip(v_lhs.elements, v_rhs.elements)) {
        bool const result = compare_types_equal(*lhs_type, *rhs_type);
        if(!result) {
          return false;
        }
      }
      return true;
    } break;

    case Type_Kind::e_array: {
      auto const v_lhs = static_cast<Type_Array const&>(lhs);
      auto const v_rhs = static_cast<Type_Array const&>(rhs);
      bool const inner_equal =
        compare_types_equal(*v_lhs.element_type, *v_rhs.element_type);
      return v_lhs.size == v_rhs.size && inner_equal;
    } break;

    default:
      return true;
    }
  }

  static Type static_type_void{Type_Kind::e_void};
  static Type static_type_bool{Type_Kind::e_bool};
  static Type static_type_int8{Type_Kind::e_int8};
  static Type static_type_int16{Type_Kind::e_int16};
  static Type static_type_int32{Type_Kind::e_int32};
  static Type static_type_fp16{Type_Kind::e_fp16};
  static Type static_type_fp32{Type_Kind::e_fp32};
  static Type static_type_fp64{Type_Kind::e_fp64};
  static Type static_type_ptr{Type_Kind::e_ptr};

  Type* get_type_void()
  {
    return &static_type_void;
  }

  Type* get_type_bool()
  {
    return &static_type_bool;
  }

  Type* get_type_int8()
  {
    return &static_type_int8;
  }

  Type* get_type_int16()
  {
    return &static_type_int16;
  }

  Type* get_type_int32()
  {
    return &static_type_int32;
  }

  Type* get_type_fp16()
  {
    return &static_type_fp16;
  }

  Type* get_type_fp32()
  {
    return &static_type_fp32;
  }

  Type* get_type_fp64()
  {
    return &static_type_fp64;
  }

  Type* get_type_ptr()
  {
    return &static_type_ptr;
  }

  bool is_int_type(Type const& type)
  {
    return type.kind == Type_Kind::e_int8 || type.kind == Type_Kind::e_int16 ||
           type.kind == Type_Kind::e_int32;
  }

  bool is_int_based_type(Type const& type)
  {
    Type_Kind kind = type.kind;
    if(type.kind == Type_Kind::e_mat) {
      kind = static_cast<Type_Mat const&>(type).element_kind;
    } else if(type.kind == Type_Kind::e_vec) {
      kind = static_cast<Type_Vec const&>(type).element_kind;
    }

    return kind == Type_Kind::e_int8 || kind == Type_Kind::e_int16 ||
           kind == Type_Kind::e_int32;
  }

  bool is_fp_type(Type const& type)
  {
    return type.kind == Type_Kind::e_fp16 || type.kind == Type_Kind::e_fp32 ||
           type.kind == Type_Kind::e_fp64;
  }

  bool is_fp_based_type(Type const& type)
  {
    Type_Kind kind = type.kind;
    if(type.kind == Type_Kind::e_mat) {
      kind = static_cast<Type_Mat const&>(type).element_kind;
    } else if(type.kind == Type_Kind::e_vec) {
      kind = static_cast<Type_Vec const&>(type).element_kind;
    }

    return kind == Type_Kind::e_fp16 || kind == Type_Kind::e_fp32 ||
           kind == Type_Kind::e_fp64;
  }
} // namespace vush::ir
