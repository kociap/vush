#include <vush_ir/types.hpp>

namespace vush::ir {
  bool compare_types_equal(Type const& lhs, Type const& rhs)
  {
    // TODO: Fix.
    return lhs.kind == rhs.kind;
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
    Type_Kind const kind = type.kind == Type_Kind::e_vec
                             ? static_cast<Type_Vec const&>(type).element_kind
                             : type.kind;
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
    if(type.kind == Type_Kind::e_mat) {
      return true;
    }

    Type_Kind const kind = type.kind == Type_Kind::e_vec
                             ? static_cast<Type_Vec const&>(type).element_kind
                             : type.kind;
    return kind == Type_Kind::e_fp16 || kind == Type_Kind::e_fp32 ||
           kind == Type_Kind::e_fp64;
  }
} // namespace vush::ir
