#include <vush_ast/types.hpp>

#include <vush_ast/ast.hpp>

namespace vush::ast {
  template<>
  bool instanceof<Type_Builtin>(Type const& node)
  {
    return node.type_kind == Type_Kind::type_builtin;
  }

  template<>
  bool instanceof<Type_Struct>(Type const& node)
  {
    return node.type_kind == Type_Kind::type_struct;
  }

  template<>
  bool instanceof<Type_Array>(Type const& node)
  {
    return node.type_kind == Type_Kind::type_array;
  }

  bool is_integer_based(Type const& type)
  {
    return is_integer_vector(type) || is_integer(type);
  }

  bool is_signed_integer_based(Type const& type)
  {
    return is_signed_integer_vector(type) || is_signed_integer(type);
  }

  bool is_unsigned_integer_based(Type const& type)
  {
    return is_unsigned_integer_vector(type) || is_unsigned_integer(type);
  }

  bool is_fp_based(Type const& type)
  {
    return is_matrix(type) || is_fp_vector(type) || is_fp(type);
  }

  bool is_void(Type const& type)
  {
    return type.type_kind == Type_Kind::type_builtin &&
           static_cast<Type_Builtin const&>(type).value ==
             Type_Builtin_Kind::e_void;
  }

  bool is_scalar(Type const& type)
  {
    Type_Builtin const& builtin = static_cast<Type_Builtin const&>(type);
    using Kind = Type_Builtin_Kind;
    return type.type_kind == Type_Kind::type_builtin &&
           (builtin.value == Kind::e_bool || builtin.value == Kind::e_int ||
            builtin.value == Kind::e_uint || builtin.value == Kind::e_float ||
            builtin.value == Kind::e_double);
  }

  bool is_integer(Type const& type)
  {
    Type_Builtin const& builtin = static_cast<Type_Builtin const&>(type);
    return type.type_kind == Type_Kind::type_builtin &&
           (builtin.value == Type_Builtin_Kind::e_int ||
            builtin.value == Type_Builtin_Kind::e_uint);
  }

  bool is_signed_integer(Type const& type)
  {
    Type_Builtin const& builtin = static_cast<Type_Builtin const&>(type);
    return type.type_kind == Type_Kind::type_builtin &&
           builtin.value == Type_Builtin_Kind::e_int;
  }

  bool is_unsigned_integer(Type const& type)
  {
    Type_Builtin const& builtin = static_cast<Type_Builtin const&>(type);
    return type.type_kind == Type_Kind::type_builtin &&
           builtin.value == Type_Builtin_Kind::e_uint;
  }

  bool is_fp(Type const& type)
  {
    Type_Builtin const& builtin = static_cast<Type_Builtin const&>(type);
    return type.type_kind == Type_Kind::type_builtin &&
           (builtin.value == Type_Builtin_Kind::e_float ||
            builtin.value == Type_Builtin_Kind::e_double);
  }

  bool is_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_vec2 || v == Kind::e_vec3 || v == Kind::e_vec4 ||
           v == Kind::e_dvec2 || v == Kind::e_dvec3 || v == Kind::e_dvec4 ||
           v == Kind::e_bvec2 || v == Kind::e_bvec3 || v == Kind::e_bvec4 ||
           v == Kind::e_ivec2 || v == Kind::e_ivec3 || v == Kind::e_ivec4 ||
           v == Kind::e_uvec2 || v == Kind::e_uvec3 || v == Kind::e_uvec4;
  }

  bool is_vector2(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_vec2 || v == Kind::e_dvec2 || v == Kind::e_bvec2 ||
           v == Kind::e_ivec2 || v == Kind::e_uvec2;
  }

  bool is_vector3(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_vec3 || v == Kind::e_dvec3 || v == Kind::e_bvec3 ||
           v == Kind::e_ivec3 || v == Kind::e_uvec3;
  }

  bool is_vector4(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_vec4 || v == Kind::e_dvec4 || v == Kind::e_bvec4 ||
           v == Kind::e_ivec4 || v == Kind::e_uvec4;
  }

  bool is_bool_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_bvec2 || v == Kind::e_bvec3 || v == Kind::e_bvec4;
  }

  bool is_integer_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_ivec2 || v == Kind::e_ivec3 || v == Kind::e_ivec4 ||
           v == Kind::e_uvec2 || v == Kind::e_uvec3 || v == Kind::e_uvec4;
  }

  bool is_signed_integer_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_ivec2 || v == Kind::e_ivec3 || v == Kind::e_ivec4;
  }

  bool is_unsigned_integer_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_uvec2 || v == Kind::e_uvec3 || v == Kind::e_uvec4;
  }

  bool is_fp_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_vec2 || v == Kind::e_vec3 || v == Kind::e_vec4 ||
           v == Kind::e_dvec2 || v == Kind::e_dvec3 || v == Kind::e_dvec4;
  }

  bool is_i32_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_ivec2 || v == Kind::e_ivec3 || v == Kind::e_ivec4;
  }

  bool is_u32_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_uvec2 || v == Kind::e_uvec3 || v == Kind::e_uvec4;
  }

  bool is_f32_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_vec2 || v == Kind::e_vec3 || v == Kind::e_vec4;
  }

  bool is_f64_vector(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_dvec2 || v == Kind::e_dvec3 || v == Kind::e_dvec4;
  }

  bool is_matrix(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_mat2 || v == Kind::e_mat3 || v == Kind::e_mat4 ||
           v == Kind::e_mat2x3 || v == Kind::e_mat2x4 || v == Kind::e_mat3x2 ||
           v == Kind::e_mat3x4 || v == Kind::e_mat4x2 || v == Kind::e_mat4x3 ||
           v == Kind::e_dmat2 || v == Kind::e_dmat3 || v == Kind::e_dmat4 ||
           v == Kind::e_dmat2x3 || v == Kind::e_dmat2x4 ||
           v == Kind::e_dmat3x2 || v == Kind::e_dmat3x4 ||
           v == Kind::e_dmat4x2 || v == Kind::e_dmat4x3;
  }

  bool is_f32_matrix(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_mat2 || v == Kind::e_mat3 || v == Kind::e_mat4 ||
           v == Kind::e_mat2x3 || v == Kind::e_mat2x4 || v == Kind::e_mat3x2 ||
           v == Kind::e_mat3x4 || v == Kind::e_mat4x2 || v == Kind::e_mat4x3;
  }

  bool is_f64_matrix(Type const& type)
  {
    if(!instanceof<Type_Builtin>(type)) {
      return false;
    }

    using Kind = Type_Builtin_Kind;
    Kind const v = static_cast<Type_Builtin const&>(type).value;
    return v == Kind::e_dmat2 || v == Kind::e_dmat3 || v == Kind::e_dmat4 ||
           v == Kind::e_dmat2x3 || v == Kind::e_dmat2x4 ||
           v == Kind::e_dmat3x2 || v == Kind::e_dmat3x4 ||
           v == Kind::e_dmat4x2 || v == Kind::e_dmat4x3;
  }

  i64 get_vector_size(Type const& type)
  {
    if(is_vector2(type)) {
      return 2;
    } else if(is_vector3(type)) {
      return 3;
    } else if(is_vector4(type)) {
      return 4;
    }

    return -1;
  }

  i64 get_matrix_rows(Type const& type)
  {
    if(!is_matrix(type)) {
      return -1;
    }

    Type_Builtin_Kind const kind = static_cast<Type_Builtin const&>(type).value;
    switch(kind) {
    case Type_Builtin_Kind::e_mat2:
    case Type_Builtin_Kind::e_mat2x3:
    case Type_Builtin_Kind::e_mat2x4:
      return 2;
    case Type_Builtin_Kind::e_mat3:
    case Type_Builtin_Kind::e_mat3x2:
    case Type_Builtin_Kind::e_mat3x4:
      return 3;
    case Type_Builtin_Kind::e_mat4:
    case Type_Builtin_Kind::e_mat4x2:
    case Type_Builtin_Kind::e_mat4x3:
      return 4;
    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  i64 get_matrix_columns(Type const& type)
  {
    if(!is_matrix(type)) {
      return -1;
    }

    Type_Builtin_Kind const kind = static_cast<Type_Builtin const&>(type).value;
    switch(kind) {
    case Type_Builtin_Kind::e_mat2:
    case Type_Builtin_Kind::e_mat3x2:
    case Type_Builtin_Kind::e_mat4x2:
      return 2;
    case Type_Builtin_Kind::e_mat3:
    case Type_Builtin_Kind::e_mat2x3:
    case Type_Builtin_Kind::e_mat4x3:
      return 3;
    case Type_Builtin_Kind::e_mat4:
    case Type_Builtin_Kind::e_mat2x4:
    case Type_Builtin_Kind::e_mat3x4:
      return 4;
    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  bool is_opaque_type(Type const& generic_type)
  {
    switch(generic_type.type_kind) {
    case Type_Kind::type_builtin: {
      Type_Builtin_Kind const v =
        static_cast<Type_Builtin const&>(generic_type).value;
      return static_cast<i32>(v) >
             static_cast<i32>(Type_Builtin_Kind::e_dmat4x3);
    }

    case Type_Kind::type_struct: {
      Type_Struct const& type = static_cast<Type_Struct const&>(generic_type);
      Decl_Struct const* const def = type.definition;
      if(def == nullptr) {
        return true;
      }

      for(auto const& field: def->fields) {
        bool const result = is_opaque_type(*field.type);
        if(result) {
          return true;
        }
      }
      return false;
    }

    case Type_Kind::type_array: {
      Type_Array const& type = static_cast<Type_Array const&>(generic_type);
      return is_opaque_type(*type.base);
    }
    }
  }

  bool is_arithmetic_type(Type const& generic_type)
  {
    if(!instanceof<Type_Builtin>(generic_type)) {
      return false;
    }

    Type_Builtin const& type = static_cast<Type_Builtin const&>(generic_type);
    return type.value != Type_Builtin_Kind::e_void &&
           static_cast<i32>(type.value) <=
             static_cast<i32>(Type_Builtin_Kind::e_dmat4x3);
  }

  bool is_array(Type const& type)
  {
    return type.type_kind == Type_Kind::type_array;
  }

  bool is_sized_array(Type const& type)
  {
    return type.type_kind == Type_Kind::type_array &&
           static_cast<Type_Array const&>(type).size;
  }

  bool is_unsized_array(Type const& type)
  {
    return type.type_kind == Type_Kind::type_array &&
           !static_cast<Type_Array const&>(type).size;
  }

  bool is_image_type(Type const& type)
  {
    switch(type.type_kind) {
    case Type_Kind::type_builtin: {
      Type_Builtin_Kind const v = static_cast<Type_Builtin const&>(type).value;
      return static_cast<i32>(v) >
             static_cast<i32>(Type_Builtin_Kind::e_dmat4x3);
    }

    case Type_Kind::type_struct: {
      return false;
    }

    case Type_Kind::type_array: {
      Type_Array const& array = static_cast<Type_Array const&>(type);
      return is_image_type(*array.base);
    }
    }
  }
} // namespace vush::ast
