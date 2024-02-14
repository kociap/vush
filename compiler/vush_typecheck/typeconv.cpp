#include <vush_typecheck/typeconv.hpp>

#include <vush_ast/ast.hpp>

namespace vush {
  [[nodiscard]] static bool
  is_builtin_to_builtin_convertible(ast::Type_Builtin const* const to,
                                    ast::Type_Builtin const* const from)
  {
    using Kind = ast::Type_Builtin_Kind;
    Kind const tv = to->value;
    Kind const fv = from->value;
    // Type is always convertible to itself.
    if(tv == fv) {
      return true;
    }

    switch(tv) {
    case Kind::e_uint:
      return fv == Kind::e_int;
    case Kind::e_float:
      return fv == Kind::e_int || fv == Kind::e_uint;
    case Kind::e_double:
      return fv == Kind::e_int || fv == Kind::e_uint || fv == Kind::e_float;
    case Kind::e_uvec2:
      return fv == Kind::e_ivec2;
    case Kind::e_uvec3:
      return fv == Kind::e_ivec3;
    case Kind::e_uvec4:
      return fv == Kind::e_ivec4;
    case Kind::e_vec2:
      return fv == Kind::e_ivec2 || fv == Kind::e_uvec2;
    case Kind::e_vec3:
      return fv == Kind::e_ivec3 || fv == Kind::e_uvec3;
    case Kind::e_vec4:
      return fv == Kind::e_ivec4 || fv == Kind::e_uvec4;
    case Kind::e_dvec2:
      return fv == Kind::e_ivec2 || fv == Kind::e_uvec2 || fv == Kind::e_vec2;
    case Kind::e_dvec3:
      return fv == Kind::e_ivec3 || fv == Kind::e_uvec3 || fv == Kind::e_vec3;
    case Kind::e_dvec4:
      return fv == Kind::e_ivec4 || fv == Kind::e_uvec4 || fv == Kind::e_vec4;
    case Kind::e_dmat2:
      return fv == Kind::e_mat2;
    case Kind::e_dmat3:
      return fv == Kind::e_mat3;
    case Kind::e_dmat4:
      return fv == Kind::e_mat4;
    case Kind::e_dmat2x3:
      return fv == Kind::e_mat2x3;
    case Kind::e_dmat2x4:
      return fv == Kind::e_mat2x4;
    case Kind::e_dmat3x2:
      return fv == Kind::e_mat3x2;
    case Kind::e_dmat3x4:
      return fv == Kind::e_mat3x4;
    case Kind::e_dmat4x2:
      return fv == Kind::e_mat4x2;
    case Kind::e_dmat4x3:
      return fv == Kind::e_mat4x3;
    default:
      return false;
    }
  }

  bool is_convertible(ast::Type const* const to, ast::Type const* const from)
  {
    ast::Type_Kind const to_kind = to->type_kind;
    ast::Type_Kind const from_kind = from->type_kind;
    if(to_kind == ast::Type_Kind::type_builtin &&
       from_kind == ast::Type_Kind::type_builtin) {
      auto const to_type = static_cast<ast::Type_Builtin const*>(to);
      auto const from_type = static_cast<ast::Type_Builtin const*>(from);
      return is_builtin_to_builtin_convertible(to_type, from_type);
    }

    // If the at least one of the types is not a builtin, then no conversions
    // apply and the types must be equal.
    return compare_types_equal(*to, *from);
  }

  static constexpr i64 CONVERSION_EXACT_MATCH = 0;
  static constexpr i64 CONVERSION_BUILTIN_IMPLICIT = 1;

  [[nodiscard]] static anton::Optional<i64>
  rank_builtin_to_builtin_conversion(ast::Type_Builtin const* const to,
                                     ast::Type_Builtin const* const from)
  {
    using Kind = ast::Type_Builtin_Kind;
    Kind const tv = to->value;
    Kind const fv = from->value;
    // Type is always convertible to itself.
    if(tv == fv) {
      return CONVERSION_EXACT_MATCH;
    }

    switch(tv) {
    case Kind::e_uint:
      if(fv == Kind::e_int) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_float:
      if(fv == Kind::e_int || fv == Kind::e_uint) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_double:
      if(fv == Kind::e_int || fv == Kind::e_uint || fv == Kind::e_float) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_uvec2:
      if(fv == Kind::e_ivec2) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_uvec3:
      if(fv == Kind::e_ivec3) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_uvec4:
      if(fv == Kind::e_ivec4) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_vec2:
      if(fv == Kind::e_ivec2 || fv == Kind::e_uvec2) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_vec3:
      if(fv == Kind::e_ivec3 || fv == Kind::e_uvec3) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_vec4:
      if(fv == Kind::e_ivec4 || fv == Kind::e_uvec4) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dvec2:
      if(fv == Kind::e_ivec2 || fv == Kind::e_uvec2 || fv == Kind::e_vec2) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dvec3:
      if(fv == Kind::e_ivec3 || fv == Kind::e_uvec3 || fv == Kind::e_vec3) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dvec4:
      if(fv == Kind::e_ivec4 || fv == Kind::e_uvec4 || fv == Kind::e_vec4) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat2:
      if(fv == Kind::e_mat2) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat3:
      if(fv == Kind::e_mat3) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat4:
      if(fv == Kind::e_mat4) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat2x3:
      if(fv == Kind::e_mat2x3) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat2x4:
      if(fv == Kind::e_mat2x4) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat3x2:
      if(fv == Kind::e_mat3x2) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat3x4:
      if(fv == Kind::e_mat3x4) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat4x2:
      if(fv == Kind::e_mat4x2) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    case Kind::e_dmat4x3:
      if(fv == Kind::e_mat4x3) {
        return CONVERSION_BUILTIN_IMPLICIT;
      } else {
        return anton::null_optional;
      }
    default:
      return anton::null_optional;
    }
  }

  anton::Optional<i64> rank_conversion(ast::Type const* const to,
                                       ast::Type const* const from)
  {
    ast::Type_Kind const to_kind = to->type_kind;
    ast::Type_Kind const from_kind = from->type_kind;
    if(to_kind == ast::Type_Kind::type_builtin &&
       from_kind == ast::Type_Kind::type_builtin) {
      auto const to_type = static_cast<ast::Type_Builtin const*>(to);
      auto const from_type = static_cast<ast::Type_Builtin const*>(from);
      return rank_builtin_to_builtin_conversion(to_type, from_type);
    }

    if(compare_types_equal(*to, *from)) {
      return CONVERSION_EXACT_MATCH;
    } else {
      return anton::null_optional;
    }
  }
} // namespace vush
