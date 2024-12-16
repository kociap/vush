#pragma once

#include <anton/string.hpp>

#include <vush_core/types.hpp>
#include <vush_ir/decoration.hpp>

namespace vush::ir {
  enum struct Type_Kind {
    e_void,
    e_bool,
    e_int8,
    e_int16,
    e_int32,
    e_uint8,
    e_uint16,
    e_uint32,
    e_fp16,
    e_fp32,
    e_fp64,
    e_ptr,
    e_sampler,
    e_image,
    e_texture,
    e_composite,
    e_array,
    e_vec,
    e_mat,
  };

  struct Type: public Decorable {
    Type_Kind kind;

    Type(Type_Kind kind): kind(kind) {}
  };

  template<typename T>
  [[nodiscard]] bool instanceof(Type const& type);

  template<typename T>
  [[nodiscard]] bool instanceof(Type const* type)
  {
    return instanceof<T>(*type);
  }

  [[nodiscard]] bool compare_types_equal(Type const& lhs, Type const& rhs);

  [[nodiscard]] Type* get_type_void();
  [[nodiscard]] Type* get_type_bool();
  [[nodiscard]] Type* get_type_int8();
  [[nodiscard]] Type* get_type_int16();
  [[nodiscard]] Type* get_type_int32();
  [[nodiscard]] Type* get_type_uint8();
  [[nodiscard]] Type* get_type_uint16();
  [[nodiscard]] Type* get_type_uint32();
  [[nodiscard]] Type* get_type_fp16();
  [[nodiscard]] Type* get_type_fp32();
  [[nodiscard]] Type* get_type_fp64();
  [[nodiscard]] Type* get_type_ptr();

  [[nodiscard]] bool is_int_type(Type const& type);
  [[nodiscard]] bool is_int_based(Type const& type);
  [[nodiscard]] bool is_signed_int_type(Type const& type);
  [[nodiscard]] bool is_signed_int_based(Type const& type);
  [[nodiscard]] bool is_unsigned_int_type(Type const& type);
  [[nodiscard]] bool is_unsigned_int_based(Type const& type);
  [[nodiscard]] bool is_fp_type(Type const& type);
  [[nodiscard]] bool is_fp_based(Type const& type);
  [[nodiscard]] bool is_vec2(Type const& type);
  [[nodiscard]] bool is_vec3(Type const& type);
  [[nodiscard]] bool is_vec4(Type const& type);
  [[nodiscard]] bool is_scalar_type(Type const& type);
  [[nodiscard]] bool is_primitive_type(Type const& type);
  [[nodiscard]] bool is_aggregate_type(Type const& type);
  [[nodiscard]] bool is_opaque_type(Type const& type);
  [[nodiscard]] bool is_pointer(Type const& type);

  enum struct Sampler_Dim {
    e_1D,
    e_2D,
    e_3D,
    e_rect,
    e_cube,
    e_MS,
    e_buffer,
    e_subpass,
    e_subpass_MS,
  };

  struct Type_Sampler: public Type {
    // Type of the data returned by this sampler.
    // Only fp and int.
    Type_Kind sampled_type;
    Sampler_Dim dimensions;
    bool array;
    bool shadow;

    Type_Sampler(Type_Kind sampled_type, Sampler_Dim dimensions, bool array,
                 bool shadow)
      : Type(Type_Kind::e_sampler), sampled_type(sampled_type),
        dimensions(dimensions), array(array), shadow(shadow)
    {
    }
  };

  struct Type_Image: public Type {
    // Type of the data returned by this sampler.
    // Only fp and int.
    Type_Kind sampled_type;
    Sampler_Dim dimensions;
    bool array;
    bool shadow;

    Type_Image(Type_Kind sampled_type, Sampler_Dim dimensions, bool array,
               bool shadow)
      : Type(Type_Kind::e_image), sampled_type(sampled_type),
        dimensions(dimensions), array(array), shadow(shadow)
    {
    }
  };

  struct Type_Texture: public Type {
    // Type of the data returned by this sampler.
    // Only fp and int.
    Type_Kind sampled_type;
    Sampler_Dim dimensions;
    bool array;
    bool shadow;

    Type_Texture(Type_Kind sampled_type, Sampler_Dim dimensions, bool array,
                 bool shadow)
      : Type(Type_Kind::e_texture), sampled_type(sampled_type),
        dimensions(dimensions), array(array), shadow(shadow)
    {
    }
  };

  struct Type_Composite: public Type {
    anton::Array<Type*> elements;
    anton::String identifier;

    Type_Composite(Allocator* allocator, anton::String_View identifier)
      : Type{Type_Kind::e_composite}, elements(allocator),
        identifier(identifier, allocator)
    {
    }
  };

  struct Type_Array: public Type {
    Type* element_type;
    // 0 if unsized.
    i32 size;

    Type_Array(Type* element_type, i32 size)
      : Type{Type_Kind::e_array}, element_type(element_type), size(size)
    {
    }
  };

  struct Type_Vec: public Type {
    Type* element_type;
    i32 rows;

    Type_Vec(Type* element_type, i32 rows)
      : Type{Type_Kind::e_vec}, element_type(element_type), rows(rows)
    {
    }
  };

  struct Type_Mat: public Type {
    Type_Vec* column_type;
    i32 columns;

    Type_Mat(Type_Vec* column_type, i32 columns)
      : Type{Type_Kind::e_mat}, column_type(column_type), columns(columns)
    {
    }
  };
} // namespace vush::ir
