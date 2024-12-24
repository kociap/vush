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
    e_vec,
    e_mat,
    e_sampler,
    e_image,
    e_sampled_image,
    e_composite,
    e_array,
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
  [[nodiscard]] Type* get_type_sampler();

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
  [[nodiscard]] bool is_sampler(Type const& type);
  [[nodiscard]] bool is_image(Type const& type);

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

  // Image_Dim
  //
  // Overlaps SPIR-V's Dimensionality.
  //
  enum struct Image_Dim : u8 {
    e_1D = 0,
    e_2D = 1,
    e_3D = 2,
    e_cube = 3,
    e_rect = 4,
    e_buffer = 5,
    e_subpass = 6,
  };

  enum struct Image_Format : u8 {
    e_unknown = 0,
    // TODO: Missing formats.
  };

  struct Type_Image: public Type {
    // Type of the data returned by this sampler.
    // Only fp and int.
    Type* sampled_type;
    Image_Format format = Image_Format::e_unknown;
    Image_Dim dimensions;
    // TODO: Consider making a bitfield.
    bool multisampled;
    bool array;
    bool shadow;
    bool sampled;

    Type_Image(Type* sampled_type, Image_Dim dimensions, bool multisampled,
               bool array, bool shadow, bool sampled)
      : Type(Type_Kind::e_image), sampled_type(sampled_type),
        dimensions(dimensions), multisampled(multisampled), array(array),
        shadow(shadow), sampled(sampled)
    {
    }
  };

  // Type_Sampled_Image
  //
  // Format is always unknown.
  //
  struct Type_Sampled_Image: public Type {
    // Type of the data returned by this sampler.
    // Only fp and int.
    Type* sampled_type;
    Image_Dim dimensions;
    // TODO: Consider making a bitfield.
    bool multisampled;
    bool array;
    bool shadow;

    Type_Sampled_Image(Type* sampled_type, Image_Dim dimensions,
                       bool multisampled, bool array, bool shadow)
      : Type(Type_Kind::e_sampled_image), sampled_type(sampled_type),
        dimensions(dimensions), multisampled(multisampled), array(array),
        shadow(shadow)
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
} // namespace vush::ir
