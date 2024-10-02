#include <vush_ast_lowering/ast_lowering.hpp>

#include <anton/expected.hpp>
#include <anton/iterators/zip.hpp>
#include <anton/math/math.hpp>
#include <anton/ranges.hpp>
#include <anton/string7_view.hpp>

#include <vush_ast/ast.hpp>
#include <vush_autogen/builtin_symbols.hpp>
#include <vush_core/memory.hpp>
#include <vush_core/scoped_map.hpp>
#include <vush_core/utility.hpp>
#include <vush_ir/ir.hpp>
#include <vush_ir/types.hpp>

namespace vush {
  using namespace anton::literals;

  using Fn_Table = Scoped_Map<anton::String_View, ir::Function*>;
  using Symbol_Table = Scoped_Map<anton::String_View, ir::Instr*>;

  struct Lowering_Context {
  public:
    Allocator* allocator;
    Fn_Table fntable;
    Symbol_Table symtable;

    ir::Basic_Block* nearest_converge_block = nullptr;
    ir::Basic_Block* nearest_continuation_block = nullptr;
    ast::Type const* current_function_return_type = nullptr;

  private:
    i64 id = 0;

  public:
    Lowering_Context(Allocator* allocator)
      : allocator(allocator), fntable(allocator), symtable(allocator)
    {
    }

    [[nodiscard]] i64 get_next_id()
    {
      i64 const value = id;
      id += 1;
      return value;
    }
  };

  struct Builder {
  private:
    ir::Basic_Block* insert_block = nullptr;

  public:
    void set_insert_block(ir::Basic_Block* const bb)
    {
      insert_block = bb;
    }

    void insert(ir::Instr* const node)
    {
      ANTON_ASSERT(insert_block != nullptr, "insert_block has not been set");
      insert_block->insert(node);
    }
  };

  [[nodiscard]] static bool have_equal_element_count(ir::Type const& lhs,
                                                     ir::Type const& rhs)
  {
    bool const lhs_single_element = ir::is_int_type(lhs) || ir::is_fp_type(lhs);
    bool const rhs_single_element = ir::is_int_type(rhs) || ir::is_fp_type(rhs);
    if(lhs_single_element && rhs_single_element) {
      return true;
    }
    if(lhs.kind == ir::Type_Kind::e_vec && rhs.kind == ir::Type_Kind::e_vec) {
      auto const lhs_elements = static_cast<ir::Type_Vec const&>(lhs).rows;
      auto const rhs_elements = static_cast<ir::Type_Vec const&>(rhs).rows;
      return lhs_elements == rhs_elements;
    }
    if(lhs.kind == ir::Type_Kind::e_mat && rhs.kind == ir::Type_Kind::e_mat) {
      auto lhs_mat = static_cast<ir::Type_Mat const&>(lhs);
      auto rhs_mat = static_cast<ir::Type_Mat const&>(rhs);
      if(lhs_mat.columns != rhs_mat.columns) {
        return false;
      }

      return have_equal_element_count(*lhs_mat.column_type,
                                      *rhs_mat.column_type);
    }
    return false;
  }

  [[nodiscard]] static i32 get_bit_count(ir::Type const& type)
  {
    ir::Type_Kind kind = type.kind;
    if(instanceof<ir::Type_Vec>(type)) {
      kind = static_cast<ir::Type_Vec const&>(type).element_kind;
    } else if(instanceof<ir::Type_Mat>(type)) {
      kind = static_cast<ir::Type_Mat const&>(type).column_type->element_kind;
    }
    switch(kind) {
    case ir::Type_Kind::e_bool:
      return 1;
    case ir::Type_Kind::e_int8:
      return 8;
    case ir::Type_Kind::e_int16:
      return 16;
    case ir::Type_Kind::e_int32:
      return 32;
    case ir::Type_Kind::e_fp16:
      return 16;
    case ir::Type_Kind::e_fp32:
      return 32;
    case ir::Type_Kind::e_fp64:
      return 64;
    default:
      return 0;
    }
  }

  [[nodiscard]] static ir::Type*
  convert_ast_to_ir_type(Lowering_Context& ctx, ast::Type const* const type)
  {
    switch(type->type_kind) {
    case ast::Type_Kind::type_builtin: {
      auto const ast_type = static_cast<ast::Type_Builtin const*>(type);
      switch(ast_type->value) {
      case ast::Type_Builtin_Kind::e_void:
        return ir::get_type_void();
      case ast::Type_Builtin_Kind::e_bool:
        return ir::get_type_bool();
      case ast::Type_Builtin_Kind::e_int:
        return ir::get_type_int32();
      case ast::Type_Builtin_Kind::e_uint:
        return ir::get_type_uint32();
      case ast::Type_Builtin_Kind::e_float:
        return ir::get_type_fp32();
      case ast::Type_Builtin_Kind::e_double:
        return ir::get_type_fp64();
      case ast::Type_Builtin_Kind::e_vec2:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32,
                             2);
      case ast::Type_Builtin_Kind::e_vec3:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32,
                             3);
      case ast::Type_Builtin_Kind::e_vec4:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32,
                             4);
      case ast::Type_Builtin_Kind::e_dvec2:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64,
                             2);
      case ast::Type_Builtin_Kind::e_dvec3:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64,
                             3);
      case ast::Type_Builtin_Kind::e_dvec4:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64,
                             4);
      case ast::Type_Builtin_Kind::e_bvec2:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_bool,
                             2);
      case ast::Type_Builtin_Kind::e_bvec3:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_bool,
                             3);
      case ast::Type_Builtin_Kind::e_bvec4:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_bool,
                             4);
      case ast::Type_Builtin_Kind::e_ivec2:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator,
                             ir::Type_Kind::e_int32, 2);
      case ast::Type_Builtin_Kind::e_uvec2:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator,
                             ir::Type_Kind::e_uint32, 2);
      case ast::Type_Builtin_Kind::e_ivec3:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator,
                             ir::Type_Kind::e_int32, 3);
      case ast::Type_Builtin_Kind::e_uvec3:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator,
                             ir::Type_Kind::e_uint32, 3);
      case ast::Type_Builtin_Kind::e_ivec4:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator,
                             ir::Type_Kind::e_uint32, 4);
      case ast::Type_Builtin_Kind::e_uvec4:
        return VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator,
                             ir::Type_Kind::e_int32, 4);
      case ast::Type_Builtin_Kind::e_mat2:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 2),
          2);
      case ast::Type_Builtin_Kind::e_mat3:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 3),
          3);
      case ast::Type_Builtin_Kind::e_mat4:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 4),
          4);
      case ast::Type_Builtin_Kind::e_mat2x3:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 2),
          3);
      case ast::Type_Builtin_Kind::e_mat2x4:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 2),
          4);
      case ast::Type_Builtin_Kind::e_mat3x2:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 3),
          2);
      case ast::Type_Builtin_Kind::e_mat3x4:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 3),
          4);
      case ast::Type_Builtin_Kind::e_mat4x2:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 4),
          2);
      case ast::Type_Builtin_Kind::e_mat4x3:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp32, 4),
          3);
      case ast::Type_Builtin_Kind::e_dmat2:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 2),
          2);
      case ast::Type_Builtin_Kind::e_dmat3:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 3),
          3);
      case ast::Type_Builtin_Kind::e_dmat4:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 4),
          4);
      case ast::Type_Builtin_Kind::e_dmat2x3:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 2),
          3);
      case ast::Type_Builtin_Kind::e_dmat2x4:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 2),
          4);
      case ast::Type_Builtin_Kind::e_dmat3x2:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 3),
          2);
      case ast::Type_Builtin_Kind::e_dmat3x4:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 3),
          4);
      case ast::Type_Builtin_Kind::e_dmat4x2:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 4),
          2);
      case ast::Type_Builtin_Kind::e_dmat4x3:
        return VUSH_ALLOCATE(
          ir::Type_Mat, ctx.allocator,
          VUSH_ALLOCATE(ir::Type_Vec, ctx.allocator, ir::Type_Kind::e_fp64, 4),
          3);
      case ast::Type_Builtin_Kind::e_image1D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_image1DArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D, true,
                             false);
      case ast::Type_Builtin_Kind::e_image2D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_image2DArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_2D, true,
                             false);
      case ast::Type_Builtin_Kind::e_image2DMS:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_image2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_MS, true,
                             false);
      case ast::Type_Builtin_Kind::e_image2DRect:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_image3D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_imageBuffer:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_imageCube:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_imageCubeArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_iimage1D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_iimage1DArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_1D,
                             true, false);
      case ast::Type_Builtin_Kind::e_iimage2D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_iimage2DArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_2D,
                             true, false);
      case ast::Type_Builtin_Kind::e_iimage2DMS:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_iimage2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_MS,
                             true, false);
      case ast::Type_Builtin_Kind::e_iimage2DRect:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_iimage3D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_iimageBuffer:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_iimageCube:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_iimageCubeArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_uimage1D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_uimage1DArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_1D,
                             true, false);
      case ast::Type_Builtin_Kind::e_uimage2D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_uimage2DArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_2D,
                             true, false);
      case ast::Type_Builtin_Kind::e_uimage2DMS:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_uimage2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_MS,
                             true, false);
      case ast::Type_Builtin_Kind::e_uimage2DRect:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_uimage3D:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_uimageBuffer:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_uimageCube:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_uimageCubeArray:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_sampler:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_samplerBuffer:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_samplerCube:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_samplerCubeArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_samplerCubeArrayShadow:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_cube,
                             true, true);
      case ast::Type_Builtin_Kind::e_samplerCubeShadow:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_cube,
                             false, true);
      case ast::Type_Builtin_Kind::e_samplerShadow:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D,
                             false, true);
      case ast::Type_Builtin_Kind::e_sampler1D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_sampler1DArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D, true,
                             false);
      case ast::Type_Builtin_Kind::e_sampler1DArrayShadow:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D, true,
                             true);
      case ast::Type_Builtin_Kind::e_sampler1DShadow:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D,
                             false, true);
      case ast::Type_Builtin_Kind::e_sampler2D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_sampler2DArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_2D, true,
                             false);
      case ast::Type_Builtin_Kind::e_sampler2DArrayShadow:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_2D, true,
                             true);
      case ast::Type_Builtin_Kind::e_sampler2DMS:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_sampler2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_MS, true,
                             true);
      case ast::Type_Builtin_Kind::e_sampler2DRect:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_sampler2DRectShadow:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_rect,
                             false, true);
      case ast::Type_Builtin_Kind::e_sampler2DShadow:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_2D,
                             false, true);
      case ast::Type_Builtin_Kind::e_sampler3D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_isampler1D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_isampler1DArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_1D,
                             true, false);
      case ast::Type_Builtin_Kind::e_isampler2D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_isampler2DArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_2D,
                             true, false);
      case ast::Type_Builtin_Kind::e_isampler2DMS:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_isampler2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_MS,
                             true, false);
      case ast::Type_Builtin_Kind::e_isampler2DRect:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_isampler3D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_isamplerBuffer:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_isamplerCube:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_isamplerCubeArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_usampler1D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_usampler1DArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_1D,
                             true, false);
      case ast::Type_Builtin_Kind::e_usampler2D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_usampler2DArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_2D,
                             true, false);
      case ast::Type_Builtin_Kind::e_usampler2DMS:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_usampler2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_MS,
                             true, false);
      case ast::Type_Builtin_Kind::e_usampler2DRect:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_usampler3D:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_usamplerBuffer:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_usamplerCube:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_usamplerCubeArray:
        return VUSH_ALLOCATE(ir::Type_Sampler, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_texture1D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_texture1DArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_1D, true,
                             false);
      case ast::Type_Builtin_Kind::e_texture2D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_texture2DArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_2D, true,
                             false);
      case ast::Type_Builtin_Kind::e_texture2DMS:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_texture2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_MS, true,
                             false);
      case ast::Type_Builtin_Kind::e_texture2DRect:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_texture3D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_textureBuffer:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_textureCube:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_textureCubeArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_itexture1D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_itexture1DArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_1D,
                             true, false);
      case ast::Type_Builtin_Kind::e_itexture2D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_itexture2DArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_2D,
                             true, false);
      case ast::Type_Builtin_Kind::e_itexture2DMS:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_itexture2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_MS,
                             true, false);
      case ast::Type_Builtin_Kind::e_itexture2DRect:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_itexture3D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_itextureBuffer:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_itextureCube:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_itextureCubeArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_utexture1D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_1D,
                             false, false);
      case ast::Type_Builtin_Kind::e_utexture1DArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_1D,
                             true, false);
      case ast::Type_Builtin_Kind::e_utexture2D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_2D,
                             false, false);
      case ast::Type_Builtin_Kind::e_utexture2DArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_2D,
                             true, false);
      case ast::Type_Builtin_Kind::e_utexture2DMS:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_MS,
                             false, false);
      case ast::Type_Builtin_Kind::e_utexture2DMSArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_MS,
                             true, false);
      case ast::Type_Builtin_Kind::e_utexture2DRect:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_rect,
                             false, false);
      case ast::Type_Builtin_Kind::e_utexture3D:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_3D,
                             false, false);
      case ast::Type_Builtin_Kind::e_utextureBuffer:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_buffer,
                             false, false);
      case ast::Type_Builtin_Kind::e_utextureCube:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_cube,
                             false, false);
      case ast::Type_Builtin_Kind::e_utextureCubeArray:
        return VUSH_ALLOCATE(ir::Type_Texture, ctx.allocator,
                             ir::Type_Kind::e_uint32, ir::Sampler_Dim::e_cube,
                             true, false);
      case ast::Type_Builtin_Kind::e_subpassInput:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32, ir::Sampler_Dim::e_subpass,
                             false, false);
      case ast::Type_Builtin_Kind::e_subpassInputMS:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_fp32,
                             ir::Sampler_Dim::e_subpass_MS, false, false);
      case ast::Type_Builtin_Kind::e_isubpassInput:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32, ir::Sampler_Dim::e_subpass,
                             false, false);
      case ast::Type_Builtin_Kind::e_isubpassInputMS:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_int32,
                             ir::Sampler_Dim::e_subpass_MS, false, false);
      case ast::Type_Builtin_Kind::e_usubpassInput:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32,
                             ir::Sampler_Dim::e_subpass, false, false);
      case ast::Type_Builtin_Kind::e_usubpassInputMS:
        return VUSH_ALLOCATE(ir::Type_Image, ctx.allocator,
                             ir::Type_Kind::e_uint32,
                             ir::Sampler_Dim::e_subpass_MS, false, false);
      }
    }

    case ast::Type_Kind::type_struct: {
      auto const ast_type = static_cast<ast::Type_Struct const*>(type);
      auto const struct_definition = ast_type->definition;
      auto const composite =
        VUSH_ALLOCATE(ir::Type_Composite, ctx.allocator, ctx.allocator,
                      struct_definition->identifier.value);
      for(auto const field: struct_definition->fields) {
        auto const field_type = convert_ast_to_ir_type(ctx, field->type);
        composite->elements.push_back(field_type);
      }
      return composite;
    }

    case ast::Type_Kind::type_array: {
      auto const ast_type = static_cast<ast::Type_Array const*>(type);
      auto const base_type = convert_ast_to_ir_type(ctx, ast_type->base);
      i64 size = -1;
      if(ast_type->size != nullptr) {
        size = ast::get_lt_integer_value_as_u32(*ast_type->size);
      }
      auto const array =
        VUSH_ALLOCATE(ir::Type_Array, ctx.allocator, base_type, size);
      return array;
    }
    }
  }

  [[nodiscard]] static ir::Value*
  make_constant_for_type(Allocator* const allocator, ir::Type_Kind const kind,
                         i64 const value)
  {
    switch(kind) {
    case ir::Type_Kind::e_bool:
      return ir::make_constant_bool(allocator, value);
    case ir::Type_Kind::e_int32:
      return ir::make_constant_i32(allocator, value);
    case ir::Type_Kind::e_uint32:
      return ir::make_constant_u32(allocator, value);
    case ir::Type_Kind::e_fp32:
      return ir::make_constant_f32(allocator, value);
    case ir::Type_Kind::e_fp64:
      return ir::make_constant_f64(allocator, value);
    default:
      ANTON_UNREACHABLE("invalid type");
    }
  }

  [[nodiscard]] static anton::Expected<ir::Value*, Error> generate_conversion(
    Lowering_Context& ctx, Builder& builder, ir::Value* const value,
    ast::Type const* const target_type, ast::Type const* const source_type,
    Source_Info const& source_info)
  {
    if(ast::compare_types_equal(*target_type, *source_type)) {
      return {anton::expected_value, value};
    }

    ir::Type* const source_ir_type = value->type;
    switch(target_type->type_kind) {
    case ast::Type_Kind::type_builtin: {
      bool const target_is_unsigned_int =
        ast::is_unsigned_integer_based(*target_type);
      bool const target_is_signed_int =
        ast::is_signed_integer_based(*target_type);
      bool const target_is_int = target_is_signed_int || target_is_unsigned_int;
      bool const source_is_unsigned_int =
        ast::is_unsigned_integer_based(*source_type);
      bool const source_is_signed_int =
        ast::is_signed_integer_based(*source_type);
      bool const source_is_int = source_is_signed_int || source_is_unsigned_int;
      auto const target_ir_type = convert_ast_to_ir_type(ctx, target_type);
      if(target_is_int && source_is_int) {
        // Both types are (signed/unsigned) integers. They might either be
        // different size (trunc/[sz]ext) or same size (nothing to do).
        if(!have_equal_element_count(*target_ir_type, *source_ir_type)) {
          // TODO: Error.
          return {anton::expected_error, Error()};
        }
        i32 const source_bits = get_bit_count(*source_ir_type);
        i32 const target_bits = get_bit_count(*target_ir_type);
        if(target_bits < source_bits) {
          auto const instr =
            make_instr_cvt_trunc(ctx.allocator, ctx.get_next_id(),
                                 target_ir_type, value, source_info);
          builder.insert(instr);
          return {anton::expected_value, instr};
        } else if(target_bits > source_bits) {
          ir::Instr* instr = nullptr;
          if(source_is_signed_int) {
            instr = make_instr_cvt_sext(ctx.allocator, ctx.get_next_id(),
                                        target_ir_type, value, source_info);
          } else {
            instr = make_instr_cvt_zext(ctx.allocator, ctx.get_next_id(),
                                        target_ir_type, value, source_info);
          }
          builder.insert(instr);
          return {anton::expected_value, instr};
        } else {
          return {anton::expected_value, value};
        }
      }

      bool const target_is_fp = ast::is_fp_based(*target_type);
      bool const source_is_fp = ast::is_fp_based(*source_type);
      if(target_is_fp && source_is_fp) {
        // Both types are floating point. They might either be different size
        // (trunc/ext) or same size (nothing to do).
        if(!have_equal_element_count(*target_ir_type, *source_ir_type)) {
          // TODO: Error.
          return {anton::expected_error, Error()};
        }
        i32 const source_bits = get_bit_count(*source_ir_type);
        i32 const target_bits = get_bit_count(*target_ir_type);
        if(target_bits < source_bits) {
          auto const instr =
            make_instr_cvt_fptrunc(ctx.allocator, ctx.get_next_id(),
                                   target_ir_type, value, source_info);
          builder.insert(instr);
          return {anton::expected_value, instr};
        } else if(target_bits > source_bits) {
          auto const instr =
            make_instr_cvt_fpext(ctx.allocator, ctx.get_next_id(),
                                 target_ir_type, value, source_info);
          builder.insert(instr);
          return {anton::expected_value, instr};
        } else {
          return {anton::expected_value, value};
        }
      }

      if(target_is_fp && source_is_int) {
        // Conversion from int to fp.
        ir::Instr* instr = nullptr;
        if(source_is_signed_int) {
          instr = ir::make_instr_cvt_si2fp(ctx.allocator, ctx.get_next_id(),
                                           target_ir_type, value, source_info);
        } else {
          instr = ir::make_instr_cvt_ui2fp(ctx.allocator, ctx.get_next_id(),
                                           target_ir_type, value, source_info);
        }
        builder.insert(instr);
        return {anton::expected_value, instr};
      }

      if(target_is_int && source_is_fp) {
        // Conversion from fp to int.
        ir::Instr* instr = nullptr;
        if(source_is_signed_int) {
          instr = ir::make_instr_cvt_fp2si(ctx.allocator, ctx.get_next_id(),
                                           target_ir_type, value, source_info);
        } else {
          instr = ir::make_instr_cvt_fp2ui(ctx.allocator, ctx.get_next_id(),
                                           target_ir_type, value, source_info);
        }
        builder.insert(instr);
        return {anton::expected_value, instr};
      }

      // TODO: Non-convertible error.
      return {anton::expected_error, Error()};
    } break;

    case ast::Type_Kind::type_struct:
    case ast::Type_Kind::type_array:
      // TODO: Non-convertible error.
      return {anton::expected_error, Error()};
    }
  }

  [[nodiscard]] static anton::Expected<ir::Value*, Error>
  generate_conversion(Lowering_Context& ctx, Builder& builder,
                      ir::Type* const target, ir::Value* const value,
                      Source_Info const& source_info)
  {
    ir::Type const* const source = value->type;
    if(ir::compare_types_equal(*target, *source)) {
      return {anton::expected_value, value};
    }

    bool const source_is_non_convertible = ir::is_aggregate_type(*source) ||
                                           ir::is_opaque_type(*source) ||
                                           ir::is_pointer(*source);
    bool const target_is_non_convertible = ir::is_aggregate_type(*target) ||
                                           ir::is_opaque_type(*target) ||
                                           ir::is_pointer(*target);
    if(source_is_non_convertible || target_is_non_convertible) {
      // TODO: Non-convertible error.
      return {anton::expected_error, Error()};
    } else {
      bool const target_is_unsigned_int = ir::is_unsigned_int_based(*target);
      bool const target_is_signed_int = ir::is_signed_int_based(*target);
      bool const target_is_int = target_is_signed_int || target_is_unsigned_int;
      bool const source_is_unsigned_int = ir::is_unsigned_int_based(*source);
      bool const source_is_signed_int = ir::is_signed_int_based(*source);
      bool const source_is_int = source_is_signed_int || source_is_unsigned_int;
      if(target_is_int && source_is_int) {
        // Both types are (signed/unsigned) integers. They might either be
        // different size (trunc/[sz]ext) or same size (nothing to do).
        if(!have_equal_element_count(*target, *source)) {
          // TODO: Error.
          return {anton::expected_error, Error()};
        }
        i32 const source_bits = get_bit_count(*source);
        i32 const target_bits = get_bit_count(*target);
        if(target_bits < source_bits) {
          auto const instr = ir::make_instr_cvt_trunc(
            ctx.allocator, ctx.get_next_id(), target, value, source_info);
          builder.insert(instr);
          return {anton::expected_value, instr};
        } else if(target_bits > source_bits) {
          ir::Instr* instr = nullptr;
          if(source_is_signed_int) {
            instr = make_instr_cvt_sext(ctx.allocator, ctx.get_next_id(),
                                        target, value, source_info);
          } else {
            instr = make_instr_cvt_zext(ctx.allocator, ctx.get_next_id(),
                                        target, value, source_info);
          }
          builder.insert(instr);
          return {anton::expected_value, instr};
        } else {
          return {anton::expected_value, value};
        }
      }

      bool const target_is_fp = ir::is_fp_based(*target);
      bool const source_is_fp = ir::is_fp_based(*source);
      if(target_is_fp && source_is_fp) {
        // Both types are floating point. They might either be different size
        // (trunc/ext) or same size (nothing to do).
        if(!have_equal_element_count(*target, *source)) {
          // TODO: Error.
          return {anton::expected_error, Error()};
        }
        i32 const source_bits = get_bit_count(*source);
        i32 const target_bits = get_bit_count(*target);
        if(target_bits < source_bits) {
          auto const instr = make_instr_cvt_fptrunc(
            ctx.allocator, ctx.get_next_id(), target, value, source_info);
          builder.insert(instr);
          return {anton::expected_value, instr};
        } else if(target_bits > source_bits) {
          auto const instr = make_instr_cvt_fpext(
            ctx.allocator, ctx.get_next_id(), target, value, source_info);
          builder.insert(instr);
          return {anton::expected_value, instr};
        } else {
          return {anton::expected_value, value};
        }
      }

      if(target_is_fp && source_is_int) {
        // Conversion from int to fp.
        ir::Instr* instr = nullptr;
        if(source_is_signed_int) {
          instr = ir::make_instr_cvt_si2fp(ctx.allocator, ctx.get_next_id(),
                                           target, value, source_info);
        } else {
          instr = ir::make_instr_cvt_ui2fp(ctx.allocator, ctx.get_next_id(),
                                           target, value, source_info);
        }
        builder.insert(instr);
        return {anton::expected_value, instr};
      }

      if(target_is_int && source_is_fp) {
        // Conversion from fp to int.
        ir::Instr* instr = nullptr;
        if(source_is_signed_int) {
          instr = ir::make_instr_cvt_fp2si(ctx.allocator, ctx.get_next_id(),
                                           target, value, source_info);
        } else {
          instr = ir::make_instr_cvt_fp2ui(ctx.allocator, ctx.get_next_id(),
                                           target, value, source_info);
        }
        builder.insert(instr);
        return {anton::expected_value, instr};
      }

      // TODO: Non-convertible error.
      return {anton::expected_error, Error()};
    }
  }

  [[nodiscard]] static ir::Value*
  construct_vec_from_vec(Lowering_Context& ctx, Builder& builder,
                         ir::Type_Vec* target, ir::Value* const value,
                         Source_Info const& source_info)
  {
    ANTON_ASSERT(instanceof<ir::Type_Vec>(value->type), "source is not vector");
    auto const source = static_cast<ir::Type_Vec const*>(value->type);
    if(ir::compare_types_equal(*target, *source)) {
      return value;
    }

    if(target->rows == source->rows) {
      // TODO: Error propagation.
      anton::Expected<ir::Value*, Error> result =
        generate_conversion(ctx, builder, target, value, source_info);
      ANTON_ASSERT(result.holds_value(), "error generating conversion");
      return result.value();
    } else {
      auto const construct = ir::make_instr_composite_construct(
        ctx.allocator, ctx.get_next_id(), target, source_info);
      i64 const min_rows = anton::math::min(source->rows, target->rows);
      i64 const max_rows = anton::math::max(source->rows, target->rows);
      for(i64 i = 0; i < min_rows; i += 1) {
        auto const element_type =
          VUSH_ALLOCATE(ir::Type, ctx.allocator, source->element_kind);
        auto const element =
          ir::make_instr_vector_extract(ctx.allocator, ctx.get_next_id(),
                                        element_type, value, i, source_info);
        builder.insert(element);
        if(target->element_kind == source->element_kind) {
          construct->add_element(element);
        } else {
          auto const cvt_element_type =
            VUSH_ALLOCATE(ir::Type, ctx.allocator, target->element_kind);
          auto const cvt_element = generate_conversion(
            ctx, builder, cvt_element_type, element, source_info);
          // TODO: Error propagation.
          ANTON_ASSERT(cvt_element.holds_value(),
                       "error generating conversion");
          construct->add_element(cvt_element.value());
        }
      }
      auto const zero =
        make_constant_for_type(ctx.allocator, target->element_kind, 0);
      for(i64 i = min_rows; i < max_rows; i += 1) {
        construct->add_element(zero);
      }
      builder.insert(construct);
      return construct;
    }
  }

  [[nodiscard]] static ast::Struct_Field const*
  get_field(ast::Decl_Struct const* const decl,
            anton::String_View const identifier)
  {
    for(ast::Struct_Field* const field: decl->fields) {
      if(field->identifier.value == identifier) {
        return field;
      }
    }

    return nullptr;
  }

  [[nodiscard]] static i32 get_field_index(ast::Decl_Struct const* const decl,
                                           anton::String_View const identifier)
  {
    i32 index = 0;
    for(ast::Struct_Field* const field: decl->fields) {
      if(field->identifier.value == identifier) {
        return index;
      }
      index += 1;
    }

    return -1;
  }

  [[nodiscard]] static ir::Instr*
  get_address(Lowering_Context& ctx, Builder& builder, ast::Expr const* expr);

  [[nodiscard]] static ir::Value*
  lower_expression(Lowering_Context& ctx, Builder& builder,
                   ast::Expr const* generic_expr);

  [[nodiscard]] static ir::Instr*
  address_expr_identifier(Lowering_Context& ctx, Builder& builder,
                          ast::Expr_Identifier const* const expr)
  {
    ANTON_UNUSED(builder);
    ir::Instr* const* const address = ctx.symtable.find_entry(expr->value);
    ANTON_ASSERT(address != nullptr, "missing entry in symbol table");
    return *address;
  }

  [[nodiscard]] static ir::Instr*
  address_expr_field(Lowering_Context& ctx, Builder& builder,
                     ast::Expr_Field const* const expr)
  {
    auto const base_eval_type = expr->base->evaluated_type;
    ANTON_ASSERT(base_eval_type->type_kind == ast::Type_Kind::type_struct,
                 "cannot address fields of a non-struct type");
    auto const base_type = static_cast<ast::Type_Struct const*>(base_eval_type);
    ir::Type* const addressed_type = convert_ast_to_ir_type(ctx, base_type);
    ir::Instr* const address = get_address(ctx, builder, expr->base);
    i32 const index = get_field_index(base_type->definition, expr->field.value);
    ANTON_ASSERT(index >= 0, "type has no field");
    ir::Value* const index_value = ir::make_constant_i32(ctx.allocator, index);
    ir::Instr* const getptr =
      ir::make_instr_getptr(ctx.allocator, ctx.get_next_id(), addressed_type,
                            address, index_value, expr->source_info);
    builder.insert(getptr);
    return getptr;
  }

  [[nodiscard]] static ir::Instr*
  address_expr_index(Lowering_Context& ctx, Builder& builder,
                     ast::Expr_Index const* const expr)
  {
    ir::Type* const addressed_type =
      convert_ast_to_ir_type(ctx, expr->base->evaluated_type);
    ir::Instr* const address = get_address(ctx, builder, expr->base);
    ir::Value* const index = lower_expression(ctx, builder, expr->index);
    ir::Instr* const getptr =
      ir::make_instr_getptr(ctx.allocator, ctx.get_next_id(), addressed_type,
                            address, index, expr->source_info);
    builder.insert(getptr);
    return getptr;
  }

  [[nodiscard]] static ir::Instr*
  address_expr_if(Lowering_Context& ctx, Builder& builder,
                  ast::Expr_If const* const expr)
  {
    ir::Value* const condition =
      lower_expression(ctx, builder, expr->condition);
    auto const then_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const else_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            then_block, else_block, expr->source_info);
    builder.insert(brcond);

    // Lower then branch.
    builder.set_insert_block(then_block);
    ir::Instr* const then_result = get_address(ctx, builder, expr->then_branch);
    auto const branch_from_then = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
    builder.insert(branch_from_then);

    // Lower else branch.
    builder.set_insert_block(else_block);
    ir::Instr* const else_result = get_address(ctx, builder, expr->else_branch);
    auto const branch_from_else = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
    builder.insert(branch_from_else);

    ANTON_ASSERT(compare_types_equal(*then_result->type, *else_result->type),
                 "expression types of then and else must be equal");

    // Insert the phi node at the start of the converge block.
    builder.set_insert_block(converge_block);
    auto const phi = ir::make_instr_phi(ctx.allocator, ctx.get_next_id(),
                                        ir::get_type_ptr(), expr->source_info);
    phi->srcs.push_back(then_result);
    then_result->add_referrer(phi);
    phi->srcs.push_back(else_result);
    else_result->add_referrer(phi);
    builder.insert(phi);
    return phi;
  }

  [[nodiscard]] static ir::Instr*
  address_expr_call(Lowering_Context& ctx, Builder& builder,
                    ast::Expr_Call const* const expr)
  {
    // TODO: Implement.
    ANTON_UNREACHABLE("unimplemented");
    return nullptr;
  }

  ir::Instr* get_address(Lowering_Context& ctx, Builder& builder,
                         ast::Expr const* const generic_expr)
  {
    switch(generic_expr->node_kind) {
    case ast::Node_Kind::expr_identifier: {
      auto const expr = static_cast<ast::Expr_Identifier const*>(generic_expr);
      return address_expr_identifier(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_field: {
      auto const expr = static_cast<ast::Expr_Field const*>(generic_expr);
      return address_expr_field(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_index: {
      auto const expr = static_cast<ast::Expr_Index const*>(generic_expr);
      return address_expr_index(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_if: {
      auto const expr = static_cast<ast::Expr_If const*>(generic_expr);
      return address_expr_if(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_call: {
      auto const expr = static_cast<ast::Expr_Call const*>(generic_expr);
      return address_expr_call(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_reinterpret:
      ANTON_UNREACHABLE("unimplemented");

    case ast::Node_Kind::expr_init:
      ANTON_UNREACHABLE("expr_init does not produce an addressable result");

    case ast::Node_Kind::expr_default:
      ANTON_UNREACHABLE("expr_default is not addressable");

    case ast::Node_Kind::lt_bool:
      ANTON_UNREACHABLE("lt_bool is not addressable");

    case ast::Node_Kind::lt_integer:
      ANTON_UNREACHABLE("lt_integer is not addressable");

    case ast::Node_Kind::lt_float:
      ANTON_UNREACHABLE("lt_float is not addressable");

    default:
      ANTON_UNREACHABLE("unhandled expression kind");
    }
  }

  [[nodiscard]] static ir::Value*
  lower_expression_and_cvt(Lowering_Context& ctx, Builder& builder,
                           ast::Type const* const target_type,
                           ast::Expr const* const expr)
  {
    auto const result = lower_expression(ctx, builder, expr);
    auto cvt_result =
      generate_conversion(ctx, builder, result, target_type,
                          expr->evaluated_type, expr->source_info);
    // TODO: Propagate error.
    ANTON_ASSERT(cvt_result.holds_value(), "conversion failed");
    return cvt_result.value();
  }

  [[nodiscard]] static ir::Value*
  lower_expression_and_cvt(Lowering_Context& ctx, Builder& builder,
                           ir::Type* const target_type,
                           ast::Expr const* const expr)
  {
    auto const result = lower_expression(ctx, builder, expr);
    auto cvt_result =
      generate_conversion(ctx, builder, target_type, result, expr->source_info);
    // TODO: Propagate error.
    ANTON_ASSERT(cvt_result.holds_value(), "conversion failed");
    return cvt_result.value();
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_identifier(Lowering_Context& ctx, Builder& builder,
                        ast::Expr_Identifier const* const expr)
  {
    ir::Type* const type = convert_ast_to_ir_type(ctx, expr->evaluated_type);
    ir::Instr* const address = get_address(ctx, builder, expr);
    ir::Instr* const load = ir::make_instr_load(
      ctx.allocator, ctx.get_next_id(), type, address, expr->source_info);
    builder.insert(load);
    return load;
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_field(Lowering_Context& ctx, Builder& builder,
                   ast::Expr_Field const* const expr)
  {
    auto const base_type = expr->base->evaluated_type;
    if(instanceof<ast::Type_Struct>(base_type)) {
      ir::Instr* const address = get_address(ctx, builder, expr);
      ir::Type* const type = convert_ast_to_ir_type(ctx, expr->evaluated_type);
      ir::Instr* const load = ir::make_instr_load(
        ctx.allocator, ctx.get_next_id(), type, address, expr->source_info);
      builder.insert(load);
      return load;
    }

    if(instanceof<ast::Type_Builtin>(base_type) && ast::is_vector(*base_type)) {
      // Vectors have fields, but we cannot take the address of
      // them, hence we need to construct a new composite or a scalar.
      ir::Type* const result_type =
        convert_ast_to_ir_type(ctx, expr->evaluated_type);
      ir::Value* const base = lower_expression(ctx, builder, expr->base);
      bool const result_is_scalar = expr->field.value.size_bytes() == 1;
      if(result_is_scalar) {
        char8 const field_char = expr->field.value.data()[0];
        i32 const extract_index = ast::vector_swizzle_char_to_index(field_char);
        auto const result = ir::make_instr_vector_extract(
          ctx.allocator, ctx.get_next_id(), result_type, base, extract_index,
          expr->source_info);
        builder.insert(result);
        return result;
      } else {
        auto const construct = ir::make_instr_composite_construct(
          ctx.allocator, ctx.get_next_id(), result_type, expr->source_info);
        for(char8 const field_char: expr->field.value.bytes()) {
          i32 const extract_index =
            ast::vector_swizzle_char_to_index(field_char);
          auto const result = ir::make_instr_vector_extract(
            ctx.allocator, ctx.get_next_id(), result_type, base, extract_index,
            expr->source_info);
          builder.insert(result);
          construct->add_element(result);
        }
        builder.insert(construct);
        return construct;
      }
    }

    ANTON_UNREACHABLE("invalid expr field base type");
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_index(Lowering_Context& ctx, Builder& builder,
                   ast::Expr_Index const* const expr)
  {
    ir::Instr* const address = get_address(ctx, builder, expr);
    ir::Type* const type = convert_ast_to_ir_type(ctx, expr->evaluated_type);
    ir::Instr* const load = ir::make_instr_load(
      ctx.allocator, ctx.get_next_id(), type, address, expr->source_info);
    builder.insert(load);
    return load;
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_init(Lowering_Context& ctx, Builder& builder,
                  ast::Expr_Init const* const expr)
  {
    auto const type = convert_ast_to_ir_type(ctx, expr->evaluated_type);
    auto const temporary = ir::make_instr_alloc(
      ctx.allocator, ctx.get_next_id(), type, expr->source_info);
    builder.insert(temporary);
    if(instanceof<ast::Type_Struct>(expr->evaluated_type)) {
      auto const constructed_type =
        static_cast<ast::Type_Struct const*>(expr->evaluated_type);
      for(auto const ginitializer: expr->initializers) {
        ANTON_ASSERT(instanceof<ast::Field_Initializer>(ginitializer),
                     "invalid struct initializer");
        auto const initializer =
          static_cast<ast::Field_Initializer const*>(ginitializer);
        ast::Struct_Field const* field = get_field(
          constructed_type->definition, initializer->identifier.value);
        auto const rhs = lower_expression_and_cvt(ctx, builder, field->type,
                                                  initializer->expression);
        i64 const index = get_field_index(constructed_type->definition,
                                          initializer->identifier.value);
        auto const value = ir::make_constant_i32(ctx.allocator, index);
        auto const field_address =
          ir::make_instr_getptr(ctx.allocator, ctx.get_next_id(), type,
                                temporary, value, initializer->source_info);
        builder.insert(field_address);
        auto const store =
          ir::make_instr_store(ctx.allocator, ctx.get_next_id(), field_address,
                               rhs, initializer->source_info);
        builder.insert(store);
      }
    } else if(is_vector(*expr->evaluated_type)) {
      auto const constructed_type = static_cast<ir::Type_Vec*>(type);
      auto const construct = ir::make_instr_composite_construct(
        ctx.allocator, ctx.get_next_id(), type, expr->source_info);
      i64 const vector_size = ast::get_vector_size(*expr->evaluated_type);
      // We handle constructor #2 separately.
      bool single_scalar_constructor = false;
      if(expr->initializers.size() == 1) {
        ANTON_ASSERT(instanceof<ast::Basic_Initializer>(expr->initializers[0]),
                     "vector initializer is not basic initializer");
        auto const initializer =
          static_cast<ast::Basic_Initializer const*>(expr->initializers[0]);
        single_scalar_constructor =
          ast::is_scalar(*initializer->expression->evaluated_type);
      }

      if(single_scalar_constructor) {
        // Overload #2.
        auto const initializer =
          static_cast<ast::Basic_Initializer const*>(expr->initializers[0]);
        auto const element_type = VUSH_ALLOCATE(ir::Type, ctx.allocator,
                                                constructed_type->element_kind);
        auto const value = lower_expression_and_cvt(ctx, builder, element_type,
                                                    initializer->expression);
        for(i64 i = 0; i < vector_size; i += 1) {
          construct->add_element(value);
        }
      } else {
        // Handle overloads #1, #3, #4, #5.
        // Push extracted elements into the construct instruction.
        for(auto const ginitializer: expr->initializers) {
          if(construct->elements.size() >= vector_size) {
            break;
          }

          ANTON_ASSERT(instanceof<ast::Basic_Initializer>(ginitializer),
                       "vector initializer is not basic initializer");
          auto const initializer =
            static_cast<ast::Basic_Initializer const*>(ginitializer);
          auto const value =
            lower_expression(ctx, builder, initializer->expression);
          if(ast::is_scalar(*initializer->expression->evaluated_type)) {
            auto const element_type = VUSH_ALLOCATE(
              ir::Type, ctx.allocator, constructed_type->element_kind);
            auto const result = generate_conversion(
              ctx, builder, element_type, value, initializer->source_info);
            ANTON_ASSERT(result.holds_value(), "conversion failed");
            construct->add_element(result.value());
          } else {
            ANTON_ASSERT(
              ast::is_vector(*initializer->expression->evaluated_type),
              "invalid expression kind");
            i64 const src_size =
              ast::get_vector_size(*initializer->expression->evaluated_type);
            for(i64 i = 0; i < src_size; i += 1) {
              if(construct->elements.size() >= vector_size) {
                break;
              }

              auto const source_type =
                static_cast<ir::Type_Vec const*>(value->type);
              auto const source_element_type = VUSH_ALLOCATE(
                ir::Type, ctx.allocator, source_type->element_kind);
              auto const extract = ir::make_instr_vector_extract(
                ctx.allocator, ctx.get_next_id(), source_element_type, value, i,
                initializer->expression->source_info);
              builder.insert(extract);
              auto const element_type = VUSH_ALLOCATE(
                ir::Type, ctx.allocator, constructed_type->element_kind);
              auto const result = generate_conversion(
                ctx, builder, element_type, extract, initializer->source_info);
              ANTON_ASSERT(result.holds_value(), "conversion failed");
              construct->add_element(result.value());
            }
          }
        }
        // Pad with zeros if missing initializers.
        auto const zero = make_constant_for_type(
          ctx.allocator, constructed_type->element_kind, 0);
        for(i64 i = construct->elements.size(); i < vector_size; i += 1) {
          construct->add_element(zero);
        }
      }
      builder.insert(construct);
    } else if(is_matrix(*expr->evaluated_type)) {
      auto const construct = ir::make_instr_composite_construct(
        ctx.allocator, ctx.get_next_id(), type, expr->source_info);
      i64 const matrix_rows = ast::get_matrix_rows(*expr->evaluated_type);
      i64 const matrix_cols = ast::get_matrix_columns(*expr->evaluated_type);
      auto const constructed_type = static_cast<ir::Type_Mat*>(type);
      if(expr->initializers.size() == 1) {
        ANTON_ASSERT(instanceof<ast::Basic_Initializer>(expr->initializers[0]),
                     "not basic initializer");
        auto const initializer =
          static_cast<ast::Basic_Initializer const*>(expr->initializers[0]);
        auto const value =
          lower_expression(ctx, builder, initializer->expression);
        if(ast::is_matrix(*initializer->expression->evaluated_type)) {
          // Overload #3.
          ANTON_ASSERT(
            instanceof<ir::Type_Mat>(value->type),
            "initializer value claimed to be matrix, but is not matrix");
          auto const source_type = static_cast<ir::Type_Mat*>(value->type);
          i64 const min_cols =
            anton::math::min(matrix_cols, source_type->columns);
          for(i64 i = 0; i < min_cols; i += 1) {
            auto const extract = ir::make_instr_composite_extract(
              ctx.allocator, ctx.get_next_id(), source_type->column_type, value,
              i, initializer->source_info);
            auto const column_type = VUSH_ALLOCATE(
              ir::Type_Vec, ctx.allocator,
              constructed_type->column_type->element_kind, matrix_rows);
            auto const column = construct_vec_from_vec(
              ctx, builder, column_type, extract, initializer->source_info);
            construct->add_element(column);
          }
        } else {
          // Overload #2.
          // Pad with zeros if missing initializers.
          auto const zero = make_constant_for_type(
            ctx.allocator, constructed_type->column_type->element_kind, 0);
          auto const element_type =
            VUSH_ALLOCATE(ir::Type, ctx.allocator,
                          constructed_type->column_type->element_kind);
          auto const element = generate_conversion(
            ctx, builder, element_type, value, initializer->source_info);
          // TODO: Propagate error.
          ANTON_ASSERT(element.holds_value(), "invalid conversion");
          for(i64 i = 0; i < matrix_cols; i += 1) {
            auto const column_type = VUSH_ALLOCATE(
              ir::Type_Vec, ctx.allocator,
              constructed_type->column_type->element_kind, matrix_rows);
            auto const column = ir::make_instr_composite_construct(
              ctx.allocator, ctx.get_next_id(), column_type, expr->source_info);
            // Fill the column with 0s.
            for(i64 r = 0; r < matrix_rows; r += 1) {
              column->add_element(zero);
            }

            // Set the correct element to the value.
            if(i < matrix_rows) {
              column->elements[i] = element.value();
            }

            construct->add_element(column);
            builder.insert(column);
          }
        }
      } else if(expr->initializers.size() == matrix_rows * matrix_cols) {
        // Overload #4.
        for(i64 col_idx = 0; col_idx < matrix_cols; col_idx += 1) {
          auto const column_type = VUSH_ALLOCATE(
            ir::Type_Vec, ctx.allocator,
            constructed_type->column_type->element_kind, matrix_rows);
          auto const column = ir::make_instr_composite_construct(
            ctx.allocator, ctx.get_next_id(), column_type, expr->source_info);
          for(i64 row_idx = 0; row_idx < matrix_rows; row_idx += 1) {
            auto const initializer = static_cast<ast::Basic_Initializer const*>(
              expr->initializers[col_idx * matrix_rows + row_idx]);
            auto const source =
              lower_expression(ctx, builder, initializer->expression);
            column->add_element(source);
          }
          construct->add_element(column);
          builder.insert(column);
        }
      } else if(expr->initializers.size() == matrix_cols) {
        // Overload #5.
        for(auto const ginitializer: expr->initializers) {
          auto const initializer =
            static_cast<ast::Basic_Initializer const*>(ginitializer);
          auto const source =
            lower_expression(ctx, builder, initializer->expression);
          auto const column_type = VUSH_ALLOCATE(
            ir::Type_Vec, ctx.allocator,
            constructed_type->column_type->element_kind, matrix_rows);
          auto const column = construct_vec_from_vec(
            ctx, builder, column_type, source, initializer->source_info);
          construct->add_element(column);
        }
      }
      // Pad with identity columns if missing initializers.
      auto const zero = make_constant_for_type(
        ctx.allocator, constructed_type->column_type->element_kind, 0);
      auto const one = make_constant_for_type(
        ctx.allocator, constructed_type->column_type->element_kind, 1);
      for(i64 i = construct->elements.size(); i < matrix_cols; i += 1) {
        auto const column_type = VUSH_ALLOCATE(
          ir::Type_Vec, ctx.allocator,
          constructed_type->column_type->element_kind, matrix_rows);
        auto const column = ir::make_instr_composite_construct(
          ctx.allocator, ctx.get_next_id(), column_type, expr->source_info);
        // Fill the column with 0s.
        for(i64 r = 0; r < matrix_rows; r += 1) {
          column->add_element(zero);
        }

        // Set the correct element to 1.
        if(i < matrix_rows) {
          column->elements[i] = one;
        }

        construct->add_element(column);
        builder.insert(column);
      }
      builder.insert(construct);
    } else {
      ANTON_UNREACHABLE("unreachable");
    }
    // We load the struct and pass it farther down.
    auto const instr_load = ir::make_instr_load(
      ctx.allocator, ctx.get_next_id(), type, temporary, expr->source_info);
    builder.insert(instr_load);
    return instr_load;
  }

  [[nodiscard]] static ir::ALU_Opcode
  select_opcode(anton::String_View const identifier, bool const binary,
                bool const result_is_fp, bool const result_is_sint,
                bool const result_is_uint, bool const parameters_are_fp,
                bool const parameters_are_sint)
  {
    if(identifier == "+"_sv) {
      ANTON_ASSERT(binary, "+ not binary");
      if(result_is_sint) {
        return ir::ALU_Opcode::e_iadd;
      } else if(result_is_uint) {
        return ir::ALU_Opcode::e_uadd;
      } else {
        return ir::ALU_Opcode::e_fadd;
      }
    } else if(identifier == "-"_sv) {
      if(binary) {
        if(result_is_sint) {
          return ir::ALU_Opcode::e_iadd;
        } else if(result_is_uint) {
          return ir::ALU_Opcode::e_uadd;
        } else {
          return ir::ALU_Opcode::e_fadd;
        }
      } else {
        if(result_is_fp) {
          return ir::ALU_Opcode::e_fneg;
        } else {
          return ir::ALU_Opcode::e_neg;
        }
      }
    } else if(identifier == "*"_sv) {
      ANTON_ASSERT(binary, "* not binary");
      if(result_is_sint) {
        return ir::ALU_Opcode::e_imul;
      } else if(result_is_uint) {
        return ir::ALU_Opcode::e_umul;
      } else {
        return ir::ALU_Opcode::e_fmul;
      }
    } else if(identifier == "/"_sv) {
      ANTON_ASSERT(binary, "/ not binary");
      if(result_is_sint) {
        return ir::ALU_Opcode::e_idiv;
      } else if(result_is_uint) {
        return ir::ALU_Opcode::e_udiv;
      } else {
        return ir::ALU_Opcode::e_fdiv;
      }
    } else if(identifier == "%"_sv) {
      ANTON_ASSERT(binary, "operator% not binary");
      ANTON_ASSERT(!result_is_fp, "result of operator% is floating point");
      if(result_is_sint) {
        return ir::ALU_Opcode::e_irem;
      } else {
        return ir::ALU_Opcode::e_urem;
      }
    } else if(identifier == "<<"_sv) {
      ANTON_ASSERT(binary, "operator<< not binary");
      ANTON_ASSERT(!result_is_fp, "result of operator<< is floating point");
      return ir::ALU_Opcode::e_shl;
    } else if(identifier == ">>"_sv) {
      ANTON_ASSERT(binary, "operator>> not binary");
      ANTON_ASSERT(!result_is_fp, "result of operator>> is floating point");
      return ir::ALU_Opcode::e_shr;
    } else if(identifier == "~"_sv) {
      ANTON_ASSERT(!binary, "operator~ not unary");
      ANTON_ASSERT(!result_is_fp, "result of operator~ is floating point");
      return ir::ALU_Opcode::e_inv;
    } else if(identifier == "&"_sv) {
      ANTON_ASSERT(binary, "operator& not binary");
      ANTON_ASSERT(!result_is_fp, "result of operator& is floating point");
      return ir::ALU_Opcode::e_and;
    } else if(identifier == "|"_sv) {
      ANTON_ASSERT(binary, "operator| not binary");
      ANTON_ASSERT(!result_is_fp, "result of operator| is floating point");
      return ir::ALU_Opcode::e_or;
    } else if(identifier == "^"_sv) {
      ANTON_ASSERT(binary, "operator^ not binary");
      ANTON_ASSERT(!result_is_fp, "result of operator^ is floating point");
      return ir::ALU_Opcode::e_xor;
    } else if(identifier == "!"_sv) {
      ANTON_ASSERT(!binary, "operator! not unary");
      return ir::ALU_Opcode::e_inv;
    } else if(identifier == "^"_sv) {
      ANTON_ASSERT(binary, "operator^ not binary");
      ANTON_ASSERT(!result_is_fp, "result of operator^ is floating point");
      return ir::ALU_Opcode::e_xor;
    } else if(identifier == "=="_sv) {
      ANTON_ASSERT(binary, "operator== not binary");
      if(parameters_are_fp) {
        return ir::ALU_Opcode::e_fcmp_eq;
      } else {
        return ir::ALU_Opcode::e_icmp_eq;
      }
    } else if(identifier == "!="_sv) {
      ANTON_ASSERT(binary, "operator!= not binary");
      if(parameters_are_fp) {
        return ir::ALU_Opcode::e_fcmp_neq;
      } else {
        return ir::ALU_Opcode::e_icmp_neq;
      }
    } else if(identifier == "<"_sv) {
      ANTON_ASSERT(binary, "operator< not binary");
      if(parameters_are_fp) {
        return ir::ALU_Opcode::e_fcmp_lt;
      } else if(parameters_are_sint) {
        return ir::ALU_Opcode::e_icmp_slt;
      } else {
        return ir::ALU_Opcode::e_icmp_ult;
      }
    } else if(identifier == ">"_sv) {
      ANTON_ASSERT(binary, "operator> not binary");
      if(parameters_are_fp) {
        return ir::ALU_Opcode::e_fcmp_gt;
      } else if(parameters_are_sint) {
        return ir::ALU_Opcode::e_icmp_sgt;
      } else {
        return ir::ALU_Opcode::e_icmp_ugt;
      }
    } else if(identifier == "<="_sv) {
      ANTON_ASSERT(binary, "operator<= not binary");
      if(parameters_are_fp) {
        return ir::ALU_Opcode::e_fcmp_le;
      } else if(parameters_are_sint) {
        return ir::ALU_Opcode::e_icmp_sle;
      } else {
        return ir::ALU_Opcode::e_icmp_ule;
      }
    } else if(identifier == ">="_sv) {
      ANTON_ASSERT(binary, "operator>= not binary");
      if(parameters_are_fp) {
        return ir::ALU_Opcode::e_fcmp_ge;
      } else if(parameters_are_sint) {
        return ir::ALU_Opcode::e_icmp_sge;
      } else {
        return ir::ALU_Opcode::e_icmp_uge;
      }
    }

    ANTON_UNREACHABLE("no opcode selected");
  }

  [[nodiscard]] static ir::Instr*
  lower_builtin_operator(Lowering_Context& ctx, Builder& builder,
                         ast::Expr_Call const* const expr)
  {
    ANTON_ASSERT(expr->is_unary() || expr->is_binary(),
                 "call to operator is not unary or binary");
    anton::String_View const identifier = anton::shrink_front_bytes(
      expr->identifier.value, "operator"_sv.size_bytes());
    // XOR is not short-circuitable.
    bool const operator_is_and = identifier == "&&"_sv;
    bool const short_circuited_operator =
      operator_is_and || (identifier == "||"_sv);
    if(short_circuited_operator) {
      ANTON_ASSERT(expr->is_binary(),
                   "call to short circuited operator is not binary");
      auto const builtin_bool =
        get_builtin_type(ast::Type_Builtin_Kind::e_bool);
      // Lower the LHS and jump based on its value.
      // AND on false jumps to converge.
      // OR on true jumps to converge.
      ir::Value* const lhs = lower_expression_and_cvt(
        ctx, builder, builtin_bool, expr->arguments[0]);
      auto const rhs_block =
        VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
      auto const converge_block =
        VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
      auto const then_block = operator_is_and ? rhs_block : converge_block;
      auto const else_block = operator_is_and ? converge_block : rhs_block;
      auto const brcond =
        ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), lhs, then_block,
                              else_block, expr->source_info);
      builder.insert(brcond);

      // Lower the RHS.
      builder.set_insert_block(rhs_block);
      ir::Value* const rhs = lower_expression_and_cvt(
        ctx, builder, builtin_bool, expr->arguments[1]);
      auto const branch = ir::make_instr_branch(
        ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
      builder.insert(branch);

      // Converge the operator.
      builder.set_insert_block(converge_block);
      auto const phi =
        ir::make_instr_phi(ctx.allocator, ctx.get_next_id(),
                           ir::get_type_bool(), expr->source_info);
      phi->add_source(lhs);
      phi->add_source(rhs);
      builder.insert(phi);
      return phi;
    }

    // Lower non-short-circuit operators.
    ast::Type const* const result_type = expr->evaluated_type;
    ir::Type* const ir_result_type = convert_ast_to_ir_type(ctx, result_type);
    bool const result_is_uint = ast::is_unsigned_integer_based(*result_type);
    bool const result_is_sint = ast::is_signed_integer_based(*result_type);
    bool const result_is_fp = ast::is_fp_based(*result_type);
    // Lower and convert arguments to parameter types.
    ast::Fn_Parameter_List const parameters = expr->function->parameters;
    ir::Value* const lhs = lower_expression_and_cvt(
      ctx, builder, parameters[0]->type, expr->arguments[0]);
    ir::Value* const rhs =
      expr->is_binary()
        ? lower_expression_and_cvt(ctx, builder, parameters[1]->type,
                                   expr->arguments[1])
        : nullptr;

    bool const parameters_are_fp = ast::is_fp_based(*parameters[0]->type);
    bool const parameters_are_sint =
      ast::is_signed_integer_based(*parameters[0]->type);
    ir::ALU_Opcode const opcode =
      select_opcode(identifier, expr->is_binary(), result_is_fp, result_is_sint,
                    result_is_uint, parameters_are_fp, parameters_are_sint);
    auto const instr =
      ir::make_instr_alu(ctx.allocator, ctx.get_next_id(), ir_result_type,
                         opcode, lhs, rhs, expr->source_info);
    builder.insert(instr);
    return instr;
  }

  [[nodiscard]] static ir::Instr*
  lower_builtin_function_call(Lowering_Context& ctx, Builder& builder,
                              ast::Expr_Call const* const expr)
  {
    ir::Type* const type = convert_ast_to_ir_type(ctx, expr->evaluated_type);
    ir::Instr_ext_call* const call =
      select_ext(ctx.allocator, ctx.get_next_id(), type, expr);
    // TODO: Do proper error handling.
    ANTON_FAIL(call != nullptr, "no ext selected");
    for(auto const [p, a]:
        anton::zip(expr->function->parameters, expr->arguments)) {
      ir::Value* const result =
        lower_expression_and_cvt(ctx, builder, p->type, a);
      call->add_argument(result);
    }
    builder.insert(call);
    return call;
  }

  [[nodiscard]] static ir::Instr*
  lower_expr_call(Lowering_Context& ctx, Builder& builder,
                  ast::Expr_Call const* const expr)
  {
    ast::Decl_Function* const fn = expr->function;
    if(fn->builtin) {
      if(anton::begins_with(fn->identifier.value, "operator"_sv)) {
        return lower_builtin_operator(ctx, builder, expr);
      } else {
        return lower_builtin_function_call(ctx, builder, expr);
      }
    } else {
      ir::Function* const* const function =
        ctx.fntable.find_entry(expr->identifier.value);
      ANTON_ASSERT(function != nullptr, "call has no function");
      ir::Type* const type = convert_ast_to_ir_type(ctx, expr->evaluated_type);
      auto const call = ir::make_instr_call(ctx.allocator, ctx.get_next_id(),
                                            *function, type, expr->source_info);
      for(auto [arg, param]:
          anton::zip(expr->arguments, expr->function->parameters)) {
        auto const value =
          lower_expression_and_cvt(ctx, builder, param->type, arg);
        call->add_argument(value);
      }
      builder.insert(call);
      return call;
    }
  }

  [[nodiscard]] static ir::Instr* lower_expr_if(Lowering_Context& ctx,
                                                Builder& builder,
                                                ast::Expr_If const* const expr)
  {
    ir::Value* const condition =
      lower_expression(ctx, builder, expr->condition);
    auto const then_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const else_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            then_block, else_block, expr->source_info);
    builder.insert(brcond);

    // Lower then branch.
    builder.set_insert_block(then_block);
    ir::Value* const then_result =
      lower_expression(ctx, builder, expr->then_branch);
    auto const jmp_from_then = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
    builder.insert(jmp_from_then);

    // Lower else branch.
    builder.set_insert_block(else_block);
    ir::Value* const else_result =
      lower_expression(ctx, builder, expr->else_branch);
    auto const jmp_from_else = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, expr->source_info);
    builder.insert(jmp_from_else);

    ANTON_ASSERT(compare_types_equal(*then_result->type, *else_result->type),
                 "expression types of then and else must be equal");

    // Insert phi nodes at the start of the converge block.
    builder.set_insert_block(converge_block);
    auto const phi = ir::make_instr_phi(ctx.allocator, ctx.get_next_id(),
                                        then_result->type, expr->source_info);
    ANTON_ASSERT(instanceof<ir::Instr>(then_result),
                 "phi argument is not an instruction");
    phi->srcs.push_back(static_cast<ir::Instr*>(then_result));
    then_result->add_referrer(phi);
    ANTON_ASSERT(instanceof<ir::Instr>(else_result),
                 "phi argument is not an instruction");
    phi->srcs.push_back(static_cast<ir::Instr*>(else_result));
    else_result->add_referrer(phi);
    builder.insert(phi);
    return phi;
  }

  // lower_expression
  // Lowers an ast::Expr node into a series of IR instructions.
  //
  ir::Value* lower_expression(Lowering_Context& ctx, Builder& builder,
                              ast::Expr const* const generic_expr)
  {
    switch(generic_expr->node_kind) {
    case ast::Node_Kind::lt_bool: {
      auto const expr = static_cast<ast::Lt_Bool const*>(generic_expr);
      auto const value = ir::make_constant_bool(ctx.allocator, expr->value);
      return value;
    } break;

    case ast::Node_Kind::lt_integer: {
      auto const expr = static_cast<ast::Lt_Integer const*>(generic_expr);
      switch(expr->kind) {
      case ast::Lt_Integer_Kind::i32: {
        auto const value =
          ir::make_constant_i32(ctx.allocator, expr->i32_value);
        return value;
      }

      case ast::Lt_Integer_Kind::u32: {
        return nullptr;
      }
      }
    } break;

    case ast::Node_Kind::lt_float: {
      auto const expr = static_cast<ast::Lt_Float const*>(generic_expr);
      switch(expr->kind) {
      case ast::Lt_Float_Kind::f32: {
        auto const value =
          ir::make_constant_f32(ctx.allocator, expr->f32_value);
        return value;
      }

      case ast::Lt_Float_Kind::f64: {
        auto const value =
          ir::make_constant_f64(ctx.allocator, expr->f64_value);
        return value;
      }
      }
    } break;

    case ast::Node_Kind::expr_identifier: {
      auto const expr = static_cast<ast::Expr_Identifier const*>(generic_expr);
      return lower_expr_identifier(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_field: {
      auto const expr = static_cast<ast::Expr_Field const*>(generic_expr);
      return lower_expr_field(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_index: {
      auto const expr = static_cast<ast::Expr_Index const*>(generic_expr);
      return lower_expr_index(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_init: {
      auto const expr = static_cast<ast::Expr_Init const*>(generic_expr);
      return lower_expr_init(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_call: {
      auto const expr = static_cast<ast::Expr_Call const*>(generic_expr);
      return lower_expr_call(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_if: {
      auto const expr = static_cast<ast::Expr_If const*>(generic_expr);
      return lower_expr_if(ctx, builder, expr);
    }

    case ast::Node_Kind::expr_reinterpret:
      ANTON_UNREACHABLE("unimplemented");

    case ast::Node_Kind::expr_default:
      ANTON_UNREACHABLE("expr_default is not lowerable");

    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  static void lower_statement(Lowering_Context& ctx, Builder& builder,
                              ast::Node const* const generic_stmt);

  static void lower_statement_block(Lowering_Context& ctx, Builder& builder,
                                    ast::Node_List const stmts)
  {
    ctx.symtable.push_scope();
    for(ast::Node* const stmt: stmts) {
      lower_statement(ctx, builder, stmt);
    }
    ctx.symtable.pop_scope();
  }

  static void lower_variable(Lowering_Context& ctx, Builder& builder,
                             ast::Variable const* const variable)
  {
    ir::Type* const type = convert_ast_to_ir_type(ctx, variable->type);
    ir::Instr* const instr = ir::make_instr_alloc(
      ctx.allocator, ctx.get_next_id(), type, variable->source_info);
    builder.insert(instr);
    ctx.symtable.add_entry(variable->identifier.value, instr);
  }

  [[nodiscard]] static ir::ALU_Opcode
  select_opcode(ast::Stmt_Assignment const* const stmt)
  {
    ANTON_ASSERT(ast::is_assignment_arithmetic(stmt),
                 "cannot select opcode for non-arithmetic assignment");
    bool const result_fp = ast::is_fp_based(*stmt->lhs->evaluated_type);
    bool const result_sint =
      ast::is_signed_integer_based(*stmt->lhs->evaluated_type);
    bool const result_uint =
      ast::is_unsigned_integer_based(*stmt->lhs->evaluated_type);
    // Because this is a compound assignment and it expands to the form
    //   lhs_type = lhs_type op (lhs_type)rhs_type
    // we always consider the parameters to be the same type as the result.
    bool const params_fp = result_fp;
    bool const params_sint = result_sint;
    switch(stmt->kind) {
    case ast::Assignment_Kind::e_add:
      return select_opcode("+"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_sub:
      return select_opcode("-"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_mul:
      return select_opcode("*"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_div:
      return select_opcode("/"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_mod:
      return select_opcode("%"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_and:
      return select_opcode("&"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_or:
      return select_opcode("|"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_xor:
      return select_opcode("^"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_shl:
      return select_opcode("<<"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    case ast::Assignment_Kind::e_shr:
      return select_opcode(">>"_sv, true, result_fp, result_sint, result_uint,
                           params_fp, params_sint);
    default:
      ANTON_UNREACHABLE("unreachable");
    }
  }

  static void lower_stmt_assignment(Lowering_Context& ctx, Builder& builder,
                                    ast::Stmt_Assignment const* const stmt)
  {
    ir::Value* const rhs = lower_expression(ctx, builder, stmt->rhs);
    auto const field_expr = static_cast<ast::Expr_Field const*>(stmt->lhs);
    // Handle swizzles of vectors separately.
    if(instanceof<ast::Expr_Field>(stmt->lhs) &&
       ast::is_vector(*field_expr->base->evaluated_type)) {
      auto const dst = get_address(ctx, builder, field_expr->base);
      ast::Type const* const ast_base_type = field_expr->base->evaluated_type;
      auto const target_type =
        safe_cast<ir::Type_Vec*>(convert_ast_to_ir_type(ctx, ast_base_type));
      auto const source_type = safe_cast<ir::Type_Vec*>(rhs->type);
      ir::Instr* const initial_target =
        ir::make_instr_load(ctx.allocator, ctx.get_next_id(), target_type, dst,
                            field_expr->base->source_info);
      ir::Instr* value = initial_target;
      builder.insert(value);
      for(auto const [src_index, swizzle]:
          anton::zip(anton::irange(0, 3), field_expr->field.value.bytes())) {
        i64 const dst_index = ast::vector_swizzle_char_to_index(swizzle);
        // Extract the element from the source vector.
        auto const source_element_type =
          VUSH_ALLOCATE(ir::Type, ctx.allocator, source_type->element_kind);
        ir::Instr* element = ir::make_instr_vector_extract(
          ctx.allocator, ctx.get_next_id(), source_element_type, rhs, src_index,
          field_expr->source_info);
        builder.insert(element);
        if(ast::is_assignment_arithmetic(stmt)) {
          // Extract the element from the target and do arithmetic.
          auto const target_element_type =
            VUSH_ALLOCATE(ir::Type, ctx.allocator, target_type->element_kind);
          auto target_element = ir::make_instr_vector_extract(
            ctx.allocator, ctx.get_next_id(), target_element_type, rhs,
            dst_index, field_expr->source_info);
          builder.insert(target_element);
          ir::ALU_Opcode const op = select_opcode(stmt);
          element = ir::make_instr_alu(ctx.allocator, ctx.get_next_id(),
                                       target_element_type, op, target_element,
                                       element, stmt->source_info);
          builder.insert(element);
        }
        // Insert the element into the target vector.
        value = ir::make_instr_vector_insert(ctx.allocator, ctx.get_next_id(),
                                             value->type, value, element,
                                             dst_index, stmt->source_info);
        builder.insert(value);
      }
      auto const store = ir::make_instr_store(ctx.allocator, ctx.get_next_id(),
                                              dst, value, stmt->source_info);
      builder.insert(store);
    } else {
      ast::Type const* const lhs_type = stmt->lhs->evaluated_type;
      if(instanceof<ast::Type_Array>(lhs_type) ||
         instanceof<ast::Type_Struct>(lhs_type)) {
        auto const dst = get_address(ctx, builder, stmt->lhs);
        auto const store = ir::make_instr_store(
          ctx.allocator, ctx.get_next_id(), dst, rhs, stmt->source_info);
        builder.insert(store);
      } else {
        ANTON_ASSERT(instanceof<ast::Type_Builtin>(lhs_type),
                     "type is not builtin");
        auto const dst = get_address(ctx, builder, stmt->lhs);
        if(ast::is_assignment_arithmetic(stmt)) {
          auto const converted_rhs = generate_conversion(
            ctx, builder, rhs, stmt->lhs->evaluated_type,
            stmt->rhs->evaluated_type, stmt->rhs->source_info);
          // TODO: Proper error handling.
          ANTON_ASSERT(converted_rhs.holds_value(), "conversion failed");
          auto const target_type =
            convert_ast_to_ir_type(ctx, stmt->lhs->evaluated_type);
          auto const initial_target =
            ir::make_instr_load(ctx.allocator, ctx.get_next_id(), target_type,
                                dst, field_expr->base->source_info);
          builder.insert(initial_target);
          ir::ALU_Opcode const op = select_opcode(stmt);
          auto const alu = ir::make_instr_alu(
            ctx.allocator, ctx.get_next_id(), target_type, op, initial_target,
            converted_rhs.value(), stmt->source_info);
          builder.insert(alu);
          auto const store = ir::make_instr_store(
            ctx.allocator, ctx.get_next_id(), dst, alu, stmt->source_info);
          builder.insert(store);
        } else {
          auto const store = ir::make_instr_store(
            ctx.allocator, ctx.get_next_id(), dst, rhs, stmt->source_info);
          builder.insert(store);
        }
      }
    }
  }

  static void lower_stmt_return(Lowering_Context& ctx, Builder& builder,
                                ast::Stmt_Return const* const stmt)
  {
    if(stmt->expression != nullptr) {
      ir::Value* const ret_expr =
        lower_expression(ctx, builder, stmt->expression);
      auto cvt_result = generate_conversion(
        ctx, builder, ret_expr, ctx.current_function_return_type,
        stmt->expression->evaluated_type, stmt->expression->source_info);
      // TODO: Propagate error up.
      ANTON_ASSERT(cvt_result.holds_value(), "conversion failed");
      ir::Instr* const ret =
        ir::make_instr_return(ctx.allocator, ctx.get_next_id(),
                              cvt_result.value(), stmt->source_info);
      builder.insert(ret);
    } else {
      ir::Instr* const ret = ir::make_instr_return(
        ctx.allocator, ctx.get_next_id(), stmt->source_info);
      builder.insert(ret);
    }
  }

  static void lower_stmt_if(Lowering_Context& ctx, Builder& builder,
                            ast::Stmt_If const* const stmt)
  {
    ir::Value* const condition =
      lower_expression(ctx, builder, stmt->condition);
    auto const then_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const else_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            then_block, else_block, stmt->source_info);
    builder.insert(brcond);

    // Lower then branch.
    builder.set_insert_block(then_block);
    lower_statement_block(ctx, builder, stmt->then_branch);
    auto const branch_from_then = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, stmt->source_info);
    builder.insert(branch_from_then);

    // Lower else branch.
    builder.set_insert_block(else_block);
    lower_statement_block(ctx, builder, stmt->else_branch);
    auto const branch_from_else = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), converge_block, stmt->source_info);
    builder.insert(branch_from_else);

    builder.set_insert_block(converge_block);
  }

  static void lower_stmt_for(Lowering_Context& ctx, Builder& builder,
                             ast::Stmt_For const* const stmt)
  {
    auto const condition_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const loop_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const continuation_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());

    ctx.nearest_continuation_block = continuation_block;
    ctx.nearest_converge_block = converge_block;

    // Lower variables in the previous block.
    for(ast::Variable const* const variable: stmt->declarations) {
      lower_variable(ctx, builder, variable);
    }

    // Branch to the condition block from wherever we are.
    ir::Instr* branch = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), condition_block, stmt->source_info);
    builder.insert(branch);

    // Lower the condition.
    builder.set_insert_block(condition_block);
    ir::Value* const condition =
      lower_expression(ctx, builder, stmt->condition);
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            loop_block, converge_block, stmt->source_info);
    builder.insert(brcond);

    // Lower the loop block and branch to continuation.
    builder.set_insert_block(loop_block);
    lower_statement_block(ctx, builder, stmt->statements);
    auto const branch_to_continuation = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), continuation_block, stmt->source_info);
    builder.insert(branch_to_continuation);

    // Lower the actions in the continuation block.
    builder.set_insert_block(continuation_block);
    for(ast::Expr const* const expr: stmt->actions) {
      ir::Value* const instr = lower_expression(ctx, builder, expr);
      // Result is discarded.
      ANTON_UNUSED(instr);
    }

    builder.set_insert_block(converge_block);
  }

  static void lower_stmt_while(Lowering_Context& ctx, Builder& builder,
                               ast::Stmt_While const* const stmt)
  {
    auto const condition_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const loop_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());

    ctx.nearest_continuation_block = condition_block;
    ctx.nearest_converge_block = converge_block;

    // Branch to the condition block from wherever we are.
    ir::Instr* branch = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), condition_block, stmt->source_info);
    builder.insert(branch);

    // Lower the condition.
    builder.set_insert_block(condition_block);
    ir::Value* const condition =
      lower_expression(ctx, builder, stmt->condition);
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            loop_block, converge_block, stmt->source_info);
    builder.insert(brcond);

    // Lower the loop block.
    builder.set_insert_block(loop_block);
    lower_statement_block(ctx, builder, stmt->statements);
    auto const branch_to_condition = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), condition_block, stmt->source_info);
    builder.insert(branch_to_condition);

    builder.set_insert_block(converge_block);
  }

  static void lower_stmt_do_while(Lowering_Context& ctx, Builder& builder,
                                  ast::Stmt_Do_While const* const stmt)
  {
    auto const condition_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const loop_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const converge_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());

    ctx.nearest_continuation_block = condition_block;
    ctx.nearest_converge_block = converge_block;

    // Branch to the loop block from wherever we are.
    ir::Instr* branch = ir::make_instr_branch(ctx.allocator, ctx.get_next_id(),
                                              loop_block, stmt->source_info);
    builder.insert(branch);

    // Lower the loop block.
    builder.set_insert_block(loop_block);
    lower_statement_block(ctx, builder, stmt->statements);
    auto const branch_to_condition = ir::make_instr_branch(
      ctx.allocator, ctx.get_next_id(), condition_block, stmt->source_info);
    builder.insert(branch_to_condition);

    // Lower the condition.
    builder.set_insert_block(condition_block);
    ir::Value* const condition =
      lower_expression(ctx, builder, stmt->condition);
    auto const brcond =
      ir::make_instr_brcond(ctx.allocator, ctx.get_next_id(), condition,
                            loop_block, converge_block, stmt->source_info);
    builder.insert(brcond);

    builder.set_insert_block(converge_block);
  }

  static void lower_stmt_switch(Lowering_Context& ctx, Builder& builder,
                                ast::Stmt_Switch const* const stmt)
  {
    ir::Value* const selector =
      lower_expression(ctx, builder, stmt->expression);
    auto const default_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    ir::Instr_switch* const instr_switch =
      ir::make_instr_switch(ctx.allocator, ctx.get_next_id(), selector,
                            default_block, stmt->source_info);
    builder.insert(instr_switch);

    for(ast::Switch_Arm const* const arm: stmt->arms) {
      ir::Basic_Block* current_block = default_block;
      if(!arm->has_default) {
        current_block =
          VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
      }

      builder.set_insert_block(current_block);
      lower_statement_block(ctx, builder, arm->statements);
      for(ast::Expr const* const label: arm->labels) {
        ANTON_ASSERT(label->node_kind == ast::Node_Kind::lt_integer ||
                       label->node_kind == ast::Node_Kind::expr_default,
                     "label is not an integer");
        if(label->node_kind == ast::Node_Kind::lt_integer) {
          auto const node = static_cast<ast::Lt_Integer const*>(label);
          i64 const value =
            (node->kind == ast::Lt_Integer_Kind::i32 ? node->i32_value
                                                     : node->u32_value);
          instr_switch->add_label(ir::Switch_Label{value, current_block});
        }
      }
    }
  }

  static void lower_statement(Lowering_Context& ctx, Builder& builder,
                              ast::Node const* const generic_stmt)
  {
    switch(generic_stmt->node_kind) {
    case ast::Node_Kind::stmt_block: {
      auto const stmt = static_cast<ast::Stmt_Block const*>(generic_stmt);
      lower_statement_block(ctx, builder, stmt->statements);
    } break;

    case ast::Node_Kind::stmt_expression: {
      auto const stmt = static_cast<ast::Stmt_Expression const*>(generic_stmt);
      ir::Value* const value = lower_expression(ctx, builder, stmt->expression);
      // The result is discarded.
      ANTON_UNUSED(value);
    } break;

    case ast::Node_Kind::stmt_discard: {
      auto const instr = ir::make_instr_die(ctx.allocator, ctx.get_next_id(),
                                            generic_stmt->source_info);
      builder.insert(instr);
    } break;

    case ast::Node_Kind::stmt_break: {
      ANTON_ASSERT(ctx.nearest_converge_block != nullptr,
                   "missing loop converge block");
      auto const instr = ir::make_instr_branch(ctx.allocator, ctx.get_next_id(),
                                               ctx.nearest_converge_block,
                                               generic_stmt->source_info);
      builder.insert(instr);
    } break;

    case ast::Node_Kind::stmt_continue: {
      ANTON_ASSERT(ctx.nearest_continuation_block != nullptr,
                   "missing loop continuation block");
      auto const instr = ir::make_instr_branch(ctx.allocator, ctx.get_next_id(),
                                               ctx.nearest_continuation_block,
                                               generic_stmt->source_info);
      builder.insert(instr);
    } break;

    case ast::Node_Kind::stmt_return: {
      auto const stmt = static_cast<ast::Stmt_Return const*>(generic_stmt);
      lower_stmt_return(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::variable: {
      auto const stmt = static_cast<ast::Variable const*>(generic_stmt);
      lower_variable(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_assignment: {
      auto const stmt = static_cast<ast::Stmt_Assignment const*>(generic_stmt);
      lower_stmt_assignment(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_if: {
      auto const stmt = static_cast<ast::Stmt_If const*>(generic_stmt);
      lower_stmt_if(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_for: {
      auto const stmt = static_cast<ast::Stmt_For const*>(generic_stmt);
      lower_stmt_for(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_while: {
      auto const stmt = static_cast<ast::Stmt_While const*>(generic_stmt);
      lower_stmt_while(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_do_while: {
      auto const stmt = static_cast<ast::Stmt_Do_While const*>(generic_stmt);
      lower_stmt_do_while(ctx, builder, stmt);
    } break;

    case ast::Node_Kind::stmt_switch: {
      auto const stmt = static_cast<ast::Stmt_Switch const*>(generic_stmt);
      lower_stmt_switch(ctx, builder, stmt);
    } break;

    default:
      ANTON_UNREACHABLE("unhandled statement node kind");
    }
  }

  static void lower_function(Lowering_Context& ctx,
                             ast::Decl_Function const* const ast_fn,
                             ir::Function* const fn)
  {
    ctx.current_function_return_type = ast_fn->return_type;
    ctx.symtable.push_scope();
    Builder builder;
    builder.set_insert_block(fn->entry_block);
    // Generate arguments and their respective allocs.
    // TODO: Unsized array parametrs.
    for(ast::Fn_Parameter const* const parameter: ast_fn->parameters) {
      ir::Type* const type = convert_ast_to_ir_type(ctx, parameter->type);
      auto const argument =
        VUSH_ALLOCATE(ir::Argument, ctx.allocator, type, fn, ctx.allocator);
      fn->arguments.insert_back(*argument);
      auto const alloc = ir::make_instr_alloc(ctx.allocator, ctx.get_next_id(),
                                              type, parameter->source_info);
      builder.insert(alloc);
      auto const store =
        ir::make_instr_store(ctx.allocator, ctx.get_next_id(), alloc, argument,
                             parameter->identifier.source_info);
      builder.insert(store);
      ctx.symtable.add_entry(parameter->identifier.value, alloc);
    }

    lower_statement_block(ctx, builder, ast_fn->body);
    ctx.symtable.pop_scope();
  }

  [[nodiscard]] ir::Storage_Class
  select_storage_class(ast::Fn_Parameter const* const parameter,
                       bool const first)
  {
    if(first && !ast::is_sourced_parameter(*parameter)) {
      return ir::Storage_Class::e_input;
    }

    if(ast::is_vertex_input_parameter(*parameter)) {
      return ir::Storage_Class::e_input;
    }

    if(parameter->buffer != nullptr) {
      auto const buffer = parameter->buffer;
      if(ast::is_uniform(buffer)) {
        return ir::Storage_Class::e_uniform;
      } else if(ast::is_push_constant(buffer)) {
        return ir::Storage_Class::e_push_constant;
      } else {
        return ir::Storage_Class::e_buffer;
      }
    }

    return ir::Storage_Class::e_automatic;
  }

  [[nodiscard]] ir::Buffer* lower_buffer(Lowering_Context& ctx,
                                         ast::Decl_Buffer const* const buffer)
  {
    auto const composite =
      VUSH_ALLOCATE(ir::Type_Composite, ctx.allocator, ctx.allocator,
                    buffer->identifier.value);
    composite->elements.ensure_capacity(buffer->fields.size());
    for(ast::Buffer_Field const* const field: buffer->fields) {
      ir::Type* const type = convert_ast_to_ir_type(ctx, field->type);
      composite->elements.push_back(type);
    }

    auto const result =
      VUSH_ALLOCATE(ir::Buffer, ctx.allocator, nullptr,
                    anton::String(buffer->identifier.value, ctx.allocator),
                    buffer->source_info);
    return result;
  }

  [[nodiscard]] static ir::Module
  lower_module(Lowering_Context& ctx,
               ast::Decl_Stage_Function const* const stage)
  {
    auto const entry_block =
      VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
    auto const fn = VUSH_ALLOCATE(
      ir::Function, ctx.allocator, ctx.get_next_id(), entry_block,
      anton::String("main"_sv, ctx.allocator), stage->source_info);
    ctx.current_function_return_type = stage->return_type;
    ctx.symtable.push_scope();
    Builder builder;
    builder.set_insert_block(fn->entry_block);
    // Generate allocas for the parameters.
    for(bool first = true;
        ast::Fn_Parameter const* const parameter: stage->parameters) {
      ir::Type* const type = convert_ast_to_ir_type(ctx, parameter->type);
      auto const argument =
        VUSH_ALLOCATE(ir::Argument, ctx.allocator, type, fn, ctx.allocator);
      argument->storage_class = select_storage_class(parameter, first);
      argument->buffer = lower_buffer(ctx, parameter->buffer);
      fn->arguments.insert_back(*argument);
      auto const alloc = ir::make_instr_alloc(ctx.allocator, ctx.get_next_id(),
                                              type, parameter->source_info);
      builder.insert(alloc);
      auto const store =
        ir::make_instr_store(ctx.allocator, ctx.get_next_id(), alloc, argument,
                             parameter->identifier.source_info);
      builder.insert(store);
      ctx.symtable.add_entry(parameter->identifier.value, alloc);

      first = false;
    }

    lower_statement_block(ctx, builder, stage->body);
    ctx.symtable.pop_scope();
    // TODO: transform module return to output variable.
    return ir::Module(anton::String(stage->pass.value, ctx.allocator),
                      stage->stage.value, fn);
  }

  Array<ir::Module> lower_ast_to_ir(Allocator* const allocator,
                                    ast::Node_List const ast)
  {
    Array<ir::Module> modules{allocator};
    Lowering_Context ctx{allocator};
    for(ast::Node const* const node: ast) {
      if(node->node_kind == ast::Node_Kind::decl_function) {
        auto const ast_fn = static_cast<ast::Decl_Function const*>(node);
        auto const entry_block =
          VUSH_ALLOCATE(ir::Basic_Block, ctx.allocator, ctx.get_next_id());
        anton::String identifier{ast_fn->identifier.value, ctx.allocator};
        auto const ir_fn =
          VUSH_ALLOCATE(ir::Function, allocator, ctx.get_next_id(), entry_block,
                        ANTON_MOV(identifier), ast_fn->source_info);
        ctx.fntable.add_entry(ir_fn->identifier, ir_fn);
      }
    }

    for(ast::Node const* const node: ast) {
      if(node->node_kind == ast::Node_Kind::decl_stage_function) {
        auto const stage = static_cast<ast::Decl_Stage_Function const*>(node);
        ir::Module module = lower_module(ctx, stage);
        modules.push_back(ANTON_MOV(module));
      }

      if(node->node_kind == ast::Node_Kind::decl_function) {
        auto const ast_fn = static_cast<ast::Decl_Function const*>(node);
        auto const fn = *ctx.fntable.find_entry(ast_fn->identifier.value);
        lower_function(ctx, ast_fn, fn);
      }
    }
    return modules;
  }
} // namespace vush
