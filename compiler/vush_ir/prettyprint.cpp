#include <vush_ir/prettyprint.hpp>

#include <anton/flat_hash_set.hpp>
#include <anton/format.hpp>
#include <anton/math/math.hpp>

#include <vush_ir/ir.hpp>

namespace vush::ir {
  using namespace anton::literals;

  struct Printer {
  private:
    anton::Output_Stream* stream;
    i32 indent_width = 0;
    i32 current_indent = 0;

  public:
    Printer(anton::Output_Stream& stream, i32 indent_width)
      : stream(&stream), indent_width(indent_width)
    {
    }

    void write(anton::String_View const string)
    {
      stream->write(string);
    }

    void indent()
    {
      for(i32 i = 0; i < current_indent; i += 1) {
        stream->write(" "_sv);
      }
    }

    void inc_indent()
    {
      current_indent += indent_width;
    }

    void dec_indent()
    {
      current_indent = anton::math::max(current_indent - indent_width, (i32)0);
    }

    void set_indent_level(i32 const level)
    {
      current_indent = anton::math::max(level * indent_width, (i32)0);
    }
  };

  static void print_source_location(Allocator* const allocator,
                                    Printer& printer, Source_Info const& source)
  {
    printer.write(source.source_path);
    printer.write(":"_sv);
    printer.write(anton::to_string(allocator, source.line));
    printer.write(":"_sv);
    printer.write(anton::to_string(allocator, source.column));
  }

  static void print_block_id(Allocator* const allocator, Printer& printer,
                             Basic_Block const* const block)
  {
    printer.write(anton::format(allocator, "B_{}"_sv, block->id));
  }

  [[nodiscard]] static anton::String_View
  scalar_type_to_string(Type_Kind const kind)
  {
    switch(kind) {
    case Type_Kind::e_bool:
      return "bool"_sv;
    case Type_Kind::e_int8:
      return "i8"_sv;
    case Type_Kind::e_int16:
      return "i16"_sv;
    case Type_Kind::e_int32:
      return "i32"_sv;
    case Type_Kind::e_uint8:
      return "u8"_sv;
    case Type_Kind::e_uint16:
      return "u16"_sv;
    case Type_Kind::e_uint32:
      return "u32"_sv;
    case Type_Kind::e_fp16:
      return "f16"_sv;
    case Type_Kind::e_fp32:
      return "f32"_sv;
    case Type_Kind::e_fp64:
      return "f64"_sv;
    default:
      ANTON_UNREACHABLE("type is not scalar");
    }
  }

  static void print_type_inline(Allocator* const allocator, Printer& printer,
                                Prettyprint_Options const& options,
                                Type const* const type)
  {
    switch(type->kind) {
    case Type_Kind::e_void:
      printer.write("void"_sv);
      break;

    case Type_Kind::e_bool:
    case Type_Kind::e_int8:
    case Type_Kind::e_int16:
    case Type_Kind::e_int32:
    case Type_Kind::e_uint8:
    case Type_Kind::e_uint16:
    case Type_Kind::e_uint32:
    case Type_Kind::e_fp16:
    case Type_Kind::e_fp32:
    case Type_Kind::e_fp64:
      printer.write(scalar_type_to_string(type->kind));
      break;

    case Type_Kind::e_ptr:
      printer.write("ptr"_sv);
      break;

    case Type_Kind::e_vec: {
      auto const vec = static_cast<ir::Type_Vec const*>(type);
      printer.write(
        anton::format(allocator, "<{} x {}>"_sv, vec->rows,
                      scalar_type_to_string(vec->element_type->kind)));
    } break;

    case Type_Kind::e_mat: {
      auto const mat = static_cast<ir::Type_Mat const*>(type);
      printer.write(anton::format(
        allocator, "<{} x <{} x {}>>"_sv, mat->columns, mat->column_type->rows,
        scalar_type_to_string(mat->column_type->element_type->kind)));
    } break;

    case Type_Kind::e_sampler:
    case Type_Kind::e_image:
    case Type_Kind::e_texture:
      // TODO: Print sampler types.
      printer.write("<type>"_sv);
      break;

    case Type_Kind::e_composite: {
      auto const composite = static_cast<ir::Type_Composite const*>(type);
      printer.write("{ "_sv);
      for(bool first = true; Type const* const member: composite->elements) {
        if(!first) {
          printer.write(", ");
        }
        print_type_inline(allocator, printer, options, member);
        first = false;
      }
      printer.write(" }"_sv);
    } break;

    case Type_Kind::e_array: {
      auto const array = static_cast<ir::Type_Array const*>(type);
      printer.write("["_sv);
      print_type_inline(allocator, printer, options, array->element_type);
      printer.write("; "_sv);
      if(array->size >= 0) {
        printer.write(anton::to_string(allocator, array->size));
      }
      printer.write("]"_sv);
    } break;
    }
  }

  static void print_constant(Allocator* const allocator, Printer& printer,
                             Prettyprint_Options const& options,
                             Constant const* const gconstant)
  {
    switch(gconstant->constant_kind) {
    case Constant_Kind::e_constant_bool: {
      auto const value = static_cast<Constant_bool const*>(gconstant);
      if(value->value) {
        printer.write("%true"_sv);
      } else {
        printer.write("%false"_sv);
      }
    } break;

    case Constant_Kind::e_constant_i32: {
      auto const value = static_cast<Constant_i32 const*>(gconstant);
      printer.write(anton::format(allocator, "%i32_{}"_sv, value->value));
    } break;

    case Constant_Kind::e_constant_u32: {
      auto const value = static_cast<Constant_u32 const*>(gconstant);
      printer.write(anton::format(allocator, "%u32_{}"_sv, value->value));
    } break;

    case Constant_Kind::e_constant_f32: {
      auto const value = static_cast<Constant_f32 const*>(gconstant);
      printer.write(anton::format(allocator, "%f32_{}"_sv, value->value));
    } break;

    case Constant_Kind::e_constant_f64: {
      auto const value = static_cast<Constant_f64 const*>(gconstant);
      printer.write(anton::format(allocator, "%f64_{}"_sv, value->value));
    } break;

    case Constant_Kind::e_undef: {
      printer.write("%undef"_sv);
    } break;
    }
  }

  static void print_value(Allocator* const allocator, Printer& printer,
                          Prettyprint_Options const& options,
                          Value const* const value)
  {
    switch(value->value_kind) {
    case Value_Kind::e_instr: {
      auto const instr = static_cast<Instr const*>(value);
      printer.write(anton::format(allocator, "%{}"_sv, instr->id));
    } break;

    case Value_Kind::e_const: {
      auto const constant = static_cast<Constant const*>(value);
      print_constant(allocator, printer, options, constant);
    } break;

    case Value_Kind::e_argument: {
      auto const argument = static_cast<Argument const*>(value);
      printer.write(anton::format(allocator, "%{}"_sv, argument->id));
    } break;
    }
  }

  [[nodiscard]] static anton::String_View stringify(ALU_Opcode op)
  {
    switch(op) {
    case ALU_Opcode::e_inv:
      return "inv"_sv;
    case ALU_Opcode::e_and:
      return "and"_sv;
    case ALU_Opcode::e_or:
      return "or"_sv;
    case ALU_Opcode::e_xor:
      return "xor"_sv;
    case ALU_Opcode::e_shl:
      return "shl"_sv;
    case ALU_Opcode::e_shr:
      return "shr"_sv;
    case ALU_Opcode::e_neg:
      return "neg"_sv;
    case ALU_Opcode::e_iadd:
      return "iadd"_sv;
    case ALU_Opcode::e_imul:
      return "imul"_sv;
    case ALU_Opcode::e_uadd:
      return "uadd"_sv;
    case ALU_Opcode::e_umul:
      return "umul"_sv;
    case ALU_Opcode::e_idiv:
      return "idiv"_sv;
    case ALU_Opcode::e_udiv:
      return "udiv"_sv;
    case ALU_Opcode::e_irem:
      return "irem"_sv;
    case ALU_Opcode::e_urem:
      return "urem"_sv;
    case ALU_Opcode::e_fneg:
      return "fneg"_sv;
    case ALU_Opcode::e_fadd:
      return "fadd"_sv;
    case ALU_Opcode::e_fmul:
      return "fmul"_sv;
    case ALU_Opcode::e_fdiv:
      return "fdiv"_sv;
    case ALU_Opcode::e_fma:
      return "fma"_sv;
    case ALU_Opcode::e_icmp_eq:
      return "icmp_eq"_sv;
    case ALU_Opcode::e_icmp_neq:
      return "icmp_neq"_sv;
    case ALU_Opcode::e_icmp_ugt:
      return "icmp_ugt"_sv;
    case ALU_Opcode::e_icmp_ult:
      return "icmp_ult"_sv;
    case ALU_Opcode::e_icmp_uge:
      return "icmp_uge"_sv;
    case ALU_Opcode::e_icmp_ule:
      return "icmp_ule"_sv;
    case ALU_Opcode::e_icmp_sgt:
      return "icmp_sgt"_sv;
    case ALU_Opcode::e_icmp_slt:
      return "icmp_slt"_sv;
    case ALU_Opcode::e_icmp_sge:
      return "icmp_sge"_sv;
    case ALU_Opcode::e_icmp_sle:
      return "icmp_sle"_sv;
    case ALU_Opcode::e_fcmp_eq:
      return "fcmp_eq"_sv;
    case ALU_Opcode::e_fcmp_neq:
      return "fcmp_neq"_sv;
    case ALU_Opcode::e_fcmp_gt:
      return "fcmp_gt"_sv;
    case ALU_Opcode::e_fcmp_lt:
      return "fcmp_lt"_sv;
    case ALU_Opcode::e_fcmp_ge:
      return "fcmp_ge"_sv;
    case ALU_Opcode::e_fcmp_le:
      return "fcmp_le"_sv;
    }
  }

  [[nodiscard]] static anton::String_View stringify(Ext_Kind kind)
  {
    switch(kind) {
    case Ext_Kind::e_round:
      return "round"_sv;
    case Ext_Kind::e_round_even:
      return "round_even"_sv;
    case Ext_Kind::e_trunc:
      return "trunc"_sv;
    case Ext_Kind::e_iabs:
      return "iabs"_sv;
    case Ext_Kind::e_fabs:
      return "fabs"_sv;
    case Ext_Kind::e_isign:
      return "isign"_sv;
    case Ext_Kind::e_fsign:
      return "fsign"_sv;
    case Ext_Kind::e_floor:
      return "floor"_sv;
    case Ext_Kind::e_ceil:
      return "ceil"_sv;
    case Ext_Kind::e_fract:
      return "fract"_sv;
    case Ext_Kind::e_fmod:
      return "fmod"_sv;
    case Ext_Kind::e_imin:
      return "imin"_sv;
    case Ext_Kind::e_umin:
      return "umin"_sv;
    case Ext_Kind::e_fmin:
      return "fmin"_sv;
    case Ext_Kind::e_imax:
      return "imax"_sv;
    case Ext_Kind::e_umax:
      return "umax"_sv;
    case Ext_Kind::e_fmax:
      return "fmax"_sv;
    case Ext_Kind::e_iclamp:
      return "iclamp"_sv;
    case Ext_Kind::e_uclamp:
      return "uclamp"_sv;
    case Ext_Kind::e_fclamp:
      return "fclamp"_sv;
    case Ext_Kind::e_radians:
      return "radians"_sv;
    case Ext_Kind::e_degrees:
      return "degrees"_sv;
    case Ext_Kind::e_fmix:
      return "fmix"_sv;
    case Ext_Kind::e_step:
      return "step"_sv;
    case Ext_Kind::e_smooth_step:
      return "smooth_step"_sv;
    case Ext_Kind::e_fma:
      return "fma"_sv;
    case Ext_Kind::e_sin:
      return "sin"_sv;
    case Ext_Kind::e_cos:
      return "cos"_sv;
    case Ext_Kind::e_tan:
      return "tan"_sv;
    case Ext_Kind::e_asin:
      return "asin"_sv;
    case Ext_Kind::e_acos:
      return "acos"_sv;
    case Ext_Kind::e_atan:
      return "atan"_sv;
    case Ext_Kind::e_atan2:
      return "atan2"_sv;
    case Ext_Kind::e_sinh:
      return "sinh"_sv;
    case Ext_Kind::e_cosh:
      return "cosh"_sv;
    case Ext_Kind::e_tanh:
      return "tanh"_sv;
    case Ext_Kind::e_asinh:
      return "asinh"_sv;
    case Ext_Kind::e_acosh:
      return "acosh"_sv;
    case Ext_Kind::e_atanh:
      return "atanh"_sv;
    case Ext_Kind::e_pow:
      return "pow"_sv;
    case Ext_Kind::e_exp:
      return "exp"_sv;
    case Ext_Kind::e_exp2:
      return "exp2"_sv;
    case Ext_Kind::e_log:
      return "log"_sv;
    case Ext_Kind::e_log2:
      return "log2"_sv;
    case Ext_Kind::e_sqrt:
      return "sqrt"_sv;
    case Ext_Kind::e_inv_sqrt:
      return "inv_sqrt"_sv;
    case Ext_Kind::e_length:
      return "length"_sv;
    case Ext_Kind::e_distance:
      return "distance"_sv;
    case Ext_Kind::e_dot:
      return "dot"_sv;
    case Ext_Kind::e_cross:
      return "cross"_sv;
    case Ext_Kind::e_normalize:
      return "normalize"_sv;
    case Ext_Kind::e_faceforward:
      return "faceforward"_sv;
    case Ext_Kind::e_reflect:
      return "reflect"_sv;
    case Ext_Kind::e_refract:
      return "refract"_sv;
    case Ext_Kind::e_mat_comp_mult:
      return "mat_comp_mult"_sv;
    case Ext_Kind::e_outer_product:
      return "outer_product"_sv;
    case Ext_Kind::e_transpose:
      return "transpose"_sv;
    case Ext_Kind::e_mat_det:
      return "mat_det"_sv;
    case Ext_Kind::e_mat_inv:
      return "mat_inv"_sv;
    case Ext_Kind::e_tex_size:
      return "tex_size"_sv;
    case Ext_Kind::e_tex_query_lod:
      return "tex_query_lod"_sv;
    case Ext_Kind::e_tex_query_levels:
      return "tex_query_levels"_sv;
    case Ext_Kind::e_tex_samples:
      return "tex_samples"_sv;
    case Ext_Kind::e_tex:
      return "tex"_sv;
    case Ext_Kind::e_tex_lod:
      return "tex_lod"_sv;
    case Ext_Kind::e_tex_proj:
      return "tex_proj"_sv;
    case Ext_Kind::e_tex_off:
      return "tex_off"_sv;
    case Ext_Kind::e_texel_fetch:
      return "texel_fetch"_sv;
    case Ext_Kind::e_texel_fetch_off:
      return "texel_fetch_off"_sv;
    case Ext_Kind::e_tex_lod_off:
      return "tex_lod_off"_sv;
    case Ext_Kind::e_tex_proj_lod_off:
      return "tex_proj_lod_off"_sv;
    case Ext_Kind::e_tex_proj_lod:
      return "tex_proj_lod"_sv;
    case Ext_Kind::e_tex_proj_off:
      return "tex_proj_off"_sv;
    case Ext_Kind::e_tex_grad:
      return "tex_grad"_sv;
    case Ext_Kind::e_tex_grad_off:
      return "tex_grad_off"_sv;
    case Ext_Kind::e_tex_proj_grad:
      return "tex_proj_grad"_sv;
    case Ext_Kind::e_tex_proj_grad_off:
      return "tex_proj_grad_off"_sv;
    case Ext_Kind::e_tex_gather:
      return "tex_gather"_sv;
    case Ext_Kind::e_tex_gather_off:
      return "tex_gather_off"_sv;
    case Ext_Kind::e_tex_gather_offs:
      return "tex_gather_offs"_sv;
    }
  }

  static void print_intrinsic(Allocator* const allocator, Printer& printer,
                              Prettyprint_Options const& options,
                              Instr_intrinsic const* const instr)
  {
    switch(instr->intrinsic_kind) {
    case Intrinsic_Kind::e_scf_branch_head: {
      auto const intrinsic =
        static_cast<Intrinsic_scf_branch_head const*>(instr);
      printer.write("@vush.scf_branch_head "_sv);
      print_block_id(allocator, printer, intrinsic->converge_block);
    } break;
    }
  }

  static void print_instr(Allocator* const allocator, Printer& printer,
                          Prettyprint_Options const& options,
                          Instr const* const generic_instr)
  {
    printer.set_indent_level(options.instruction_indent_level);
    printer.indent();
    switch(generic_instr->instr_kind) {
    case Instr_Kind::e_intrinsic: {
      auto const instr = static_cast<Instr_intrinsic const*>(generic_instr);
      print_intrinsic(allocator, printer, options, instr);
    } break;

    case Instr_Kind::e_alloc: {
      auto const instr = static_cast<Instr_alloc const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = alloc "_sv);
      print_type_inline(allocator, printer, options, instr->alloc_type);
    } break;

    case Instr_Kind::e_load: {
      auto const instr = static_cast<Instr_load const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = load "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->address);
    } break;

    case Instr_Kind::e_store: {
      auto const instr = static_cast<Instr_store const*>(generic_instr);
      printer.write("store "_sv);
      print_value(allocator, printer, options, instr->dst);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->src);
    } break;

    case Instr_Kind::e_getptr: {
      auto const instr = static_cast<Instr_getptr const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = getptr "_sv);
      print_value(allocator, printer, options, instr->address);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->index);
    } break;

    case Instr_Kind::e_alu: {
      auto const instr = static_cast<Instr_ALU const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = "_sv);
      printer.write(stringify(instr->op));
      printer.write(" "_sv);
      print_value(allocator, printer, options, instr->src1);
      if(instr->src2) {
        printer.write(", "_sv);
        print_value(allocator, printer, options, instr->src2);
      }
    } break;

    case Instr_Kind::e_branch: {
      auto const instr = static_cast<Instr_branch const*>(generic_instr);
      printer.write("branch "_sv);
      print_block_id(allocator, printer, instr->target);
    } break;

    case Instr_Kind::e_brcond: {
      auto const instr = static_cast<Instr_brcond const*>(generic_instr);
      printer.write("brcond "_sv);
      print_value(allocator, printer, options, instr->condition);
      printer.write(", "_sv);
      print_block_id(allocator, printer, instr->then_target);
      printer.write(", "_sv);
      print_block_id(allocator, printer, instr->else_target);
    } break;

    case Instr_Kind::e_phi: {
      auto const instr = static_cast<Instr_phi const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write("= phi "_sv);
      for(bool first = true; Value const* const src: instr->srcs) {
        if(first) {
          printer.write(" "_sv);
        } else {
          printer.write(", "_sv);
        }
        print_value(allocator, printer, options, src);
      }
    } break;

    case Instr_Kind::e_switch: {
      auto const instr = static_cast<Instr_switch const*>(generic_instr);
      printer.write("switch "_sv);
      print_value(allocator, printer, options, instr->selector);
      printer.write(" "_sv);
      print_block_id(allocator, printer, instr->default_label);
      printer.write(" {\n"_sv);
      printer.inc_indent();
      for(auto const label: instr->labels) {
        printer.indent();
        printer.write(anton::format(allocator, "{} -> ", label.value));
        print_block_id(allocator, printer, label.target);
        printer.write("\n"_sv);
      }
      printer.dec_indent();
      printer.indent();
      printer.write("}"_sv);
    } break;

    case Instr_Kind::e_vector_extract: {
      auto const instr =
        static_cast<Instr_vector_extract const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = vector_extract "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
      printer.write(anton::format(allocator, ", {}"_sv, instr->index));
    } break;

    case Instr_Kind::e_vector_insert: {
      auto const instr = static_cast<Instr_vector_insert const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = vector_insert"_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
      printer.write(anton::format(allocator, ", {}"_sv, instr->index));
    } break;

    case Instr_Kind::e_composite_extract: {
      auto const instr =
        static_cast<Instr_composite_extract const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = composite_extract "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
      for(auto const index: instr->indices) {
        printer.write(anton::format(allocator, ", {}"_sv, index));
      }
    } break;

    case Instr_Kind::e_composite_construct: {
      auto const instr =
        static_cast<Instr_composite_construct const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = composite_construct "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      for(auto const element: instr->elements) {
        printer.write(", "_sv);
        print_value(allocator, printer, options, element);
      }
    } break;

    case Instr_Kind::e_cvt_sext: {
      auto const instr = static_cast<Instr_cvt_sext const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_sext "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_zext: {
      auto const instr = static_cast<Instr_cvt_zext const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_zext "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_trunc: {
      auto const instr = static_cast<Instr_cvt_trunc const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_trunc "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_fpext: {
      auto const instr = static_cast<Instr_cvt_fpext const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_fpext "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_fptrunc: {
      auto const instr = static_cast<Instr_cvt_fptrunc const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_fptrunc "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_si2fp: {
      auto const instr = static_cast<Instr_cvt_si2fp const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_si2fp "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_fp2si: {
      auto const instr = static_cast<Instr_cvt_fp2si const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_fp2si "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_ui2fp: {
      auto const instr = static_cast<Instr_cvt_ui2fp const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_ui2fp "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_fp2ui: {
      auto const instr = static_cast<Instr_cvt_fp2ui const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_fp2ui "_sv);
      print_type_inline(allocator, printer, options, instr->type);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_call: {
      auto const instr = static_cast<Instr_call const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(anton::format(allocator, " = call {} {}"_sv,
                                  instr->function->identifier,
                                  instr->function->id));
      for(auto const arg: instr->args) {
        printer.write(", "_sv);
        print_value(allocator, printer, options, arg);
      }
    } break;

    case Instr_Kind::e_ext_call: {
      auto const instr = static_cast<Instr_ext_call const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(anton::format(" = ext_call {}"_sv, stringify(instr->ext)));
      for(auto const arg: instr->args) {
        printer.write(", "_sv);
        print_value(allocator, printer, options, arg);
      }
    } break;

    case Instr_Kind::e_return: {
      auto const instr = static_cast<Instr_return const*>(generic_instr);
      printer.write("return ");
      if(instr->value) {
        print_value(allocator, printer, options, instr->value);
      }
    } break;

    case Instr_Kind::e_die: {
      printer.write("die");
    } break;
    }

    if(options.instruction_location) {
      printer.write(" @ "_sv);
      print_source_location(allocator, printer, generic_instr->source_info);
    }
    printer.write("\n"_sv);
  }

  using Block_Set = anton::Flat_Hash_Set<ir::Basic_Block const*>;

  static void print_block(Allocator* const allocator, Printer& printer,
                          Prettyprint_Options const& options,
                          Block_Set& visited_blocks,
                          Basic_Block const* const block)
  {
    // Ensure we do not print duplicates.
    {
      auto const iterator = visited_blocks.find(block);
      if(iterator != visited_blocks.end()) {
        return;
      }

      visited_blocks.emplace(block);
    }

    printer.set_indent_level(options.block_indent_level);
    printer.indent();
    print_block_id(allocator, printer, block);
    printer.write(":\n"_sv);
    for(Instr const& instr: block->instructions) {
      print_instr(allocator, printer, options, &instr);
    }

    auto const last_instr = block->get_last();
    if(instanceof<Instr_branch>(last_instr)) {
      auto const instr = static_cast<Instr_branch const*>(last_instr);
      print_block(allocator, printer, options, visited_blocks, instr->target);
    } else if(instanceof<Instr_brcond>(last_instr)) {
      auto const instr = static_cast<Instr_brcond const*>(last_instr);
      print_block(allocator, printer, options, visited_blocks,
                  instr->then_target);
      print_block(allocator, printer, options, visited_blocks,
                  instr->else_target);
    } else if(instanceof<Instr_switch>(last_instr)) {
      auto const instr = static_cast<Instr_switch const*>(last_instr);
      for(auto const label: instr->labels) {
        print_block(allocator, printer, options, visited_blocks, label.target);
      }
    }
  }

  static void print_function(Allocator* const allocator, Printer& printer,
                             Prettyprint_Options const& options,
                             Function const* const function)
  {
    printer.write(function->identifier);
    printer.write("("_sv);
    for(bool first = true; Argument const& argument: function->arguments) {
      if(!first) {
        printer.write(", "_sv);
      }
      print_value(allocator, printer, options, &argument);
      if(argument.storage_class == Storage_Class::e_input) {
        printer.write(" "_sv);
        print_type_inline(allocator, printer, options, argument.pointee_type);
        printer.write(" input"_sv);
      } else if(argument.storage_class == Storage_Class::e_output) {
        printer.write(" "_sv);
        print_type_inline(allocator, printer, options, argument.pointee_type);
        printer.write(" output"_sv);
      } else if(argument.buffer != nullptr) {
        printer.write(" "_sv);
        print_type_inline(allocator, printer, options, argument.pointee_type);
        printer.write(argument.buffer->identifier);
      }
      first = false;
    }
    printer.write(") -> "_sv);
    print_type_inline(allocator, printer, options, function->return_type);

    if(options.function_location) {
      printer.write(" @ "_sv);
      print_source_location(allocator, printer, function->source_info);
    }

    printer.write("\n"_sv);
    Block_Set visited_blocks(allocator);
    print_block(allocator, printer, options, visited_blocks,
                function->entry_block);
  }

  void prettyprint(Allocator* allocator, anton::Output_Stream& stream,
                   Prettyprint_Options const& options, Module const& module)
  {
    Printer printer(stream, options.indent_width);
    printer.write(module.pass_identifier);
    printer.write(" "_sv);
    switch(module.stage) {
    case Stage_Kind::compute:
      printer.write("compute"_sv);
      break;
    case Stage_Kind::vertex:
      printer.write("vertex"_sv);
      break;
    case Stage_Kind::fragment:
      printer.write("fragment"_sv);
      break;
    }
    printer.write(" "_sv);
    print_function(allocator, printer, options, module.entry);
  }
} // namespace vush::ir
