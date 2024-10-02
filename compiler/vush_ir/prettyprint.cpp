#include <vush_ir/prettyprint.hpp>

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

  static void print_constant(Allocator* const allocator, Printer& printer,
                             Prettyprint_Options const& options,
                             Constant const* const generic_constant)
  {
    printer.write("%const"_sv);
  }

  static void print_value(Allocator* const allocator, Printer& printer,
                          Prettyprint_Options const& options,
                          Value const* const value)
  {
    if(instanceof<Instr>(value)) {
      auto const instr = static_cast<Instr const*>(value);
      printer.write(anton::format(allocator, "%{}"_sv, instr->id));
    } else if(instanceof<Constant>(value)) {
      auto const constant = static_cast<Constant const*>(value);
      print_constant(allocator, printer, options, constant);
    } else {
      printer.write("%unknown"_sv);
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
      return "round";
    case Ext_Kind::e_round_even:
      return "round_even";
    case Ext_Kind::e_trunc:
      return "trunc";
    case Ext_Kind::e_iabs:
      return "iabs";
    case Ext_Kind::e_fabs:
      return "fabs";
    case Ext_Kind::e_isign:
      return "isign";
    case Ext_Kind::e_fsign:
      return "fsign";
    case Ext_Kind::e_floor:
      return "floor";
    case Ext_Kind::e_ceil:
      return "ceil";
    case Ext_Kind::e_fract:
      return "fract";
    case Ext_Kind::e_fmod:
      return "fmod";
    case Ext_Kind::e_imin:
      return "imin";
    case Ext_Kind::e_umin:
      return "umin";
    case Ext_Kind::e_fmin:
      return "fmin";
    case Ext_Kind::e_imax:
      return "imax";
    case Ext_Kind::e_umax:
      return "umax";
    case Ext_Kind::e_fmax:
      return "fmax";
    case Ext_Kind::e_iclamp:
      return "iclamp";
    case Ext_Kind::e_uclamp:
      return "uclamp";
    case Ext_Kind::e_fclamp:
      return "fclamp";
    case Ext_Kind::e_radians:
      return "radians";
    case Ext_Kind::e_degrees:
      return "degrees";
    case Ext_Kind::e_fmix:
      return "fmix";
    case Ext_Kind::e_step:
      return "step";
    case Ext_Kind::e_smooth_step:
      return "smooth_step";
    case Ext_Kind::e_fma:
      return "fma";
    case Ext_Kind::e_sin:
      return "sin";
    case Ext_Kind::e_cos:
      return "cos";
    case Ext_Kind::e_tan:
      return "tan";
    case Ext_Kind::e_asin:
      return "asin";
    case Ext_Kind::e_acos:
      return "acos";
    case Ext_Kind::e_atan:
      return "atan";
    case Ext_Kind::e_atan2:
      return "atan2";
    case Ext_Kind::e_sinh:
      return "sinh";
    case Ext_Kind::e_cosh:
      return "cosh";
    case Ext_Kind::e_tanh:
      return "tanh";
    case Ext_Kind::e_asinh:
      return "asinh";
    case Ext_Kind::e_acosh:
      return "acosh";
    case Ext_Kind::e_atanh:
      return "atanh";
    case Ext_Kind::e_pow:
      return "pow";
    case Ext_Kind::e_exp:
      return "exp";
    case Ext_Kind::e_exp2:
      return "exp2";
    case Ext_Kind::e_log:
      return "log";
    case Ext_Kind::e_log2:
      return "log2";
    case Ext_Kind::e_sqrt:
      return "sqrt";
    case Ext_Kind::e_inv_sqrt:
      return "inv_sqrt";
    case Ext_Kind::e_length:
      return "length";
    case Ext_Kind::e_distance:
      return "distance";
    case Ext_Kind::e_dot:
      return "dot";
    case Ext_Kind::e_cross:
      return "cross";
    case Ext_Kind::e_normalize:
      return "normalize";
    case Ext_Kind::e_faceforward:
      return "faceforward";
    case Ext_Kind::e_reflect:
      return "reflect";
    case Ext_Kind::e_refract:
      return "refract";
    case Ext_Kind::e_mat_comp_mult:
      return "mat_comp_mult";
    case Ext_Kind::e_outer_product:
      return "outer_product";
    case Ext_Kind::e_transpose:
      return "transpose";
    case Ext_Kind::e_mat_det:
      return "mat_det";
    case Ext_Kind::e_mat_inv:
      return "mat_inv";
    case Ext_Kind::e_tex_size:
      return "tex_size";
    case Ext_Kind::e_tex_query_lod:
      return "tex_query_lod";
    case Ext_Kind::e_tex_query_levels:
      return "tex_query_levels";
    case Ext_Kind::e_tex_samples:
      return "tex_samples";
    case Ext_Kind::e_tex:
      return "tex";
    case Ext_Kind::e_tex_lod:
      return "tex_lod";
    case Ext_Kind::e_tex_proj:
      return "tex_proj";
    case Ext_Kind::e_tex_off:
      return "tex_off";
    case Ext_Kind::e_texel_fetch:
      return "texel_fetch";
    case Ext_Kind::e_texel_fetch_off:
      return "texel_fetch_off";
    case Ext_Kind::e_tex_lod_off:
      return "tex_lod_off";
    case Ext_Kind::e_tex_proj_lod_off:
      return "tex_proj_lod_off";
    case Ext_Kind::e_tex_proj_lod:
      return "tex_proj_lod";
    case Ext_Kind::e_tex_proj_off:
      return "tex_proj_off";
    case Ext_Kind::e_tex_grad:
      return "tex_grad";
    case Ext_Kind::e_tex_grad_off:
      return "tex_grad_off";
    case Ext_Kind::e_tex_proj_grad:
      return "tex_proj_grad";
    case Ext_Kind::e_tex_proj_grad_off:
      return "tex_proj_grad_off";
    case Ext_Kind::e_tex_gather:
      return "tex_gather";
    case Ext_Kind::e_tex_gather_off:
      return "tex_gather_off";
    case Ext_Kind::e_tex_gather_offs:
      return "tex_gather_offs";
    }
  }

  static void print_instr(Allocator* const allocator, Printer& printer,
                          Prettyprint_Options const& options,
                          Instr const* const generic_instr)
  {
    printer.set_indent_level(options.instruction_indent_level);
    printer.indent();
    switch(generic_instr->instr_kind) {
    case Instr_Kind::e_alloc: {
      auto const instr = static_cast<Instr_alloc const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = alloc <type>"_sv);
    } break;

    case Instr_Kind::e_load: {
      auto const instr = static_cast<Instr_load const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = load <type>, "_sv);
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
      // TODO: Print type.
      printer.write("<type>, "_sv);
      print_value(allocator, printer, options, instr->value);
      printer.write(anton::format(allocator, ", {}"_sv, instr->index));
    } break;

    case Instr_Kind::e_vector_insert: {
      auto const instr = static_cast<Instr_vector_insert const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = vector_insert"_sv);
      // TODO: Print type.
      printer.write("<type>, "_sv);
      print_value(allocator, printer, options, instr->value);
      printer.write(anton::format(allocator, ", {}"_sv, instr->index));
    } break;

    case Instr_Kind::e_composite_extract: {
      auto const instr =
        static_cast<Instr_composite_extract const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = composite_extract "_sv);
      // TODO: Print type.
      printer.write("<type>, "_sv);
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
      // TODO: Print type.
      printer.write("<type>"_sv);
      for(auto const element: instr->elements) {
        printer.write(", "_sv);
        print_value(allocator, printer, options, element);
      }
    } break;

    case Instr_Kind::e_cvt_sext: {
      auto const instr = static_cast<Instr_cvt_sext const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_sext "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_zext: {
      auto const instr = static_cast<Instr_cvt_zext const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_zext "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_trunc: {
      auto const instr = static_cast<Instr_cvt_trunc const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_trunc "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_fpext: {
      auto const instr = static_cast<Instr_cvt_fpext const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_fpext "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_fptrunc: {
      auto const instr = static_cast<Instr_cvt_fptrunc const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_fptrunc "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_si2fp: {
      auto const instr = static_cast<Instr_cvt_si2fp const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_si2fp "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_fp2si: {
      auto const instr = static_cast<Instr_cvt_fp2si const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_fp2si "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_ui2fp: {
      auto const instr = static_cast<Instr_cvt_ui2fp const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_ui2fp "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
      printer.write(", "_sv);
      print_value(allocator, printer, options, instr->value);
    } break;

    case Instr_Kind::e_cvt_fp2ui: {
      auto const instr = static_cast<Instr_cvt_fp2ui const*>(generic_instr);
      print_value(allocator, printer, options, instr);
      printer.write(" = cvt_fp2ui "_sv);
      // TODO: Print type.
      printer.write("<type>"_sv);
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

  static void print_block(Allocator* const allocator, Printer& printer,
                          Prettyprint_Options const& options,
                          Basic_Block const* const block)
  {
    printer.set_indent_level(options.block_indent_level);
    printer.indent();
    print_block_id(allocator, printer, block);
    printer.write(":\n"_sv);
    for(Instr const& instr: block->instructions) {
      print_instr(allocator, printer, options, &instr);
    }
  }

  static void print_function(Allocator* const allocator, Printer& printer,
                             Prettyprint_Options const& options,
                             Function const* const function)
  {
    printer.write(function->identifier);
    if(options.function_location) {
      printer.write(" @ "_sv);
      print_source_location(allocator, printer, function->source_info);
    }
    printer.write("\n"_sv);
    print_block(allocator, printer, options, function->entry_block);
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
