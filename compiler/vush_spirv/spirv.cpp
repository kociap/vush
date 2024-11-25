#include <vush_spirv/spirv.hpp>

#include <vush_core/memory.hpp>

namespace vush::spirv {
  template<>
  bool instanceof<Instr_string>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_string;
  }

  template<>
  bool instanceof<Instr_line>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_line;
  }

  template<>
  bool instanceof<Instr_extension>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_extension;
  }

  template<>
  bool instanceof<Instr_ext_instr_import>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_ext_instr_import;
  }

  template<>
  bool instanceof<Instr_ext_instr>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_ext_instr;
  }

  template<>
  bool instanceof<Instr_memory_model>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_memory_model;
  }

  template<>
  bool instanceof<Instr_entry_point>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_entry_point;
  }

  template<>
  bool instanceof<Instr_capability>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_capability;
  }

  template<>
  bool instanceof<Instr_type_void>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_void;
  }

  template<>
  bool instanceof<Instr_type_bool>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_bool;
  }

  template<>
  bool instanceof<Instr_type_int>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_int;
  }

  template<>
  bool instanceof<Instr_type_float>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_float;
  }

  template<>
  bool instanceof<Instr_type_vector>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_vector;
  }

  template<>
  bool instanceof<Instr_type_matrix>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_matrix;
  }

  template<>
  bool instanceof<Instr_type_image>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_image;
  }

  template<>
  bool instanceof<Instr_type_sampler>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_sampler;
  }

  template<>
  bool instanceof<Instr_type_sampled_image>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_sampled_image;
  }

  template<>
  bool instanceof<Instr_type_array>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_array;
  }

  template<>
  bool instanceof<Instr_type_runtime_array>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_runtime_array;
  }

  template<>
  bool instanceof<Instr_type_struct>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_struct;
  }

  template<>
  bool instanceof<Instr_type_pointer>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_pointer;
  }

  template<>
  bool instanceof<Instr_type_function>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_type_function;
  }

  template<>
  bool instanceof<Instr_constant_true>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_constant_true;
  }

  template<>
  bool instanceof<Instr_constant_false>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_constant_false;
  }

  template<>
  bool instanceof<Instr_constant>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_constant;
  }

  template<>
  bool instanceof<Instr_constant_composite>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_constant_composite;
  }

  template<>
  bool instanceof<Instr_variable>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_variable;
  }

  template<>
  bool instanceof<Instr_load>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_load;
  }

  template<>
  bool instanceof<Instr_store>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_store;
  }

  template<>
  bool instanceof<Instr_access_chain>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_access_chain;
  }

  template<>
  bool instanceof<Instr_function>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_function;
  }

  template<>
  bool instanceof<Instr_function_parameter>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_function_parameter;
  }

  template<>
  bool instanceof<Instr_function_end>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_function_end;
  }

  template<>
  bool instanceof<Instr_function_call>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_function_call;
  }

  template<>
  bool instanceof<Instr_convert_f2u>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_convert_f2u;
  }

  template<>
  bool instanceof<Instr_convert_f2s>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_convert_f2s;
  }

  template<>
  bool instanceof<Instr_convert_s2f>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_convert_s2f;
  }

  template<>
  bool instanceof<Instr_convert_u2f>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_convert_u2f;
  }

  template<>
  bool instanceof<Instr_uconvert>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_uconvert;
  }

  template<>
  bool instanceof<Instr_sconvert>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_sconvert;
  }

  template<>
  bool instanceof<Instr_fconvert>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fconvert;
  }

  template<>
  bool instanceof<Instr_convert_ptr2u>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_convert_ptr2u;
  }

  template<>
  bool instanceof<Instr_convert_u2ptr>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_convert_u2ptr;
  }

  template<>
  bool instanceof<Instr_composite_construct>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_composite_construct;
  }

  template<>
  bool instanceof<Instr_composite_extract>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_composite_extract;
  }

  template<>
  bool instanceof<Instr_composite_insert>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_composite_insert;
  }

  template<>
  bool instanceof<Instr_copy_object>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_copy_object;
  }

  template<>
  bool instanceof<Instr_transpose>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_transpose;
  }

  template<>
  bool instanceof<Instr_snegate>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_snegate;
  }

  template<>
  bool instanceof<Instr_fnegate>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fnegate;
  }

  template<>
  bool instanceof<Instr_iadd>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_iadd;
  }

  template<>
  bool instanceof<Instr_fadd>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fadd;
  }

  template<>
  bool instanceof<Instr_isub>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_isub;
  }

  template<>
  bool instanceof<Instr_fsub>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fsub;
  }

  template<>
  bool instanceof<Instr_imul>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_imul;
  }

  template<>
  bool instanceof<Instr_fmul>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fmul;
  }

  template<>
  bool instanceof<Instr_udiv>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_udiv;
  }

  template<>
  bool instanceof<Instr_sdiv>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_sdiv;
  }

  template<>
  bool instanceof<Instr_fdiv>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fdiv;
  }

  template<>
  bool instanceof<Instr_umod>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_umod;
  }

  template<>
  bool instanceof<Instr_srem>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_srem;
  }

  template<>
  bool instanceof<Instr_smod>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_smod;
  }

  template<>
  bool instanceof<Instr_frem>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_frem;
  }

  template<>
  bool instanceof<Instr_fmod>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fmod;
  }

  template<>
  bool instanceof<Instr_vec_times_scalar>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_vec_times_scalar;
  }

  template<>
  bool instanceof<Instr_mat_times_scalar>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_mat_times_scalar;
  }

  template<>
  bool instanceof<Instr_vec_times_mat>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_vec_times_mat;
  }

  template<>
  bool instanceof<Instr_mat_times_vec>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_mat_times_vec;
  }

  template<>
  bool instanceof<Instr_mat_times_mat>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_mat_times_mat;
  }

  template<>
  bool instanceof<Instr_outer_product>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_outer_product;
  }

  template<>
  bool instanceof<Instr_dot>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_dot;
  }

  template<>
  bool instanceof<Instr_shr_logical>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_shr_logical;
  }

  template<>
  bool instanceof<Instr_shr_arithmetic>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_shr_arithmetic;
  }

  template<>
  bool instanceof<Instr_shl>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_shl;
  }

  template<>
  bool instanceof<Instr_bit_or>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_bit_or;
  }

  template<>
  bool instanceof<Instr_bit_xor>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_bit_xor;
  }

  template<>
  bool instanceof<Instr_bit_and>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_bit_and;
  }

  template<>
  bool instanceof<Instr_bit_not>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_bit_not;
  }

  template<>
  bool instanceof<Instr_logical_eq>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_logical_eq;
  }

  template<>
  bool instanceof<Instr_logical_neq>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_logical_neq;
  }

  template<>
  bool instanceof<Instr_logical_or>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_logical_or;
  }

  template<>
  bool instanceof<Instr_logical_and>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_logical_and;
  }

  template<>
  bool instanceof<Instr_logical_not>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_logical_not;
  }

  template<>
  bool instanceof<Instr_select>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_select;
  }

  template<>
  bool instanceof<Instr_ieq>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_ieq;
  }

  template<>
  bool instanceof<Instr_ineq>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_ineq;
  }

  template<>
  bool instanceof<Instr_ugt>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_ugt;
  }

  template<>
  bool instanceof<Instr_sgt>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_sgt;
  }

  template<>
  bool instanceof<Instr_uge>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_uge;
  }

  template<>
  bool instanceof<Instr_sge>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_sge;
  }

  template<>
  bool instanceof<Instr_ult>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_ult;
  }

  template<>
  bool instanceof<Instr_slt>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_slt;
  }

  template<>
  bool instanceof<Instr_ule>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_ule;
  }

  template<>
  bool instanceof<Instr_sle>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_sle;
  }

  template<>
  bool instanceof<Instr_foeq>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_foeq;
  }

  template<>
  bool instanceof<Instr_fueq>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fueq;
  }

  template<>
  bool instanceof<Instr_foneq>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_foneq;
  }

  template<>
  bool instanceof<Instr_funeq>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_funeq;
  }

  template<>
  bool instanceof<Instr_folt>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_folt;
  }

  template<>
  bool instanceof<Instr_fult>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fult;
  }

  template<>
  bool instanceof<Instr_fogt>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fogt;
  }

  template<>
  bool instanceof<Instr_fugt>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fugt;
  }

  template<>
  bool instanceof<Instr_fole>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fole;
  }

  template<>
  bool instanceof<Instr_fule>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fule;
  }

  template<>
  bool instanceof<Instr_foge>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_foge;
  }

  template<>
  bool instanceof<Instr_fuge>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fuge;
  }

  template<>
  bool instanceof<Instr_dPdx>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_dPdx;
  }

  template<>
  bool instanceof<Instr_dPdy>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_dPdy;
  }

  template<>
  bool instanceof<Instr_fwidth>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fwidth;
  }

  template<>
  bool instanceof<Instr_dPdx_fine>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_dPdx_fine;
  }

  template<>
  bool instanceof<Instr_dPdy_fine>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_dPdy_fine;
  }

  template<>
  bool instanceof<Instr_fwidth_fine>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fwidth_fine;
  }

  template<>
  bool instanceof<Instr_dPdx_coarse>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_dPdx_coarse;
  }

  template<>
  bool instanceof<Instr_dPdy_coarse>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_dPdy_coarse;
  }

  template<>
  bool instanceof<Instr_fwidth_coarse>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_fwidth_coarse;
  }

  template<>
  bool instanceof<Instr_phi>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_phi;
  }

  template<>
  bool instanceof<Instr_label>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_label;
  }

  template<>
  bool instanceof<Instr_branch>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_branch;
  }

  template<>
  bool instanceof<Instr_brcond>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_brcond;
  }

  template<>
  bool instanceof<Instr_switch>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_switch;
  }

  template<>
  bool instanceof<Instr_return>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_return;
  }

  template<>
  bool instanceof<Instr_return_value>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_return_value;
  }

  template<>
  bool instanceof<Instr_terminate>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_terminate;
  }

  template<>
  bool instanceof<Instr_unreachable>(Instr const* const instr)
  {
    return instr->instr_kind == Instr_Kind::e_unreachable;
  }

  Instr* get_result_type(Instr* const instruction)
  {
    switch(instruction->instr_kind) {
    case Instr_Kind::e_ext_instr:
      return static_cast<Instr_ext_instr*>(instruction)->result_type;
    case Instr_Kind::e_constant_true:
      return static_cast<Instr_constant_true*>(instruction)->result_type;
    case Instr_Kind::e_constant_false:
      return static_cast<Instr_constant_false*>(instruction)->result_type;
    case Instr_Kind::e_constant:
      return static_cast<Instr_constant*>(instruction)->result_type;
    case Instr_Kind::e_constant_composite:
      return static_cast<Instr_constant_composite*>(instruction)->result_type;
    case Instr_Kind::e_variable:
      return static_cast<Instr_variable*>(instruction)->result_type;
    case Instr_Kind::e_load:
      return static_cast<Instr_load*>(instruction)->result_type;
    case Instr_Kind::e_access_chain:
      return static_cast<Instr_access_chain*>(instruction)->result_type;
    case Instr_Kind::e_function_parameter:
      return static_cast<Instr_function_parameter*>(instruction)->result_type;
    case Instr_Kind::e_function_call:
      return static_cast<Instr_function_call*>(instruction)
        ->function->function_type->return_type;
    case Instr_Kind::e_convert_f2u:
      return static_cast<Instr_convert_f2u*>(instruction)->result_type;
    case Instr_Kind::e_convert_f2s:
      return static_cast<Instr_convert_f2s*>(instruction)->result_type;
    case Instr_Kind::e_convert_s2f:
      return static_cast<Instr_convert_s2f*>(instruction)->result_type;
    case Instr_Kind::e_convert_u2f:
      return static_cast<Instr_convert_u2f*>(instruction)->result_type;
    case Instr_Kind::e_uconvert:
      return static_cast<Instr_uconvert*>(instruction)->result_type;
    case Instr_Kind::e_sconvert:
      return static_cast<Instr_sconvert*>(instruction)->result_type;
    case Instr_Kind::e_fconvert:
      return static_cast<Instr_fconvert*>(instruction)->result_type;
    case Instr_Kind::e_convert_ptr2u:
      return static_cast<Instr_convert_ptr2u*>(instruction)->result_type;
    case Instr_Kind::e_convert_u2ptr:
      return static_cast<Instr_convert_u2ptr*>(instruction)->result_type;
    case Instr_Kind::e_composite_construct:
      return static_cast<Instr_composite_construct*>(instruction)->result_type;
    case Instr_Kind::e_composite_extract:
      return static_cast<Instr_composite_extract*>(instruction)->result_type;
    case Instr_Kind::e_composite_insert:
      return static_cast<Instr_composite_insert*>(instruction)->result_type;
    case Instr_Kind::e_copy_object:
      return static_cast<Instr_copy_object*>(instruction)->result_type;
    case Instr_Kind::e_transpose:
      return static_cast<Instr_transpose*>(instruction)->result_type;
    case Instr_Kind::e_snegate:
      return static_cast<Instr_snegate*>(instruction)->result_type;
    case Instr_Kind::e_fnegate:
      return static_cast<Instr_fnegate*>(instruction)->result_type;
    case Instr_Kind::e_iadd:
      return static_cast<Instr_iadd*>(instruction)->result_type;
    case Instr_Kind::e_fadd:
      return static_cast<Instr_fadd*>(instruction)->result_type;
    case Instr_Kind::e_isub:
      return static_cast<Instr_isub*>(instruction)->result_type;
    case Instr_Kind::e_fsub:
      return static_cast<Instr_fsub*>(instruction)->result_type;
    case Instr_Kind::e_imul:
      return static_cast<Instr_imul*>(instruction)->result_type;
    case Instr_Kind::e_fmul:
      return static_cast<Instr_fmul*>(instruction)->result_type;
    case Instr_Kind::e_udiv:
      return static_cast<Instr_udiv*>(instruction)->result_type;
    case Instr_Kind::e_sdiv:
      return static_cast<Instr_sdiv*>(instruction)->result_type;
    case Instr_Kind::e_fdiv:
      return static_cast<Instr_fdiv*>(instruction)->result_type;
    case Instr_Kind::e_umod:
      return static_cast<Instr_umod*>(instruction)->result_type;
    case Instr_Kind::e_srem:
      return static_cast<Instr_srem*>(instruction)->result_type;
    case Instr_Kind::e_smod:
      return static_cast<Instr_smod*>(instruction)->result_type;
    case Instr_Kind::e_frem:
      return static_cast<Instr_frem*>(instruction)->result_type;
    case Instr_Kind::e_fmod:
      return static_cast<Instr_fmod*>(instruction)->result_type;
    case Instr_Kind::e_vec_times_scalar:
      return static_cast<Instr_vec_times_scalar*>(instruction)->result_type;
    case Instr_Kind::e_mat_times_scalar:
      return static_cast<Instr_mat_times_scalar*>(instruction)->result_type;
    case Instr_Kind::e_vec_times_mat:
      return static_cast<Instr_vec_times_mat*>(instruction)->result_type;
    case Instr_Kind::e_mat_times_vec:
      return static_cast<Instr_mat_times_vec*>(instruction)->result_type;
    case Instr_Kind::e_mat_times_mat:
      return static_cast<Instr_mat_times_mat*>(instruction)->result_type;
    case Instr_Kind::e_outer_product:
      return static_cast<Instr_outer_product*>(instruction)->result_type;
    case Instr_Kind::e_dot:
      return static_cast<Instr_dot*>(instruction)->result_type;
    case Instr_Kind::e_shr_logical:
      return static_cast<Instr_shr_logical*>(instruction)->result_type;
    case Instr_Kind::e_shr_arithmetic:
      return static_cast<Instr_shr_arithmetic*>(instruction)->result_type;
    case Instr_Kind::e_shl:
      return static_cast<Instr_shl*>(instruction)->result_type;
    case Instr_Kind::e_bit_or:
      return static_cast<Instr_bit_or*>(instruction)->result_type;
    case Instr_Kind::e_bit_xor:
      return static_cast<Instr_bit_xor*>(instruction)->result_type;
    case Instr_Kind::e_bit_and:
      return static_cast<Instr_bit_and*>(instruction)->result_type;
    case Instr_Kind::e_bit_not:
      return static_cast<Instr_bit_not*>(instruction)->result_type;
    case Instr_Kind::e_logical_eq:
      return static_cast<Instr_logical_eq*>(instruction)->result_type;
    case Instr_Kind::e_logical_neq:
      return static_cast<Instr_logical_neq*>(instruction)->result_type;
    case Instr_Kind::e_logical_or:
      return static_cast<Instr_logical_or*>(instruction)->result_type;
    case Instr_Kind::e_logical_and:
      return static_cast<Instr_logical_and*>(instruction)->result_type;
    case Instr_Kind::e_logical_not:
      return static_cast<Instr_logical_not*>(instruction)->result_type;
    case Instr_Kind::e_select:
      return static_cast<Instr_select*>(instruction)->result_type;
    case Instr_Kind::e_ieq:
      return static_cast<Instr_ieq*>(instruction)->result_type;
    case Instr_Kind::e_ineq:
      return static_cast<Instr_ineq*>(instruction)->result_type;
    case Instr_Kind::e_ugt:
      return static_cast<Instr_ugt*>(instruction)->result_type;
    case Instr_Kind::e_sgt:
      return static_cast<Instr_sgt*>(instruction)->result_type;
    case Instr_Kind::e_uge:
      return static_cast<Instr_uge*>(instruction)->result_type;
    case Instr_Kind::e_sge:
      return static_cast<Instr_sge*>(instruction)->result_type;
    case Instr_Kind::e_ult:
      return static_cast<Instr_ult*>(instruction)->result_type;
    case Instr_Kind::e_slt:
      return static_cast<Instr_slt*>(instruction)->result_type;
    case Instr_Kind::e_ule:
      return static_cast<Instr_ule*>(instruction)->result_type;
    case Instr_Kind::e_sle:
      return static_cast<Instr_sle*>(instruction)->result_type;
    case Instr_Kind::e_foeq:
      return static_cast<Instr_foeq*>(instruction)->result_type;
    case Instr_Kind::e_fueq:
      return static_cast<Instr_fueq*>(instruction)->result_type;
    case Instr_Kind::e_foneq:
      return static_cast<Instr_foneq*>(instruction)->result_type;
    case Instr_Kind::e_funeq:
      return static_cast<Instr_funeq*>(instruction)->result_type;
    case Instr_Kind::e_folt:
      return static_cast<Instr_folt*>(instruction)->result_type;
    case Instr_Kind::e_fult:
      return static_cast<Instr_fult*>(instruction)->result_type;
    case Instr_Kind::e_fogt:
      return static_cast<Instr_fogt*>(instruction)->result_type;
    case Instr_Kind::e_fugt:
      return static_cast<Instr_fugt*>(instruction)->result_type;
    case Instr_Kind::e_fole:
      return static_cast<Instr_fole*>(instruction)->result_type;
    case Instr_Kind::e_fule:
      return static_cast<Instr_fule*>(instruction)->result_type;
    case Instr_Kind::e_foge:
      return static_cast<Instr_foge*>(instruction)->result_type;
    case Instr_Kind::e_fuge:
      return static_cast<Instr_fuge*>(instruction)->result_type;
    case Instr_Kind::e_dPdx:
      return static_cast<Instr_dPdx*>(instruction)->result_type;
    case Instr_Kind::e_dPdy:
      return static_cast<Instr_dPdy*>(instruction)->result_type;
    case Instr_Kind::e_fwidth:
      return static_cast<Instr_fwidth*>(instruction)->result_type;
    case Instr_Kind::e_dPdx_fine:
      return static_cast<Instr_dPdx_fine*>(instruction)->result_type;
    case Instr_Kind::e_dPdy_fine:
      return static_cast<Instr_dPdy_fine*>(instruction)->result_type;
    case Instr_Kind::e_fwidth_fine:
      return static_cast<Instr_fwidth_fine*>(instruction)->result_type;
    case Instr_Kind::e_dPdx_coarse:
      return static_cast<Instr_dPdx_coarse*>(instruction)->result_type;
    case Instr_Kind::e_dPdy_coarse:
      return static_cast<Instr_dPdy_coarse*>(instruction)->result_type;
    case Instr_Kind::e_fwidth_coarse:
      return static_cast<Instr_fwidth_coarse*>(instruction)->result_type;
    case Instr_Kind::e_phi:
      return static_cast<Instr_phi*>(instruction)->result_type;

    default:
      return nullptr;
    }
  }

  u32 calculate_bound(anton::IList<spirv::Instr> const& instructions)
  {
    u32 max = 0;
    for(auto const& instruction: instructions) {
      max = anton::math::max(max, instruction.id);
    }
    return max;
  }

  u32 calculate_bound(Module const& module)
  {
    // We do not check capabilities, extensions, declarations, annotations.
    // Those sections do not contain instructions producing IDs.
    u32 const max_imports = calculate_bound(module.imports);
    u32 const max_debug = calculate_bound(module.debug);
    u32 const max_types = calculate_bound(module.types);
    u32 const max_functions = calculate_bound(module.functions);
    return anton::math::max(max_imports, max_debug, max_types, max_functions);
  }

#define BINARY_INSTR_MAKE_FN(IDENTIFIER)                                \
  Instr_##IDENTIFIER* make_instr_##IDENTIFIER(                          \
    Allocator* allocator, u32 id, Instr* result_type, Instr* operand1,  \
    Instr* operand2)                                                    \
  {                                                                     \
    auto const instr = VUSH_ALLOCATE(Instr_##IDENTIFIER, allocator, id, \
                                     result_type, operand1, operand2);  \
    return instr;                                                       \
  }

#define TYPED_INSTR_MAKE_FN(IDENTIFIER)                                     \
  Instr_##IDENTIFIER* make_instr_##IDENTIFIER(Allocator* allocator, u32 id, \
                                              Instr* result_type)           \
  {                                                                         \
    auto const instr =                                                      \
      VUSH_ALLOCATE(Instr_##IDENTIFIER, allocator, id, result_type);        \
    return instr;                                                           \
  }

  Instr_memory_model* make_instr_memory_model(Allocator* allocator,
                                              Addressing_Model am,
                                              Memory_Model mm)
  {
    auto const instr = VUSH_ALLOCATE(Instr_memory_model, allocator, am, mm);
    return instr;
  }

  Instr_entry_point* make_instr_entry_point(Allocator* allocator,
                                            Instr_function* entry_point,
                                            anton::String&& name,
                                            Execution_Model execution_model)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_entry_point, allocator, entry_point, ANTON_MOV(name),
                    execution_model, allocator);
    return instr;
  }

  Instr_capability* make_instr_capability(Allocator* allocator,
                                          Capability capability)
  {
    auto const instr = VUSH_ALLOCATE(Instr_capability, allocator, capability);
    return instr;
  }

  Instr_type_void* make_instr_type_void(Allocator* allocator, u32 id)
  {
    auto const instr = VUSH_ALLOCATE(Instr_type_void, allocator, id);
    return instr;
  }

  Instr_type_bool* make_instr_type_bool(Allocator* allocator, u32 id)
  {
    auto const instr = VUSH_ALLOCATE(Instr_type_bool, allocator, id);
    return instr;
  }

  Instr_type_int* make_instr_type_int(Allocator* allocator, u32 id, u32 width,
                                      bool signedness)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_type_int, allocator, id, width, signedness);
    return instr;
  }

  Instr_type_float* make_instr_type_float(Allocator* allocator, u32 id,
                                          u32 width)
  {
    auto const instr = VUSH_ALLOCATE(Instr_type_float, allocator, id, width);
    return instr;
  }

  Instr_type_struct* make_instr_type_struct(Allocator* allocator, u32 id)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_type_struct, allocator, id, allocator);
    return instr;
  }

  Instr_type_array* make_instr_type_array(Allocator* allocator, u32 id,
                                          Instr* element_type, Instr* length)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_type_array, allocator, id, element_type, length);
    return instr;
  }

  Instr_type_runtime_array* make_instr_type_runtime_array(Allocator* allocator,
                                                          u32 id,
                                                          Instr* element_type)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_type_runtime_array, allocator, id, element_type);
    return instr;
  }

  Instr_type_vector* make_instr_type_vector(Allocator* allocator, u32 id,
                                            Instr* component_type,
                                            u32 component_count)
  {
    auto const instr = VUSH_ALLOCATE(Instr_type_vector, allocator, id,
                                     component_type, component_count);
    return instr;
  }

  Instr_type_matrix* make_instr_type_matrix(Allocator* allocator, u32 id,
                                            Instr* column_type,
                                            u32 column_count)
  {
    auto const instr = VUSH_ALLOCATE(Instr_type_matrix, allocator, id,
                                     column_type, column_count);
    return instr;
  }

  Instr_type_pointer* make_instr_type_pointer(Allocator* allocator, u32 id,
                                              Instr* type,
                                              Storage_Class storage_class)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_type_pointer, allocator, id, type, storage_class);
    return instr;
  }

  Instr_type_function* make_instr_type_function(Allocator* allocator, u32 id,
                                                Instr* return_type)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_type_function, allocator, id, return_type, allocator);
    return instr;
  }

  TYPED_INSTR_MAKE_FN(constant_true)
  TYPED_INSTR_MAKE_FN(constant_false)

  Instr_constant* make_instr_constant_i32(Allocator* allocator, u32 id,
                                          Instr* result_type, i32 value)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_constant, allocator, id, result_type, 4, &value);
    return instr;
  }

  Instr_constant* make_instr_constant_u32(Allocator* allocator, u32 id,
                                          Instr* result_type, u32 value)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_constant, allocator, id, result_type, 4, &value);
    return instr;
  }

  Instr_constant* make_instr_constant_f32(Allocator* allocator, u32 id,
                                          Instr* result_type, f32 value)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_constant, allocator, id, result_type, 4, &value);
    return instr;
  }

  Instr_constant* make_instr_constant_f64(Allocator* allocator, u32 id,
                                          Instr* result_type, f64 value)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_constant, allocator, id, result_type, 8, &value);
    return instr;
  }

  Instr_variable* make_instr_variable(Allocator* allocator, u32 id,
                                      Instr* result_type,
                                      Storage_Class storage_class)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_variable, allocator, id, result_type, storage_class);
    return instr;
  }

  Instr_load* make_instr_load(Allocator* allocator, u32 id, Instr* result_type,
                              Instr* pointer)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_load, allocator, id, result_type, pointer);
    return instr;
  }

  Instr_store* make_instr_store(Allocator* allocator, Instr* pointer,
                                Instr* object)
  {
    auto const instr = VUSH_ALLOCATE(Instr_store, allocator, pointer, object);
    return instr;
  }

  Instr_access_chain*
  make_instr_access_chain(Allocator* allocator, u32 id, Instr* result_type,
                          Instr* base, anton::Slice<Instr* const> indices)
  {
    auto const instr = VUSH_ALLOCATE(Instr_access_chain, allocator, id,
                                     result_type, base, allocator, indices);
    return instr;
  }

  Instr_function* make_instr_function(Allocator* allocator, u32 id,
                                      Instr_type_function* function_type)
  {
    auto const instr =
      VUSH_ALLOCATE(Instr_function, allocator, id, function_type);
    return instr;
  }

  TYPED_INSTR_MAKE_FN(function_parameter)

  Instr_function_end* make_instr_function_end(Allocator* allocator)
  {
    auto const instr = VUSH_ALLOCATE(Instr_function_end, allocator);
    return instr;
  }

  Instr_composite_construct*
  make_instr_composite_construct(Allocator* allocator, u32 id,
                                 Instr* result_type)
  {
    auto const instr = VUSH_ALLOCATE(Instr_composite_construct, allocator, id,
                                     result_type, allocator);
    return instr;
  }

  Instr_composite_extract* make_instr_composite_extract(Allocator* allocator,
                                                        u32 id,
                                                        Instr* result_type,
                                                        Instr* composite)
  {
    auto const instr = VUSH_ALLOCATE(Instr_composite_extract, allocator, id,
                                     result_type, composite, allocator);
    return instr;
  }

  Instr_composite_insert*
  make_instr_composite_insert(Allocator* allocator, u32 id, Instr* result_type,
                              Instr* composite, Instr* object)
  {
    auto const instr = VUSH_ALLOCATE(Instr_composite_insert, allocator, id,
                                     result_type, composite, object, allocator);
    return instr;
  }

  BINARY_INSTR_MAKE_FN(snegate);
  BINARY_INSTR_MAKE_FN(fnegate);
  BINARY_INSTR_MAKE_FN(iadd);
  BINARY_INSTR_MAKE_FN(fadd);
  BINARY_INSTR_MAKE_FN(isub);
  BINARY_INSTR_MAKE_FN(fsub);
  BINARY_INSTR_MAKE_FN(imul);
  BINARY_INSTR_MAKE_FN(fmul);
  BINARY_INSTR_MAKE_FN(udiv);
  BINARY_INSTR_MAKE_FN(sdiv);
  BINARY_INSTR_MAKE_FN(fdiv);
  BINARY_INSTR_MAKE_FN(umod);
  BINARY_INSTR_MAKE_FN(srem);
  BINARY_INSTR_MAKE_FN(smod);
  BINARY_INSTR_MAKE_FN(frem);
  BINARY_INSTR_MAKE_FN(fmod);
  BINARY_INSTR_MAKE_FN(vec_times_scalar);
  BINARY_INSTR_MAKE_FN(mat_times_scalar);
  BINARY_INSTR_MAKE_FN(vec_times_mat);
  BINARY_INSTR_MAKE_FN(mat_times_vec);
  BINARY_INSTR_MAKE_FN(mat_times_mat);
  BINARY_INSTR_MAKE_FN(outer_product);
  BINARY_INSTR_MAKE_FN(dot);
  BINARY_INSTR_MAKE_FN(shr_logical);
  BINARY_INSTR_MAKE_FN(shr_arithmetic);
  BINARY_INSTR_MAKE_FN(shl); // ShiftLeftLogical
  BINARY_INSTR_MAKE_FN(bit_or);
  BINARY_INSTR_MAKE_FN(bit_xor);
  BINARY_INSTR_MAKE_FN(bit_and);
  BINARY_INSTR_MAKE_FN(bit_not);
  BINARY_INSTR_MAKE_FN(logical_eq);
  BINARY_INSTR_MAKE_FN(logical_neq);
  BINARY_INSTR_MAKE_FN(logical_or);
  BINARY_INSTR_MAKE_FN(logical_and);
  BINARY_INSTR_MAKE_FN(logical_not);
  BINARY_INSTR_MAKE_FN(ieq); // IEqual
  BINARY_INSTR_MAKE_FN(ineq); // INotEqual
  BINARY_INSTR_MAKE_FN(ugt); // UGreaterThan
  BINARY_INSTR_MAKE_FN(sgt); // SGreaterThan
  BINARY_INSTR_MAKE_FN(uge); // UGreaterThanEqual
  BINARY_INSTR_MAKE_FN(sge); // SGreaterThanEqual
  BINARY_INSTR_MAKE_FN(ult); // ULessThan
  BINARY_INSTR_MAKE_FN(slt); // SLessThan
  BINARY_INSTR_MAKE_FN(ule); // ULessThanEqual
  BINARY_INSTR_MAKE_FN(sle); // SLessThanEqual
  BINARY_INSTR_MAKE_FN(foeq); // FOrdEqual
  BINARY_INSTR_MAKE_FN(fueq); // FUnordEqual
  BINARY_INSTR_MAKE_FN(foneq); // FOrdNotEqual
  BINARY_INSTR_MAKE_FN(funeq); // FUnordNotEqual
  BINARY_INSTR_MAKE_FN(folt); // FOrdLessThan
  BINARY_INSTR_MAKE_FN(fult); // FUnordLessThan
  BINARY_INSTR_MAKE_FN(fogt); // FOrdGreaterThan
  BINARY_INSTR_MAKE_FN(fugt); // FUnordGreaterThan
  BINARY_INSTR_MAKE_FN(fole); // FOrdLessThanEqual
  BINARY_INSTR_MAKE_FN(fule); // FUnordLessThanEqual
  BINARY_INSTR_MAKE_FN(foge); // FOrdGreaterThanEqual
  BINARY_INSTR_MAKE_FN(fuge); // FUnordGreaterThanEqual

  Instr_label* make_instr_label(Allocator* allocator, u32 id)
  {
    auto const instr = VUSH_ALLOCATE(Instr_label, allocator, id);
    return instr;
  }

  Instr_branch* make_instr_branch(Allocator* allocator, Instr_label* target)
  {
    auto const instr = VUSH_ALLOCATE(Instr_branch, allocator, target);
    return instr;
  }

  Instr_brcond* make_instr_brcond(Allocator* allocator, Instr* condition,
                                  Instr_label* true_label,
                                  Instr_label* false_label)
  {
    auto const instr = VUSH_ALLOCATE(Instr_brcond, allocator, condition,
                                     true_label, false_label);
    return instr;
  }

  Instr_switch* make_instr_switch(Allocator* allocator, Instr* selector,
                                  Instr_label* default_label)
  {
    auto const instr = VUSH_ALLOCATE(Instr_switch, allocator, selector,
                                     default_label, allocator);
    return instr;
  }

  Instr_return* make_instr_return(Allocator* allocator)
  {
    auto const instr = VUSH_ALLOCATE(Instr_return, allocator);
    return instr;
  }

  Instr_return_value* make_instr_return_value(Allocator* allocator,
                                              Instr* value)
  {
    auto const instr = VUSH_ALLOCATE(Instr_return_value, allocator, value);
    return instr;
  }

  Instr_terminate* make_instr_terminate(Allocator* allocator)
  {
    auto const instr = VUSH_ALLOCATE(Instr_terminate, allocator);
    return instr;
  }
} // namespace vush::spirv
