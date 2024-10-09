#include <vush_spirv/spirv.hpp>

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
} // namespace vush::spirv
