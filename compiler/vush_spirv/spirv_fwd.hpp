#pragma once

namespace vush::spirv {
  struct Instr_string;
  struct Instr_line;
  struct Instr_extension;
  struct Instr_ext_instr_import;
  struct Instr_ext_instr;
  struct Instr_memory_model;
  struct Instr_entry_point;
  struct Instr_capability;
  struct Instr_type_void;
  struct Instr_type_bool;
  struct Instr_type_int;
  struct Instr_type_float;
  struct Instr_type_vector;
  struct Instr_type_matrix;
  struct Instr_type_image;
  struct Instr_type_sampler;
  struct Instr_type_sampled_image;
  struct Instr_type_array;
  struct Instr_type_runtime_array;
  struct Instr_type_struct;
  struct Instr_type_pointer;
  struct Instr_type_function;
  struct Instr_constant_true;
  struct Instr_constant_false;
  struct Instr_constant;
  struct Instr_constant_composite;
  struct Instr_variable;
  struct Instr_load;
  struct Instr_store;
  struct Instr_access_chain;
  struct Instr_function;
  struct Instr_function_parameter;
  struct Instr_function_end;
  struct Instr_function_call;
  struct Instr_convert_f2u;
  struct Instr_convert_f2s;
  struct Instr_convert_s2f;
  struct Instr_convert_u2f;
  struct Instr_uconvert;
  struct Instr_sconvert;
  struct Instr_fconvert;
  struct Instr_convert_ptr2u;
  struct Instr_convert_u2ptr;
  struct Instr_composite_construct;
  struct Instr_composite_extract;
  struct Instr_composite_insert;
  struct Instr_copy_object;
  struct Instr_transpose;
  struct Instr_snegate;
  struct Instr_fnegate;
  struct Instr_iadd;
  struct Instr_fadd;
  struct Instr_isub;
  struct Instr_fsub;
  struct Instr_imul;
  struct Instr_fmul;
  struct Instr_udiv;
  struct Instr_sdiv;
  struct Instr_fdiv;
  struct Instr_umod;
  struct Instr_srem;
  struct Instr_smod;
  struct Instr_frem;
  struct Instr_fmod;
  struct Instr_vec_times_scalar;
  struct Instr_mat_times_scalar;
  struct Instr_vec_times_mat;
  struct Instr_mat_times_vec;
  struct Instr_mat_times_mat;
  struct Instr_outer_product;
  struct Instr_dot;
  struct Instr_shr_logical;
  struct Instr_shr_arithmetic;
  struct Instr_shl;
  struct Instr_bit_or;
  struct Instr_bit_xor;
  struct Instr_bit_and;
  struct Instr_bit_not;
  struct Instr_logical_eq;
  struct Instr_logical_neq;
  struct Instr_logical_or;
  struct Instr_logical_and;
  struct Instr_logical_not;
  struct Instr_select;
  struct Instr_ieq;
  struct Instr_ineq;
  struct Instr_ugt;
  struct Instr_sgt;
  struct Instr_uge;
  struct Instr_sge;
  struct Instr_ult;
  struct Instr_slt;
  struct Instr_ule;
  struct Instr_sle;
  struct Instr_foeq;
  struct Instr_fueq;
  struct Instr_foneq;
  struct Instr_funeq;
  struct Instr_folt;
  struct Instr_fult;
  struct Instr_fogt;
  struct Instr_fugt;
  struct Instr_fole;
  struct Instr_fule;
  struct Instr_foge;
  struct Instr_fuge;
  struct Instr_dPdx;
  struct Instr_dPdy;
  struct Instr_fwidth;
  struct Instr_dPdx_fine;
  struct Instr_dPdy_fine;
  struct Instr_fwidth_fine;
  struct Instr_dPdx_coarse;
  struct Instr_dPdy_coarse;
  struct Instr_fwidth_coarse;
  struct Instr_phi;
  struct Instr_label;
  struct Instr_branch;
  struct Instr_brcond;
  struct Instr_switch;
  struct Instr_return;
  struct Instr_return_value;
  struct Instr_terminate;
  struct Instr_unreachable;
} // namespace vush::spirv
