#pragma once

namespace vush::ir {
  struct Module;
  struct Basic_Block;
  struct Function;
  struct Value;
  struct Constant;
  struct Constant_bool;
  struct Constant_i32;
  struct Constant_f32;
  struct Constant_f64;
  struct Constant_undef;
  struct Instr;
  struct Instr_alloc;
  struct Instr_load;
  struct Instr_store;
  struct Instr_getptr;
  struct Instr_ALU;
  struct Instr_setvalue;
  struct Instr_call;
  struct Instr_branch;
  struct Instr_brcond;
  struct Switch_Label;
  struct Instr_switch;
  struct Instr_phi;
  struct Instr_return;
  struct Instr_die;
} // namespace vush::ir
