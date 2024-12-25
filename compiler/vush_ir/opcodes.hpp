#pragma once

namespace vush::ir {
  enum struct ALU_Opcode {
    e_inv, // Bitwise inversion
    e_and, // Bitwise and
    e_or, // Bitwise or
    e_xor, // Bitwise xor
    e_shl,
    e_shr,
    e_neg, // Algebraic negation
    e_iadd,
    e_imul,
    e_uadd,
    e_umul,
    e_idiv,
    e_udiv,
    e_irem,
    e_urem,
    e_fneg,
    e_fadd,
    e_fmul,
    e_fdiv,
    // Integer comparisons
    e_icmp_eq,
    e_icmp_neq,
    e_icmp_ugt, // Unsigned greater than
    e_icmp_ult, // Unsigned less than
    e_icmp_uge, // Unsigned greater or equal
    e_icmp_ule, // Unsigned less or equal
    e_icmp_sgt, // Signed greater than
    e_icmp_slt, // Signed less than
    e_icmp_sge, // Signed greater or equal
    e_icmp_sle, // Signed less or equal
    // FP comparisons
    e_fcmp_eq,
    e_fcmp_neq,
    e_fcmp_gt,
    e_fcmp_lt,
    e_fcmp_ge,
    e_fcmp_le,
  };
} // namespace vush::ir
