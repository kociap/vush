#pragma once

namespace vush::ir {
  enum struct ALU_Opcode {
    neg,
    iadd,
    imul,
    uadd,
    umul,
    fneg,
    fadd,
    fmul,
    fma,
    // perm - Permute Components
    //
    // dst = perm src, mask
    //
    // Permute components of the source register according to the mask storing
    // the result in the destination register. The destination register and the
    // mask register must have equal number of components, but need not be the
    // same width.
    //
    // Operation: for i in 0..components dst[i] = src[mask[i]]
    //
    perm,
    // extract - Extract Component
    //
    // dst = extract src, index
    //
    // Extract component of the source register. The destination register must
    // be a 1-component register of the same width.
    //
    // Operation: dst = src[index]
    //
    extract,
  };
} // namespace vush::ir
