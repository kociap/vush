#pragma once

#include <vush_ir/ir.hpp>
#include <vush_spirv/spirv.hpp>

namespace vush {
  spirv::Instr* lower_ir_module(Allocator* const allocator,
                                ir::Module const* const module);
}
