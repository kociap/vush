#pragma once

#include <anton/stream.hpp>

#include <vush_ir/ir.hpp>

namespace vush::ir {
  struct Prettyprint_Options {
    bool function_location = false;
    bool instruction_location = false;
    i32 indent_width = 2;
    i32 function_indent_level = 0;
    i32 block_indent_level = 0;
    i32 instruction_indent_level = 1;
  };

  void prettyprint(Allocator* allocator, anton::Output_Stream& stream,
                   Prettyprint_Options const& options, Module const& module);
} // namespace vush::ir
