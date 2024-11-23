#pragma once

#include <anton/stream.hpp>

#include <vush_spirv/spirv.hpp>

namespace vush::spirv {
  struct Prettyprint_Options {
    i32 indent_width = 2;
    i32 function_indent_level = 0;
    i32 block_indent_level = 0;
    i32 instruction_indent_level = 1;
  };

  void prettyprint(Allocator* allocator, anton::Output_Stream& stream,
                   Prettyprint_Options const& options, Module const& module);
} // namespace vush::spirv
