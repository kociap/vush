#pragma once

#include <anton/expected.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <codegen.hpp>
#include <context.hpp>
#include <vush/vush.hpp>

namespace vush {
  // request_source_definitions
  //
  //
  [[nodiscard]] anton::Expected<void, anton::String>
  request_source_definitions(Context const& ctx, Pass_Context& pass,
                             anton::Slice<Setting_Key_Value const> settings);
} // namespace vush
