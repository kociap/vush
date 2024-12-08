#pragma once

#include <anton/expected.hpp>

#include <vush_core/types.hpp>
#include <vush_diagnostics/error.hpp>
#include <vush_syntax/syntax.hpp>

namespace vush {
  struct Context;

  [[nodiscard]] anton::Expected<SNOT*, Error> full_expand(Context& ctx,
                                                          SNOT* snots);
} // namespace vush
