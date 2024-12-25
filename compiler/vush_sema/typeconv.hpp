#pragma once

#include <anton/optional.hpp>

#include <vush_ast/fwd.hpp>

namespace vush {
  [[nodiscard]] bool is_convertible(ast::Type const* to, ast::Type const* from);
  [[nodiscard]] anton::Optional<i64>
  rank_conversion(ast::Type const* const to, ast::Type const* const from);
} // namespace vush
