#pragma once

#include <anton/slice.hpp>

#include <ast_fwd.hpp>

namespace vush {
    [[nodiscard]] anton::Slice<ast::Decl_Overloaded_Function const* const> get_builtin_functions_declarations();
}
