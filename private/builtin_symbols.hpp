#pragma once

#include <anton/slice.hpp>

#include <ast2.hpp>

namespace vush {
    [[nodiscard]] anton::Slice<ast::Decl_Overloaded_Function const* const> get_builtin_functions_declarations();

    // get_builtin_type
    // Get an immutable builtin type.
    //
    [[nodiscard]] ast::Type_Builtin const* get_builtin_type(ast::GLSL_Type type);
} // namespace vush
