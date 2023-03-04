#pragma once

#include <anton/expected.hpp>
#include <anton/slice.hpp>

#include <ast_fwd.hpp>
#include <vush/vush.hpp>

namespace vush {
    struct Context;

    // AST passes

    [[nodiscard]] anton::Expected<ast::Node_List, Error> run_ast_construction_pass(Context& ctx, ast::Node_List ast);
    [[nodiscard]] anton::Expected<void, Error> run_ast_validation_pass(Context& ctx, ast::Node_List ast);
    [[nodiscard]] anton::Expected<void, Error> run_ast_defcheck_pass(Context& ctx, ast::Node_List ast);
} // namespace vush
