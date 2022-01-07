#pragma once

#include <ast_fwd.hpp>
#include <context.hpp>

namespace vush {
    struct Builtin_Declarations {
        anton::Array<Owning_Ptr<Overloaded_Function_Declaration>> functions;
        anton::Array<Owning_Ptr<Variable_Declaration>> variables;
    };

    [[nodiscard]] Builtin_Declarations get_builtin_declarations(Context& ctx);
} // namespace vush
