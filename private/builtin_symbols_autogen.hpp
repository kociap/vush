#pragma once

#include <ast.hpp>
#include <owning_ptr.hpp>

namespace vush {
    [[nodiscard]] Array<Owning_Ptr<Function_Declaration>> get_builtin_functions_declarations(Allocator* const allocator);
}
