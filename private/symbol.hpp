#pragma once

namespace vush {
    enum struct Symbol_Type {
        struct_decl,
        variable,
        constant,
        function,
        pass_stage,
    };

    struct Declaration;

    struct Symbol {
        Symbol_Type type;
        Declaration* declaration;
    };
} // namespace vush
