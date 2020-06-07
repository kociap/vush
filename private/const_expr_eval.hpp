#pragma once

#include <ast.hpp>
#include <context.hpp>
#include <owning_ptr.hpp>
#include <vush/expected.hpp>
#include <vush/types.hpp>

namespace vush {
    enum struct Expr_Value_Type {
        boolean,
        int32,
    };

    struct Const_Expr_Value {
        Expr_Value_Type type;
        union {
            bool boolean;
            i32 int32;
        };

        bool as_boolean() const {
            switch(type) {
                case Expr_Value_Type::boolean:
                    return boolean;

                case Expr_Value_Type::int32:
                    return int32;
            }
        }
    };

    bool is_implicitly_convertible_to_boolean(Expr_Value_Type type);
    Expected<Const_Expr_Value, String> evaluate_expr(Context& ctx, Expression& expression);
} // namespace vush
