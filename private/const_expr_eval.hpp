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
        uint32,
        float32,
        float64,
    };

    struct Const_Expr_Value {
        Expr_Value_Type type;
        union {
            bool boolean;
            i32 int32;
            u32 uint32;
            f32 float32;
            f64 float64;
        };

        template<typename T>
        T as() const {
            switch(type) {
                case Expr_Value_Type::boolean:
                    return boolean;

                case Expr_Value_Type::uint32:
                    return uint32;

                case Expr_Value_Type::int32:
                    return int32;

                case Expr_Value_Type::float32:
                    return float32;

                case Expr_Value_Type::float64:
                    return float64;
            }
        }

        bool as_boolean() const {
            return as<bool>();
        }

        i32 as_int32() const {
            return as<i32>();
        }

        u32 as_uint32() const {
            return as<u32>();
        }

        f64 as_float32() const {
            return as<f32>();
        }

        f64 as_float64() const {
            return as<f64>();
        }
    };

    bool is_implicitly_convertible_to_boolean(Expr_Value_Type type);
    Expected<Const_Expr_Value, String> evaluate_expr(Context& ctx, Expression& expression);
} // namespace vush
