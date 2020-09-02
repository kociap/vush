#pragma once

#include <anton/expected.hpp>
#include <ast.hpp>
#include <context.hpp>
#include <owning_ptr.hpp>

namespace vush {
    enum struct Expr_Value_Type {
        boolean,
        int32,
        uint32,
        float32,
        float64,
    };

    struct Expr_Value {
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
                    return static_cast<T>(boolean);

                case Expr_Value_Type::int32:
                    return static_cast<T>(int32);

                case Expr_Value_Type::uint32:
                    return static_cast<T>(uint32);

                case Expr_Value_Type::float32:
                    return static_cast<T>(float32);

                case Expr_Value_Type::float64:
                    return static_cast<T>(float64);
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

        f32 as_float32() const {
            return as<f32>();
        }

        f64 as_float64() const {
            return as<f64>();
        }
    };

    [[nodiscard]] bool is_implicitly_convertible_to_boolean(Expr_Value_Type type);
    anton::Expected<bool, anton::String> is_compiletime_evaluable(Context& ctx, Expression& expression);
    anton::Expected<Expr_Value, anton::String> evaluate_const_expr(Context& ctx, Expression& expression);
} // namespace vush
