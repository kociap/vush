#pragma once

namespace vush {
    struct Expected_Error_Tag {
        explicit Expected_Error_Tag() = default;
    };

    constexpr Expected_Error_Tag expected_error;

    struct Expected_Value_Tag {
        explicit Expected_Value_Tag() = default;
    };

    constexpr Expected_Value_Tag expected_value;

    template<typename T, typename E>
    class Expected {
    public:
        using value_type = T;
        using error_type = E;

        template<typename... Args>
        Expected(Expected_Value_Tag, Args&&... args): _holds_value(true), _value{static_cast<Args&&>(args)...} {}

        template<typename... Args>
        Expected(Expected_Error_Tag, Args&&... args): _holds_value(false), _error{static_cast<Args&&>(args)...} {}

        ~Expected() {
            if(_holds_value) {
                _value.~T();
            } else {
                _error.~E();
            }
        }

        [[nodiscard]] operator bool() const {
            return _holds_value;
        }

        [[nodiscard]] bool holds_value() const {
            return _holds_value;
        }

        [[nodiscard]] T* operator->() {
            return &_value;
        }

        [[nodiscard]] T const* operator->() const {
            return &_value;
        }

        [[nodiscard]] T& operator*() & {
            return _value;
        }

        [[nodiscard]] T const& operator*() const& {
            return _value;
        }

        [[nodiscard]] T&& operator*() && {
            return _value;
        }

        [[nodiscard]] T const&& operator*() const&& {
            return _value;
        }

        [[nodiscard]] T& value() & {
            return _value;
        }

        [[nodiscard]] T const& value() const& {
            return _value;
        }

        [[nodiscard]] T&& value() && {
            return _value;
        }

        [[nodiscard]] T const&& value() const&& {
            return _value;
        }

        [[nodiscard]] E& error() & {
            return _error;
        }

        [[nodiscard]] E const& error() const& {
            return _error;
        }

        [[nodiscard]] E&& error() && {
            return _error;
        }

        [[nodiscard]] E const&& error() const&& {
            return _error;
        }

    private:
        bool _holds_value;
        union {
            T _value;
            E _error;
        };
    };
} // namespace vush
