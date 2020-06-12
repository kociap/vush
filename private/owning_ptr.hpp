#pragma once

#include <anton/assert.hpp>

namespace vush {
    template<typename T>
    struct Owning_Ptr {
    public:
        Owning_Ptr(): _pointer(nullptr) {}
        Owning_Ptr(T* ptr): _pointer(ptr) {}
        Owning_Ptr(Owning_Ptr const&) = delete;
        Owning_Ptr& operator=(Owning_Ptr const&) = delete;

        Owning_Ptr(Owning_Ptr&& other): _pointer(other._pointer) {
            other._pointer = nullptr;
        }

        Owning_Ptr& operator=(Owning_Ptr&& other) {
            T* tmp = _pointer;
            _pointer = other._pointer;
            other._pointer = tmp;
            return *this;
        }

        ~Owning_Ptr() {
            delete _pointer;
        }

        [[nodiscard]] operator bool() const {
            return _pointer;
        }

        [[nodiscard]] T& operator*() const {
            ANTON_ASSERT(_pointer, u8"dereferencing nullptr");
            return *_pointer;
        }

        [[nodiscard]] T* operator->() const {
            ANTON_ASSERT(_pointer, u8"dereferencing nullptr");
            return _pointer;
        }

        [[nodiscard]] T* get() const {
            return _pointer;
        }

        [[nodiscard]] T* release() {
            T* pointer = _pointer;
            _pointer = nullptr;
            return pointer;
        }

    private:
        T* _pointer;
    };

    template<typename T>
    Owning_Ptr(T*) -> Owning_Ptr<T>;
} // namespace vush
