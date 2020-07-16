#pragma once

#include <anton/assert.hpp>
#include <anton/type_traits.hpp>

namespace vush {
    template<typename T>
    struct Owning_Ptr {
    public:
        Owning_Ptr(): _pointer(nullptr) {}
        Owning_Ptr(T* ptr): _pointer(ptr) {}
        Owning_Ptr(Owning_Ptr const&) = delete;
        Owning_Ptr& operator=(Owning_Ptr const&) = delete;

        Owning_Ptr(Owning_Ptr&& other): _pointer(other.release()) {}

        Owning_Ptr& operator=(Owning_Ptr&& other) {
            ANTON_ASSERT(this != &other, u8"self-move-assignment of owning_ptr");
            T* tmp = _pointer;
            _pointer = other.release();
            delete tmp;
            return *this;
        }

        template<typename U>
        Owning_Ptr(Owning_Ptr<U>&& other, anton::enable_if<anton::is_convertible<U*, T*>, anton::i32> = 0): _pointer(other.release()) {}

        template<typename U>
        anton::enable_if<anton::is_convertible<U*, T*>, Owning_Ptr&> operator=(Owning_Ptr<U>&& other) {
            ANTON_ASSERT((void*)this != (void*)&other, u8"self-move-assignment of owning_ptr");
            T* tmp = _pointer;
            _pointer = other.release();
            delete tmp;
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
