#pragma once

#include <anton/assert.hpp>
#include <anton/type_traits.hpp>
#include <vush/allocator.hpp>
#include <vush/types.hpp>

namespace vush {
    // Downcast_Tag
    // Tag for Owning_Ptr constructor that downcasts the pointer type.
    //
    struct Downcast_Tag {
        explicit Downcast_Tag() = default; // Explicit constructor so that it may not be constructed via {}
    };
    inline constexpr Downcast_Tag downcast;

    struct Deleter {
        i64 size;
        i64 alignment;
        Allocator* allocator;

        Deleter(Allocator* allocator, i64 size, i64 alignment): size(size), alignment(alignment), allocator(allocator) {}

        void operator()(void* v) {
            allocator->deallocate(v, size, alignment);
        }
    };

    template<typename T>
    struct Owning_Ptr {
    public:
        Owning_Ptr(nullptr_t): _pointer(nullptr), _deleter(nullptr, 0, 0) {}
        explicit Owning_Ptr(T* ptr, Allocator* allocator): _pointer(ptr), _deleter(allocator, sizeof(T), alignof(T)) {}

        template<typename U>
        explicit Owning_Ptr(Downcast_Tag, U* ptr, Allocator* allocator): _pointer(static_cast<T*>(ptr)), _deleter(allocator, sizeof(T), alignof(T)) {}

        template<typename U>
        explicit Owning_Ptr(Downcast_Tag, Owning_Ptr<U>&& other): _pointer(static_cast<T*>(other.release())), _deleter(other._deleter) {}

        Owning_Ptr(Owning_Ptr const&) = delete;
        Owning_Ptr& operator=(Owning_Ptr const&) = delete;

        Owning_Ptr(Owning_Ptr&& other): _pointer(other.release()), _deleter(other._deleter) {}

        Owning_Ptr& operator=(Owning_Ptr&& other) {
            ANTON_ASSERT(this != &other, u8"self-move-assignment of owning_ptr");
            if(_pointer) {
                _deleter(_pointer);
            }
            _pointer = other.release();
            _deleter = other._deleter;
            return *this;
        }

        template<typename U>
        Owning_Ptr(Owning_Ptr<U>&& other, anton::enable_if<anton::is_convertible<U*, T*>, anton::i32> = 0)
            : _pointer(other.release()), _deleter(other._deleter) {}

        template<typename U>
        anton::enable_if<anton::is_convertible<U*, T*>, Owning_Ptr&> operator=(Owning_Ptr<U>&& other) {
            ANTON_ASSERT((void*)this != (void*)&other, u8"self-move-assignment of owning_ptr");
            if(_pointer) {
                _deleter(_pointer);
            }
            _pointer = other.release();
            _deleter = other._deleter;
            return *this;
        }

        ~Owning_Ptr() {
            if(_pointer) {
                _deleter(_pointer);
            }
        }

        // Allow upcasting
        template<typename U, anton::enable_if<anton::is_convertible<T*, U*>, int> = 0>
        [[nodiscard]] operator Owning_Ptr<U>&() & {
            return reinterpret_cast<Owning_Ptr<U>&>(*this);
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
        Deleter _deleter;

        template<typename U>
        friend struct Owning_Ptr;
    };

    template<typename T>
    Owning_Ptr(T*, Allocator*) -> Owning_Ptr<T>;
} // namespace vush
