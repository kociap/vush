#pragma once

namespace vush {
    using char8 = char;
    using char32 = char32_t;

    using i8 = signed char;
    using i16 = signed short;
    using i32 = signed int;
    using i64 = signed long long;

    using u8 = unsigned char;
    using u16 = unsigned short;
    using u32 = unsigned int;
    using u64 = unsigned long long;

    using f32 = float;
    using f64 = double;

    // Immutable string
    class String {
    public:
        String() = default;
        String(char8 const* string, i64 size): _data(string), _size(size) {}
        String(String&& str): _data(str._data), _size(str._size) {
            str._data = nullptr;
            str._size = 0;
        }

        ~String();

        char8 const* data() const {
            return _data;
        }

        i64 size() const {
            return _size;
        }

    private:
        char8 const* _data = nullptr;
        i64 _size = 0;
    };
} // namespace vush