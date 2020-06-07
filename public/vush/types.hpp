#pragma once

namespace vush {
    using char8 = char;
    using char32 = char32_t;

    using i32 = int;
    using i64 = long long;

    using u8 = unsigned char;
    using u32 = unsigned int;
    using u64 = unsigned long long;

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