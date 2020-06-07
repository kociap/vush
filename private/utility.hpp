#pragma once

#include <string>
#include <string_view>

namespace vush {
    [[nodiscard]] inline String build_error_message(std::string const& path, i64 const line, i64 const column, std::string const& message) {
        std::string msg = std::move(path) + u8":" + std::to_string(line + 1) + u8":" + std::to_string(column + 1) + u8": error: " + message;
        char* str_data = (char*)::operator new(msg.size() + 1);
        memcpy(str_data, msg.data(), msg.size() + 1);
        return {str_data, (i64)msg.size()};
    }

    [[nodiscard]] constexpr i64 str_to_i64(std::string_view const string, u64 const base = 10) {
        // Convert ['0', '9'] ['A', 'Z'] ['a', 'z'] to [0, 35], everything else to 255.
        constexpr u8 byte_to_digit[] = {
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0,   1,   2,   3,   4,   5,   6,   7,   8,   9,
            255, 255, 255, 255, 255, 255, 255, 10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,
            32,  33,  34,  35,  255, 255, 255, 255, 255, 255, 10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,
            29,  30,  31,  32,  33,  34,  35,  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
            255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255};

        char8 const* first = string.data();
        bool negative = false;
        if(*first == '-' || *first == '+') {
            negative = (*first == '-');
            ++first;
        }

        i64 number = 0;
        char8 const* last = string.data() + string.size();
        for(; first != last; ++first) {
            u8 const mapped = byte_to_digit[static_cast<u8>(*first)];
            if(mapped <= base) {
                number = number * base + mapped;
            } else {
                break;
            }
        }

        if(negative) {
            number = -number;
        }

        return number;
    }
} // namespace vush
