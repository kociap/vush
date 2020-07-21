#include <string_stream.hpp>

namespace vush {
    Input_String_Stream::Input_String_Stream(anton::String string): _string(anton::move(string)), _current(_string.chars_begin()) {}

    [[nodiscard]] Input_String_Stream::operator bool() const {
        return true;
    }

    void Input_String_Stream::read(void*, i64) {
        // TODO: Implement. We currently do not use those, so I just skipped those.
    }

    void Input_String_Stream::read(anton::Slice<u8>) {
        // TODO: Implement. We currently do not use those, so I just skipped those.
    }

    char32 Input_String_Stream::peek() {
        if(_current != _string.chars_end()) {
            return *_current;
        } else {
            return anton::eof_char32;
        }
    }

    char32 Input_String_Stream::get() {
        if(_current != _string.chars_end()) {
            char32 c = *_current;
            ++_current;
            return c;
        } else {
            return anton::eof_char32;
        }
    }

    void Input_String_Stream::unget() {
        if(_current != _string.chars_begin()) {
            --_current;
        }
    }

    void Input_String_Stream::seek(anton::Seek_Dir dir, i64 offset) {
        if(dir == anton::Seek_Dir::beg) {
            auto begin = _string.chars_begin();
            _current = anton::UTF8_Char_Iterator{begin.get_underlying_pointer() + offset, offset};
        }
    }

    i64 Input_String_Stream::tell() {
        return _current.get_offset();
    }
} // namespace vush
