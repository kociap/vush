#pragma once

#include <anton/stream.hpp>
#include <anton/string.hpp>
#include <vush/types.hpp>

namespace vush {
    class Input_String_Stream: public anton::Input_Stream {
    public:
        Input_String_Stream(anton::String string);

        [[nodiscard]] virtual operator bool() const override;

        virtual void read(void* buffer, i64 count) override;
        virtual void read(anton::Slice<u8> buffer) override;
        virtual char32 peek() override;
        virtual char32 get() override;
        virtual void unget() override;
        // The only valid value for offset is a value previously returned by tell.
        // The only allowed value for dir is Seek_Dir::beg. If dir is not Seek_Dir::beg, this function does nothing.
        virtual void seek(anton::Seek_Dir dir, i64 offset) override;
        virtual i64 tell() override;

    private:
        anton::String _string;
        anton::UTF8_Char_Iterator _current;
    };
} // namespace vush
