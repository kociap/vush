#pragma once

#include <anton/string_view.hpp>

#include <vush_core/types.hpp>

namespace vush {
  // Running_Hash
  //
  // Implements the murmurhash.
  //
  struct Running_Hash {
  private:
    static constexpr u64 m = 0xc6a4a7935bd1e995;
    static constexpr i64 r = 47;

    u64 h = 0;
    u64 buffer = 0;
    i64 buffer_length = 0;

  public:
    void start(u64 seed = 0x1F0D3804)
    {
      h = seed;
      buffer = 0;
      buffer_length = 0;
    }

    u64 finish()
    {
      switch(buffer_length) {
      case 7:
        h ^= buffer & 0xFF000000000000;
      case 6:
        h ^= buffer & 0xFF0000000000;
      case 5:
        h ^= buffer & 0xFF00000000;
      case 4:
        h ^= buffer & 0xFF000000;
      case 3:
        h ^= buffer & 0xFF0000;
      case 2:
        h ^= buffer & 0xFF00;
      case 1:
        h ^= buffer & 0xFF;
        h *= m;
      }

      h ^= h >> r;
      h *= m;
      h ^= h >> r;

      return h;
    }

    void feed(anton::String_View const data)
    {
      for(char8 const c: data.bytes()) {
        feed(c);
      }
    }

    void feed(char8 const data)
    {
      feed((u8)data);
    }

    void feed(u8 const data)
    {
      buffer = buffer << 8 | data;
      buffer_length += 1;

      if(buffer_length == 8) {
        flush_buffer();
      }
    }

    void feed(u32 const data)
    {
      if(buffer_length + 4 >= 8) {
        i32 const rem = 8 * ((buffer_length + 4) % 8);
        i32 const size = 32 - rem;
        buffer = buffer << size | data >> (32 - size);
        flush_buffer();
        buffer = data & (((u32)0x1 << rem) - 1);
      } else {
        buffer = buffer << 32 | data;
      }
      buffer_length = (buffer_length + 4) % 8;
    }

    void feed(u64 const data)
    {
      // buffer_length remains unchanged.
      // v + 8 mod 8 = v
      i32 const rem = 64 * buffer_length;
      i32 const size = 64 - rem;
      buffer = buffer << size | data >> (64 - size);
      flush_buffer();
      buffer = data & (((u32)0x1 << rem) - 1);
    }

  private:
    void flush_buffer()
    {
      buffer *= m;
      buffer ^= buffer >> r;
      buffer *= m;

      h ^= buffer;
      h *= m;

      buffer = 0;
      buffer_length = 0;
    }
  };
} // namespace vush
