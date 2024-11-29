#pragma once

#include <anton/ilist.hpp>
#include <anton/string.hpp>

#include <vush_core/types.hpp>

namespace vush::ir {
  struct Decoration_Argument {
  public:
    enum Kind {
      e_none,
      e_u64,
      e_string,
    };

  private:
    Kind kind;
    union {
      u64 value_u64;
      anton::String value_string;
    };

  public:
    Decoration_Argument(): kind(e_none) {}
    Decoration_Argument(u64 value): kind(e_u64), value_u64(value) {}
    Decoration_Argument(anton::String&& value)
      : kind(e_string), value_string(ANTON_MOV(value))
    {
    }

    Decoration_Argument(Decoration_Argument&& other);

    ~Decoration_Argument();

    Kind get_kind() const;

    [[nodiscard]] u64& get_u64();
    [[nodiscard]] u64 get_u64() const;
    [[nodiscard]] anton::String& get_string();
    [[nodiscard]] anton::String const& get_string() const;

    void reset();
    void set(u64 value);
    void set(anton::String&& value);
  };

  struct Decoration: public anton::IList_Node<Decoration> {
    anton::String identifier;
    Decoration_Argument argument;

    Decoration(anton::String&& identifier)
      : identifier(ANTON_MOV(identifier)), argument()
    {
    }

    Decoration(anton::String&& identifier, Decoration_Argument&& argument)
      : identifier(ANTON_MOV(identifier)), argument(ANTON_MOV(argument))
    {
    }
  };

  struct Decorable {
    Decoration* decorations = nullptr;

    void decorate(Decoration* const decoration)
    {
      if(decorations == nullptr) {
        decorations = decoration;
      } else {
        ilist_insert_before(decorations, decoration);
      }
    }
  };
} // namespace vush::ir
