#include <vush_ir/decoration.hpp>

namespace vush::ir {
  Decoration_Argument::Decoration_Argument(Decoration_Argument&& other)
    : kind(other.kind)
  {
    switch(kind) {
    case e_string:
      anton::construct(&value_string, ANTON_MOV(other.value_string));
      break;
    case e_u64:
      value_u64 = other.value_u64;
      break;
    case e_none:
      break;
    }
  }

  Decoration_Argument::~Decoration_Argument()
  {
    reset();
  }

  Decoration_Argument::Kind Decoration_Argument::get_kind() const
  {
    return kind;
  }

  u64& Decoration_Argument::get_u64()
  {
    ANTON_ASSERT(kind == e_u64, "literal is not u64");
    return value_u64;
  }

  u64 Decoration_Argument::get_u64() const
  {
    ANTON_ASSERT(kind == e_u64, "literal is not u64");
    return value_u64;
  }

  anton::String& Decoration_Argument::get_string()
  {
    ANTON_ASSERT(kind == e_string, "literal is not string");
    return value_string;
  }

  anton::String const& Decoration_Argument::get_string() const
  {
    ANTON_ASSERT(kind == e_string, "literal is not string");
    return value_string;
  }

  void Decoration_Argument::reset()
  {
    switch(kind) {
    case Kind::e_string:
      anton::destruct(&value_string);
      break;

    case Kind::e_u64:
    case Kind::e_none:
      break;
    }

    kind = e_none;
  }

  void Decoration_Argument::set(u64 value)
  {
    reset();
    kind = e_u64;
    value_u64 = value;
  }

  void Decoration_Argument::set(anton::String&& value)
  {
    reset();
    kind = e_string;
    anton::construct(&value_string, ANTON_MOV(value));
  }
} // namespace vush::ir
