#include <vush_syntax/syntax.hpp>

namespace vush {
  Syntax_Token::Syntax_Token(Syntax_Node_Kind kind, anton::String value,
                             Source_Info const& source_info)
    : value(ANTON_MOV(value)), source_info(source_info), kind(kind)
  {
  }

  Syntax_Node::Syntax_Node(Syntax_Node_Kind kind, Array<SNOT> array, Source_Info const& source_info)
    : children(ANTON_MOV(array)), source_info(source_info), kind(kind)
  {
  }
} // namespace vush
