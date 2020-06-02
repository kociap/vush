#include <ast.hpp>

namespace vush {
    std::ostream& operator<<(std::ostream& stream, Indent indent) {
        for(i64 i = 0; i < indent.indent_count; ++i) {
            stream << "  ";
        }
        return stream;
    }

    void Declaration_List::print(std::ostream& stream, Indent const indent) const {
        for(auto& declaration: _declarations) {
            declaration->print(stream, indent);
        }
    }

    void Declaration_If::print(std::ostream& stream, Indent const indent) const {
        stream << indent << "Declaration_If:\n";
        _condition->print(stream, Indent{indent.indent_count + 1});
        _true_declarations->print(stream, Indent{indent.indent_count + 1});
        if(_false_declarations) {
            _false_declarations->print(stream, Indent{indent.indent_count + 1});
        }
    }

    void Variable_Declaration::print(std::ostream& stream, Indent const indent) const {
        stream << indent << "Variable Declaration:\n";
        _Type->print(stream, {indent.indent_count + 1});
        _identifier->print(stream, {indent.indent_count + 1});
        if(_initializer) {
            _initializer->print(stream, {indent.indent_count + 1});
        }
    }

    void Function_Body::print(std::ostream& stream, Indent const indent) const {
        stream << indent << "Function Body:\n";
        if(_statements) {
            _statements->print(stream, Indent{indent.indent_count + 1});
        }
    }

    void Function_Declaration::print(std::ostream& stream, Indent const indent) const {
        stream << indent << "Function Declaration:\n";
        stream << Indent{indent.indent_count + 1} << "Name:\n";
        _name->print(stream, Indent{indent.indent_count + 2});
        stream << Indent{indent.indent_count + 1} << "Parameter List:\n";
        _parameter_list->print(stream, {indent.indent_count + 2});
        stream << Indent{indent.indent_count + 1} << "Return_Type:\n";
        _return_type->print(stream, Indent{indent.indent_count + 2});
        stream << Indent{indent.indent_count + 1} << "Body:\n";
        _body->print(stream, Indent{indent.indent_count + 2});
    }

    void Pass_Stage_Declaration::print(std::ostream& stream, Indent const indent) const {
        stream << indent << "Pass_Stage_Declaration:\n";
        stream << Indent{indent.indent_count + 1} << "Pass:\n";
        _pass->print(stream, Indent{indent.indent_count + 2});
        stream << Indent{indent.indent_count + 1} << "Name:\n";
        _name->print(stream, Indent{indent.indent_count + 2});
        stream << Indent{indent.indent_count + 1} << "Parameter List:\n";
        _parameter_list->print(stream, {indent.indent_count + 2});
        stream << Indent{indent.indent_count + 1} << "Return_Type:\n";
        _return_type->print(stream, Indent{indent.indent_count + 2});
        stream << Indent{indent.indent_count + 1} << "Body:\n";
        _body->print(stream, Indent{indent.indent_count + 2});
    }

    void Statement_List::print(std::ostream& stream, Indent const indent) const {
        for(auto& statement: _statements) {
            statement->print(stream, indent);
        }
    }
} // namespace vush