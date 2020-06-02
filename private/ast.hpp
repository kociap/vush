#pragma once

#include <owning_ptr.hpp>
#include <vush/types.hpp>

#include <ostream>
#include <string>
#include <vector>

namespace vush {
    struct Indent {
        i64 indent_count = 0;
    };

    std::ostream& operator<<(std::ostream& stream, Indent indent);

    class Syntax_Tree_Node {
    public:
        virtual ~Syntax_Tree_Node() = default;
        virtual void print(std::ostream& stream, Indent indent) const = 0;
    };

    class Identifier: public Syntax_Tree_Node {
    public:
        Identifier(std::string const& string): _name(string) {}
        Identifier(std::string&& string): _name(std::move(string)) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Identifier: '" << _name << "'\n";
        }

    private:
        std::string _name;
    };

    class Type: public Syntax_Tree_Node {
    public:
        Type(std::string name): _name(std::move(name)) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Type: '" << _name << "'\n";
        }

    private:
        std::string _name;
    };

    class Expression;
    class Declaration: public Syntax_Tree_Node {};

    class Declaration_List: public Syntax_Tree_Node {
    public:
        void append(Declaration* const declaration) {
            _declarations.emplace_back(declaration);
        }

        [[nodiscard]] i64 size() const {
            return _declarations.size();
        }

        virtual void print(std::ostream& stream, Indent const indent) const override;

    private:
        std::vector<Owning_Ptr<Declaration>> _declarations;
    };

    class Declaration_If: public Declaration {
    public:
        Declaration_If(Expression* condition, Declaration_List* true_declarations, Declaration_List* false_declarations)
            : _condition(condition), _true_declarations(true_declarations), _false_declarations(false_declarations) {}

        virtual void print(std::ostream& stream, Indent const indent) const override;

    private:
        Owning_Ptr<Expression> _condition;
        Owning_Ptr<Declaration_List> _true_declarations;
        Owning_Ptr<Declaration_List> _false_declarations;
    };

    class Expression;

    class Variable_Declaration: public Declaration {
    public:
        Variable_Declaration(Type* type, Identifier* identifier, Expression* initializer): _Type(type), _identifier(identifier), _initializer(initializer) {}

        virtual void print(std::ostream& stream, Indent const indent) const override;

    private:
        Owning_Ptr<Type> _Type = nullptr;
        Owning_Ptr<Identifier> _identifier = nullptr;
        Owning_Ptr<Expression> _initializer = nullptr;
    };

    class Statement_List;

    class Function_Body: public Syntax_Tree_Node {
    public:
        Function_Body(Statement_List* statement_list): _statements(statement_list) {}

        virtual void print(std::ostream& stream, Indent const indent) const override;

    private:
        Owning_Ptr<Statement_List> _statements;
    };

    class Function_Parameter: public Syntax_Tree_Node {
    public:
        Function_Parameter(Identifier* identifier, Type* type): _identifier(identifier), _type(type) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Function Parameter:\n";
            _identifier->print(stream, {indent.indent_count + 1});
            _type->print(stream, {indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Identifier> _identifier;
        Owning_Ptr<Type> _type;
    };

    class Function_Parameter_List: public Syntax_Tree_Node {
    public:
        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Function Parameter List:\n";
            for(Owning_Ptr<Function_Parameter> const& param: _params) {
                param->print(stream, {indent.indent_count + 1});
            }
        }

        void append_parameter(Function_Parameter* parameter) {
            _params.push_back(parameter);
        }

        i64 get_parameter_count() const {
            return _params.size();
        }

    private:
        std::vector<Owning_Ptr<Function_Parameter>> _params;
    };

    class Function_Declaration: public Declaration {
    public:
        Function_Declaration(Identifier* name, Function_Parameter_List* function_parameter_list, Type* return_type, Function_Body* body)
            : _name(name), _parameter_list(function_parameter_list), _return_type(return_type), _body(body) {}

        virtual void print(std::ostream& stream, Indent const indent) const override;

    private:
        Owning_Ptr<Identifier> _name;
        Owning_Ptr<Function_Parameter_List> _parameter_list;
        Owning_Ptr<Type> _return_type;
        Owning_Ptr<Function_Body> _body;
    };

    class Pass_Stage_Parameter: public Syntax_Tree_Node {
    public:
        Pass_Stage_Parameter(Identifier* identifier, Type* type, Identifier* source): _identifier(identifier), _type(type), _source(source) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Pass_Stage_Parameter:\n";
            stream << Indent{indent.indent_count + 1} << "Name:\n";
            _identifier->print(stream, {indent.indent_count + 2});
            stream << Indent{indent.indent_count + 1} << "Type:\n";
            _type->print(stream, {indent.indent_count + 2});
            if(_source) {
                stream << Indent{indent.indent_count + 1} << "Source:\n";
                _source->print(stream, {indent.indent_count + 2});
            }
        }

    private:
        Owning_Ptr<Identifier> _identifier;
        Owning_Ptr<Type> _type;
        Owning_Ptr<Identifier> _source;
    };

    class Pass_Stage_Parameter_List: public Syntax_Tree_Node {
    public:
        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Pass_Stage_Parameter_List:\n";
            for(Owning_Ptr<Pass_Stage_Parameter> const& param: _params) {
                param->print(stream, {indent.indent_count + 1});
            }
        }

        void append_parameter(Pass_Stage_Parameter* parameter) {
            _params.push_back(parameter);
        }

        i64 get_parameter_count() const {
            return _params.size();
        }

    private:
        std::vector<Owning_Ptr<Pass_Stage_Parameter>> _params;
    };

    class Pass_Stage_Declaration: public Declaration {
    public:
        Pass_Stage_Declaration(Identifier* pass, Identifier* name, Pass_Stage_Parameter_List* parameter_list, Type* return_type, Function_Body* body)
            : _pass(pass), _name(name), _parameter_list(parameter_list), _return_type(return_type), _body(body) {}

        virtual void print(std::ostream& stream, Indent const indent) const override;

    private:
        Owning_Ptr<Identifier> _pass;
        Owning_Ptr<Identifier> _name;
        Owning_Ptr<Pass_Stage_Parameter_List> _parameter_list;
        Owning_Ptr<Type> _return_type;
        Owning_Ptr<Function_Body> _body;
    };

    class Expression: public Syntax_Tree_Node {};

    class Expression_If: public Expression {
    public:
        Expression_If(Expression* condition, Expression* true_expression, Expression* false_expression)
            : _condition(condition), _true_expression(true_expression), _false_expression(false_expression) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Expression_If:\n";
            _condition->print(stream, Indent{indent.indent_count + 1});
            _true_expression->print(stream, Indent{indent.indent_count + 1});
            if(_false_expression) {
                _false_expression->print(stream, Indent{indent.indent_count + 1});
            }
        }

    private:
        Owning_Ptr<Expression> _condition;
        Owning_Ptr<Expression> _true_expression;
        Owning_Ptr<Expression> _false_expression;
    };

    class Identifier_Expression: public Expression {
    public:
        Identifier_Expression(Identifier* identifier): _identifier(identifier) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Identifier_Expression:\n";
            _identifier->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Identifier> _identifier;
    };

    class Assignment_Expression: public Expression {
    public:
        Assignment_Expression(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Assignment_Expression:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    enum class Arithmetic_Assignment_Type {
        plus,
        minus,
        multiply,
        divide,
        remainder,
        lshift,
        rshift,
        bit_and,
        bit_or,
        bit_xor,
    };

    class Arithmetic_Assignment_Expression: public Expression {
    public:
        Arithmetic_Assignment_Expression(Arithmetic_Assignment_Type type, Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs), _type(type) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            char const* repr = nullptr;
            switch(_type) {
                case Arithmetic_Assignment_Type::plus:
                    repr = u8"+=";
                    break;
                case Arithmetic_Assignment_Type::minus:
                    repr = u8"-=";
                    break;
                case Arithmetic_Assignment_Type::multiply:
                    repr = u8"*=";
                    break;
                case Arithmetic_Assignment_Type::divide:
                    repr = u8"/=";
                    break;
                case Arithmetic_Assignment_Type::remainder:
                    repr = u8"%=";
                    break;
                case Arithmetic_Assignment_Type::lshift:
                    repr = u8"<<=";
                    break;
                case Arithmetic_Assignment_Type::rshift:
                    repr = u8">>=";
                    break;
                case Arithmetic_Assignment_Type::bit_and:
                    repr = u8"&=";
                    break;
                case Arithmetic_Assignment_Type::bit_or:
                    repr = u8"|=";
                    break;
                case Arithmetic_Assignment_Type::bit_xor:
                    repr = u8"^=";
                    break;
            }

            stream << indent << "Arithmetic_Assignment_Expression (" << repr << "):\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
        Arithmetic_Assignment_Type _type;
    };

    class Logic_Or_Expr: public Expression {
    public:
        Logic_Or_Expr(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Logic_Or_Expr:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    class Logic_Xor_Expr: public Expression {
    public:
        Logic_Xor_Expr(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Logic_Xor_Expr:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    class Logic_And_Expr: public Expression {
    public:
        Logic_And_Expr(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Logic_And_Expr:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    class Relational_Equality_Expression: public Expression {
    public:
        Relational_Equality_Expression(bool is_equality, Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs), _is_equality(is_equality) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Relational_Expression (" << (_is_equality ? "==" : "!=") << "):\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
        bool _is_equality;
    };

    enum class Relational_Type {
        greater_than,
        less_than,
        greater_equal,
        less_equal,
    };

    class Relational_Expression: public Expression {
    public:
        Relational_Expression(Relational_Type type, Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs), _type(type) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            char const* repr = nullptr;
            if(_type == Relational_Type::greater_than) {
                repr = u8">";
            } else if(_type == Relational_Type::greater_equal) {
                repr = u8">=";
            } else if(_type == Relational_Type::less_than) {
                repr = u8"<";
            } else {
                repr = u8"<=";
            }
            stream << indent << "Relational_Expression (" << repr << "):\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
        Relational_Type _type;
    };

    class Add_Expr: public Expression {
    public:
        Add_Expr(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Add_Expr:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    class Sub_Expr: public Expression {
    public:
        Sub_Expr(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Sub_Expr:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    class Mul_Expr: public Expression {
    public:
        Mul_Expr(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Mul_Expr:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    class Div_Expr: public Expression {
    public:
        Div_Expr(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Div_Expr:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    class Mod_Expr: public Expression {
    public:
        Mod_Expr(Expression* lhs, Expression* rhs): _lhs(lhs), _rhs(rhs) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Mod_Expr:\n";
            _lhs->print(stream, Indent{indent.indent_count + 1});
            _rhs->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _lhs;
        Owning_Ptr<Expression> _rhs;
    };

    enum class Unary_Type {
        plus,
        minus,
        bit_not,
        logic_not,
    };

    class Unary_Expression: public Expression {
    public:
        Unary_Expression(Unary_Type type, Expression* expression): _expression(expression), _type(type) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            char const* repr = nullptr;
            switch(_type) {
                case Unary_Type::plus:
                    repr = u8"+";
                    break;
                case Unary_Type::minus:
                    repr = u8"-";
                    break;
                case Unary_Type::bit_not:
                    repr = u8"~";
                    break;
                case Unary_Type::logic_not:
                    repr = u8"!";
                    break;
            }

            stream << indent << "Unary_Expression (" << repr << "):\n";
            _expression->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _expression;
        Unary_Type _type;
    };

    class Prefix_Inc_Dec_Expression: public Expression {
    public:
        Prefix_Inc_Dec_Expression(bool is_inc, Expression* expression): _expression(expression), _is_inc(is_inc) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Prefix_Inc_Dec_Expression (" << (_is_inc ? "++" : "--") << "):\n";
            _expression->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _expression;
        bool _is_inc;
    };

    class Argument_List: public Syntax_Tree_Node {
    public:
        void append(Expression* argument) {
            _arguments.emplace_back(argument);
        }

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Argument_List:\n";
            for(auto& argument: _arguments) {
                argument->print(stream, Indent{indent.indent_count + 1});
            }
        }

    private:
        std::vector<Owning_Ptr<Expression>> _arguments;
    };

    class Function_Call_Expression: public Expression {
    public:
        Function_Call_Expression(Identifier* identifier, Argument_List* arg_list): _identifier(identifier), _arg_list(arg_list) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Function_Call_Expression:\n";
            _identifier->print(stream, Indent{indent.indent_count + 1});
            _arg_list->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Identifier> _identifier;
        Owning_Ptr<Argument_List> _arg_list;
    };

    class Member_Access_Expression: public Expression {
    public:
        Member_Access_Expression(Expression* base, Identifier* member): _base(base), _member(member) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Member_Access_Expression:\n";
            _base->print(stream, Indent{indent.indent_count + 1});
            _member->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _base;
        Owning_Ptr<Identifier> _member;
    };

    class Array_Access_Expression: public Expression {
    public:
        Array_Access_Expression(Expression* base, Expression* index): _base(base), _index(index) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Array_Access_Expression:\n";
            _base->print(stream, Indent{indent.indent_count + 1});
            _index->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _base;
        Owning_Ptr<Expression> _index;
    };

    class Postfix_Inc_Dec_Expression: public Expression {
    public:
        Postfix_Inc_Dec_Expression(bool is_inc, Expression* base): _base(base), _is_inc(is_inc) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Postfix_Inc_Dec_Expression (" << (_is_inc ? u8"++" : u8"--") << "):\n";
            _base->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _base;
        bool _is_inc;
    };

    class Bool_Literal: public Expression {
    public:
        Bool_Literal(bool value): _value(value) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Bool_Literal: " << (_value ? "true" : "false") << "\n";
        }

    private:
        bool _value;
    };

    class Integer_Literal: public Expression {
    public:
        Integer_Literal(std::string value): _value(std::move(value)) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Integer_Literal: " << _value << "\n";
        }

    private:
        std::string _value;
    };

    class Float_Literal: public Expression {
    public:
        Float_Literal(std::string value): _value(std::move(value)) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Float_Literal: " << _value << "\n";
        }

    private:
        std::string _value;
    };

    class Statement: public Syntax_Tree_Node {};

    class Statement_List: public Syntax_Tree_Node {
    public:
        void append(Statement* const statement) {
            _statements.emplace_back(statement);
        }

        [[nodiscard]] i64 size() const {
            return _statements.size();
        }

        virtual void print(std::ostream& stream, Indent const indent) const override;

    private:
        std::vector<Owning_Ptr<Statement>> _statements;
    };

    class Block_Statement: public Statement {
    public:
        Block_Statement(Statement_List* statements): _statements(statements) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Block_Statement:\n";
            if(_statements) {
                _statements->print(stream, Indent{indent.indent_count + 1});
            }
        }

    private:
        Owning_Ptr<Statement_List> _statements;
    };

    class If_Statement: public Statement {
    public:
        If_Statement(Expression* condition, Statement* true_statement, Statement* false_statement)
            : _condition(condition), _true_statement(true_statement), _false_statement(false_statement) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "If_Statement:\n";
            _condition->print(stream, Indent{indent.indent_count + 1});
            _true_statement->print(stream, Indent{indent.indent_count + 1});
            if(_false_statement) {
                _false_statement->print(stream, Indent{indent.indent_count + 1});
            }
        }

    private:
        Owning_Ptr<Expression> _condition;
        Owning_Ptr<Statement> _true_statement;
        Owning_Ptr<Statement> _false_statement;
    };

    class Case_Statement: public Statement {
    public:
        Case_Statement(Expression* condition, Statement_List* statements): _condition(condition), _statements(statements) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Case_Statement:\n";
            _condition->print(stream, Indent{indent.indent_count + 1});
            _statements->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _condition;
        Owning_Ptr<Statement_List> _statements;
    };

    class Default_Case_Statement: public Statement {
    public:
        Default_Case_Statement(Statement_List* statements): _statements(statements) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Default_Case_Statement:\n";
            _statements->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Statement_List> _statements;
    };

    class Switch_Statement: public Statement {
    public:
        void append(Case_Statement* case_stmt) {
            _cases.emplace_back(case_stmt);
        }

        void append(Default_Case_Statement* case_stmt) {
            _cases.emplace_back(case_stmt);
        }

        i64 case_count() const {
            return _cases.size();
        }

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Switch_Statement:\n";
            for(auto& statement: _cases) {
                statement->print(stream, Indent{indent.indent_count + 1});
            }
        }

    private:
        std::vector<Owning_Ptr<Statement>> _cases;
    };

    class For_Statement: public Statement {
    public:
        For_Statement(Variable_Declaration* declaration, Expression* condition, Expression* post_expression, Block_Statement* block)
            : _declaration(declaration), _condition(condition), _block(block), _post_expression(post_expression) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "For_Statement:\n";
            _declaration->print(stream, Indent{indent.indent_count + 1});
            _condition->print(stream, Indent{indent.indent_count + 1});
            _post_expression->print(stream, Indent{indent.indent_count + 1});
            _block->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Variable_Declaration> _declaration;
        Owning_Ptr<Expression> _condition;
        Owning_Ptr<Expression> _post_expression;
        Owning_Ptr<Block_Statement> _block;
    };

    class While_Statement: public Statement {
    public:
        While_Statement(Expression* condition, Block_Statement* block): _condition(condition), _block(block) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "While_Statement:\n";
            _condition->print(stream, Indent{indent.indent_count + 1});
            _block->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _condition;
        Owning_Ptr<Block_Statement> _block;
    };

    class Do_While_Statement: public Statement {
    public:
        Do_While_Statement(Expression* condition, Block_Statement* block): _condition(condition), _block(block) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Do_While_Statement:\n";
            _condition->print(stream, Indent{indent.indent_count + 1});
            _block->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _condition;
        Owning_Ptr<Block_Statement> _block;
    };

    class Return_Statement: public Statement {
    public:
        Return_Statement(Expression* return_expr): _return_expr(return_expr) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Return_Statement:\n";
            _return_expr->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _return_expr;
    };

    class Break_Statement: public Statement {
    public:
        Break_Statement() {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Break_Statement\n";
        }
    };

    class Continue_Statement: public Statement {
    public:
        Continue_Statement() {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Continue_Statement\n";
        }
    };

    class Declaration_Statement: public Statement {
    public:
        Declaration_Statement(Variable_Declaration* var_decl): _var_decl(var_decl) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Declaration_Statement (Variable Declaration):\n";
            _var_decl->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Variable_Declaration> _var_decl;
    };

    class Expression_Statement: public Statement {
    public:
        Expression_Statement(Expression* expression): _expr(expression) {}

        virtual void print(std::ostream& stream, Indent const indent) const override {
            stream << indent << "Expression_Statement:\n";
            _expr->print(stream, Indent{indent.indent_count + 1});
        }

    private:
        Owning_Ptr<Expression> _expr;
    };
} // namespace vush