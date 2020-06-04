#pragma once

#include <owning_ptr.hpp>
#include <vush/types.hpp>

#include <string>
#include <vector>

namespace vush {
    struct Identifier;
    struct Type;
    struct Declaration_List;
    struct Declaration_If;
    struct Import_Decl;
    struct Variable_Declaration;
    struct Struct_Decl;
    struct Function_Body;
    struct Function_Param_List;
    struct Function_Param_If;
    struct Ordinary_Function_Param;
    struct Function_Declaration;
    struct Sourced_Function_Param;
    struct Pass_Stage_Declaration;
    struct Expression;
    struct Expression_If;
    struct Identifier_Expression;
    struct Assignment_Expression;
    struct Arithmetic_Assignment_Expression;
    struct Logic_Or_Expr;
    struct Logic_Xor_Expr;
    struct Logic_And_Expr;
    struct Relational_Equality_Expression;
    struct Relational_Expression;
    struct Bit_Or_Expr;
    struct Bit_Xor_Expr;
    struct Bit_And_Expr;
    struct LShift_Expr;
    struct RShift_Expr;
    struct Add_Expr;
    struct Sub_Expr;
    struct Mul_Expr;
    struct Div_Expr;
    struct Mod_Expr;
    struct Unary_Expression;
    struct Prefix_Inc_Dec_Expression;
    struct Argument_List;
    struct Function_Call_Expression;
    struct Member_Access_Expression;
    struct Array_Access_Expression;
    struct Postfix_Inc_Dec_Expression;
    struct String_Literal;
    struct Bool_Literal;
    struct Integer_Literal;
    struct Float_Literal;
    struct Statement;
    struct Statement_List;
    struct Block_Statement;
    struct If_Statement;
    struct Case_Statement;
    struct Default_Case_Statement;
    struct Switch_Statement;
    struct For_Statement;
    struct While_Statement;
    struct Do_While_Statement;
    struct Return_Statement;
    struct Break_Statement;
    struct Continue_Statement;
    struct Declaration_Statement;
    struct Expression_Statement;

    struct AST_Visitor {
        virtual ~AST_Visitor() = default;

        virtual void visit(Identifier& node) = 0;
        virtual void visit(Type& node) = 0;
        virtual void visit(Declaration_List& node) = 0;
        virtual void visit(Declaration_If& node) = 0;
        virtual void visit(Import_Decl& node) = 0;
        virtual void visit(Variable_Declaration& node) = 0;
        virtual void visit(Struct_Decl& node) = 0;
        virtual void visit(Function_Body& node) = 0;
        virtual void visit(Function_Param_List& node) = 0;
        virtual void visit(Function_Param_If& node) = 0;
        virtual void visit(Ordinary_Function_Param& node) = 0;
        virtual void visit(Function_Declaration& node) = 0;
        virtual void visit(Sourced_Function_Param& node) = 0;
        virtual void visit(Pass_Stage_Declaration& node) = 0;
        virtual void visit(Expression_If& node) = 0;
        virtual void visit(Identifier_Expression& node) = 0;
        virtual void visit(Assignment_Expression& node) = 0;
        virtual void visit(Arithmetic_Assignment_Expression& node) = 0;
        virtual void visit(Logic_Or_Expr& node) = 0;
        virtual void visit(Logic_Xor_Expr& node) = 0;
        virtual void visit(Logic_And_Expr& node) = 0;
        virtual void visit(Relational_Equality_Expression& node) = 0;
        virtual void visit(Relational_Expression& node) = 0;
        virtual void visit(Bit_Or_Expr& node) = 0;
        virtual void visit(Bit_Xor_Expr& node) = 0;
        virtual void visit(Bit_And_Expr& node) = 0;
        virtual void visit(LShift_Expr& node) = 0;
        virtual void visit(RShift_Expr& node) = 0;
        virtual void visit(Add_Expr& node) = 0;
        virtual void visit(Sub_Expr& node) = 0;
        virtual void visit(Mul_Expr& node) = 0;
        virtual void visit(Div_Expr& node) = 0;
        virtual void visit(Mod_Expr& node) = 0;
        virtual void visit(Unary_Expression& node) = 0;
        virtual void visit(Prefix_Inc_Dec_Expression& node) = 0;
        virtual void visit(Argument_List& node) = 0;
        virtual void visit(Function_Call_Expression& node) = 0;
        virtual void visit(Member_Access_Expression& node) = 0;
        virtual void visit(Array_Access_Expression& node) = 0;
        virtual void visit(Postfix_Inc_Dec_Expression& node) = 0;
        virtual void visit(String_Literal& node) = 0;
        virtual void visit(Bool_Literal& node) = 0;
        virtual void visit(Integer_Literal& node) = 0;
        virtual void visit(Float_Literal& node) = 0;
        virtual void visit(Statement_List& node) = 0;
        virtual void visit(Block_Statement& node) = 0;
        virtual void visit(If_Statement& node) = 0;
        virtual void visit(Case_Statement& node) = 0;
        virtual void visit(Default_Case_Statement& node) = 0;
        virtual void visit(Switch_Statement& node) = 0;
        virtual void visit(For_Statement& node) = 0;
        virtual void visit(While_Statement& node) = 0;
        virtual void visit(Do_While_Statement& node) = 0;
        virtual void visit(Return_Statement& node) = 0;
        virtual void visit(Break_Statement& node) = 0;
        virtual void visit(Continue_Statement& node) = 0;
        virtual void visit(Declaration_Statement& node) = 0;
        virtual void visit(Expression_Statement& node) = 0;
    };

    enum struct AST_Node_Type {
        identifier,
        type,
        declaration_list,
        declaration_if,
        import_decl,
        variable_declaration,
        struct_decl,
        function_body,
        function_param_list,
        function_param_if,
        ordinary_function_param,
        function_declaration,
        sourced_function_param,
        pass_stage_declaration,
        expression,
        expression_if,
        identifier_expression,
        assignment_expression,
        arithmetic_assignment_expression,
        logic_or_expr,
        logic_xor_expr,
        logic_and_expr,
        relational_equality_expression,
        relational_expression,
        bit_or_expr,
        bit_xor_expr,
        bit_and_expr,
        lshift_expr,
        rshift_expr,
        add_expr,
        sub_expr,
        mul_expr,
        div_expr,
        mod_expr,
        unary_expression,
        prefix_inc_dec_expression,
        argument_list,
        function_call_expression,
        member_access_expression,
        array_access_expression,
        postfix_inc_dec_expression,
        string_literal,
        bool_literal,
        integer_literal,
        float_literal,
        statement,
        statement_list,
        block_statement,
        if_statement,
        case_statement,
        default_case_statement,
        switch_statement,
        for_statement,
        while_statement,
        do_while_statement,
        return_statement,
        break_statement,
        continue_statement,
        declaration_statement,
        expression_statement,
    };

    struct Source_Information {
        std::string* file_path;
        i64 line;
        i64 column;
        i64 file_offset;
    };

    struct Syntax_Tree_Node {
        Source_Information source_info;
        AST_Node_Type node_type;

        Syntax_Tree_Node(Source_Information source_info, AST_Node_Type node_type): source_info(source_info), node_type(node_type) {}
        virtual ~Syntax_Tree_Node() = default;
        virtual void visit(AST_Visitor& visitor) = 0;
    };

    struct Identifier: public Syntax_Tree_Node {
        std::string identifier;

        Identifier(std::string string): Syntax_Tree_Node({}, AST_Node_Type::identifier), identifier(std::move(string)) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Type: public Syntax_Tree_Node {
        std::string type;

        Type(std::string string): Syntax_Tree_Node({}, AST_Node_Type::type), type(std::move(string)) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Declaration: public Syntax_Tree_Node {
        using Syntax_Tree_Node::Syntax_Tree_Node;
    };

    struct Declaration_List: public Syntax_Tree_Node {
        std::vector<Owning_Ptr<Declaration>> declarations;

        Declaration_List(): Syntax_Tree_Node({}, AST_Node_Type::declaration_list) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }

        void append(Declaration* const declaration) {
            declarations.emplace_back(declaration);
        }

        [[nodiscard]] i64 size() const {
            return declarations.size();
        }
    };

    struct Declaration_If: public Declaration {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Declaration_List> true_declarations;
        Owning_Ptr<Declaration_List> false_declarations;

        Declaration_If(Expression* condition, Declaration_List* true_declarations, Declaration_List* false_declarations)
            : Declaration({}, AST_Node_Type::declaration_if), condition(condition), true_declarations(true_declarations),
              false_declarations(false_declarations) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Import_Decl: public Declaration {
        std::string path;

        Import_Decl(std::string path): Declaration({}, AST_Node_Type::import_decl), path(std::move(path)) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Variable_Declaration: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Expression> initializer;

        Variable_Declaration(Type* type, Identifier* identifier, Expression* initializer)
            : Declaration({}, AST_Node_Type::variable_declaration), type(type), identifier(identifier), initializer(initializer) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Struct_Decl: public Declaration {
        Owning_Ptr<Identifier> name;
        std::vector<Owning_Ptr<Variable_Declaration>> members;

        Struct_Decl(Identifier* name): Declaration({}, AST_Node_Type::struct_decl), name(name) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }

        void append(Variable_Declaration* decl) {
            members.emplace_back(decl);
        }

        i64 size() const {
            return members.size();
        }
    };

    struct Statement_List;

    struct Function_Body: public Syntax_Tree_Node {
        Owning_Ptr<Statement_List> statement_list;

        Function_Body(Statement_List* statement_list): Syntax_Tree_Node({}, AST_Node_Type::function_body), statement_list(statement_list) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Function_Param: public Syntax_Tree_Node {
        using Syntax_Tree_Node::Syntax_Tree_Node;
    };

    struct Function_Param_List: public Syntax_Tree_Node {
        std::vector<Owning_Ptr<Function_Param>> params;

        Function_Param_List(): Syntax_Tree_Node({}, AST_Node_Type::function_param_list) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }

        void append_parameter(Function_Param* parameter) {
            params.push_back(parameter);
        }

        i64 get_parameter_count() const {
            return params.size();
        }
    };

    struct Function_Param_If: public Function_Param {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Function_Param> true_param;
        Owning_Ptr<Function_Param> false_param;

        Function_Param_If(Expression* condition, Function_Param* true_param, Function_Param* false_param)
            : Function_Param({}, AST_Node_Type::function_param_if), condition(condition), true_param(true_param), false_param(false_param) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Ordinary_Function_Param: public Function_Param {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> type;

        Ordinary_Function_Param(Identifier* identifier, Type* type)
            : Function_Param({}, AST_Node_Type::ordinary_function_param), identifier(identifier), type(type) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Function_Declaration: public Declaration {
        Owning_Ptr<Identifier> name;
        Owning_Ptr<Function_Param_List> param_list;
        Owning_Ptr<Type> return_type;
        Owning_Ptr<Function_Body> body;

        Function_Declaration(Identifier* name, Function_Param_List* function_param_list, Type* return_type, Function_Body* body)
            : Declaration({}, AST_Node_Type::function_declaration), name(name), param_list(function_param_list), return_type(return_type), body(body) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Sourced_Function_Param: public Function_Param {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> source;

        Sourced_Function_Param(Identifier* identifier, Type* type, Identifier* source)
            : Function_Param({}, AST_Node_Type::sourced_function_param), identifier(identifier), type(type), source(source) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Pass_Stage_Declaration: public Declaration {
        Owning_Ptr<Identifier> pass;
        Owning_Ptr<Identifier> name;
        Owning_Ptr<Function_Param_List> param_list;
        Owning_Ptr<Type> return_type;
        Owning_Ptr<Function_Body> body;

        Pass_Stage_Declaration(Identifier* pass, Identifier* name, Function_Param_List* parameter_list, Type* return_type, Function_Body* body)
            : Declaration({}, AST_Node_Type::pass_stage_declaration), pass(pass), name(name), param_list(parameter_list), return_type(return_type), body(body) {
        }

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Expression: public Syntax_Tree_Node {
        using Syntax_Tree_Node::Syntax_Tree_Node;
    };

    struct Expression_If: public Expression {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> true_expr;
        Owning_Ptr<Expression> false_expr;

        Expression_If(Expression* condition, Expression* true_expr, Expression* false_expr)
            : Expression({}, AST_Node_Type::expression_if), condition(condition), true_expr(true_expr), false_expr(false_expr) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Identifier_Expression: public Expression {
        Owning_Ptr<Identifier> identifier;

        Identifier_Expression(Identifier* identifier): Expression({}, AST_Node_Type::identifier_expression), identifier(identifier) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Assignment_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Assignment_Expression(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::assignment_expression), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    enum struct Arithmetic_Assignment_Type {
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

    struct Arithmetic_Assignment_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        Arithmetic_Assignment_Type type;

        Arithmetic_Assignment_Expression(Arithmetic_Assignment_Type type, Expression* lhs, Expression* rhs)
            : Expression({}, AST_Node_Type::arithmetic_assignment_expression), lhs(lhs), rhs(rhs), type(type) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Logic_Or_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_Or_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::logic_or_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Logic_Xor_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_Xor_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::logic_xor_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Logic_And_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_And_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::logic_and_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Relational_Equality_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        bool is_equality;

        Relational_Equality_Expression(bool is_equality, Expression* lhs, Expression* rhs)
            : Expression({}, AST_Node_Type::relational_equality_expression), lhs(lhs), rhs(rhs), is_equality(is_equality) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    enum struct Relational_Type {
        greater_than,
        less_than,
        greater_equal,
        less_equal,
    };

    struct Relational_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        Relational_Type type;

        Relational_Expression(Relational_Type type, Expression* lhs, Expression* rhs)
            : Expression({}, AST_Node_Type::relational_expression), lhs(lhs), rhs(rhs), type(type) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Bit_Or_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_Or_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::bit_or_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Bit_Xor_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_Xor_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::bit_xor_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Bit_And_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_And_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::bit_and_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct LShift_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        LShift_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::lshift_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct RShift_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        RShift_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::rshift_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Add_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Add_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::add_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Sub_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Sub_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::sub_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Mul_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Mul_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::mul_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Div_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Div_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::div_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Mod_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Mod_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::mod_expr), lhs(lhs), rhs(rhs) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    enum struct Unary_Type {
        plus,
        minus,
        bit_not,
        logic_not,
    };

    struct Unary_Expression: public Expression {
        Owning_Ptr<Expression> expression;
        Unary_Type type;

        Unary_Expression(Unary_Type type, Expression* expression): Expression({}, AST_Node_Type::unary_expression), expression(expression), type(type) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Prefix_Inc_Dec_Expression: public Expression {
        Owning_Ptr<Expression> expression;
        bool is_inc;

        Prefix_Inc_Dec_Expression(bool is_inc, Expression* expression)
            : Expression({}, AST_Node_Type::prefix_inc_dec_expression), expression(expression), is_inc(is_inc) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Argument_List: public Syntax_Tree_Node {
        std::vector<Owning_Ptr<Expression>> arguments;

        Argument_List(): Syntax_Tree_Node({}, AST_Node_Type::argument_list) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }

        void append(Expression* argument) {
            arguments.emplace_back(argument);
        }

        i64 size() const {
            return arguments.size();
        }
    };

    struct Function_Call_Expression: public Expression {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Argument_List> arg_list;

        Function_Call_Expression(Identifier* identifier, Argument_List* arg_list)
            : Expression({}, AST_Node_Type::function_call_expression), identifier(identifier), arg_list(arg_list) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Member_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Identifier> member;

        Member_Access_Expression(Expression* base, Identifier* member): Expression({}, AST_Node_Type::member_access_expression), base(base), member(member) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Array_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Expression> index;

        Array_Access_Expression(Expression* base, Expression* index): Expression({}, AST_Node_Type::array_access_expression), base(base), index(index) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Postfix_Inc_Dec_Expression: public Expression {
        Owning_Ptr<Expression> base;
        bool is_inc;

        Postfix_Inc_Dec_Expression(bool is_inc, Expression* base): Expression({}, AST_Node_Type::postfix_inc_dec_expression), base(base), is_inc(is_inc) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct String_Literal: public Expression {
        std::string value;

        String_Literal(std::string value): Expression({}, AST_Node_Type::string_literal), value(std::move(value)) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Bool_Literal: public Expression {
        bool value;

        Bool_Literal(bool value): Expression({}, AST_Node_Type::bool_literal), value(value) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Integer_Literal: public Expression {
        std::string value;

        Integer_Literal(std::string value): Expression({}, AST_Node_Type::integer_literal), value(std::move(value)) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Float_Literal: public Expression {
        std::string value;

        Float_Literal(std::string value): Expression({}, AST_Node_Type::float_literal), value(std::move(value)) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Statement: public Syntax_Tree_Node {
        using Syntax_Tree_Node::Syntax_Tree_Node;
    };

    struct Statement_List: public Syntax_Tree_Node {
        std::vector<Owning_Ptr<Statement>> statements;

        Statement_List(): Syntax_Tree_Node({}, AST_Node_Type::statement_list) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }

        void append(Statement* const statement) {
            statements.emplace_back(statement);
        }

        [[nodiscard]] i64 size() const {
            return statements.size();
        }
    };

    struct Block_Statement: public Statement {
        Owning_Ptr<Statement_List> statements;

        Block_Statement(Statement_List* statements): Statement({}, AST_Node_Type::block_statement), statements(statements) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct If_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Statement> true_statement;
        Owning_Ptr<Statement> false_statement;

        If_Statement(Expression* condition, Statement* true_statement, Statement* false_statement)
            : Statement({}, AST_Node_Type::if_statement), condition(condition), true_statement(true_statement), false_statement(false_statement) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Case_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Statement_List> statements;

        Case_Statement(Expression* condition, Statement_List* statements)
            : Statement({}, AST_Node_Type::case_statement), condition(condition), statements(statements) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Default_Case_Statement: public Statement {
        Owning_Ptr<Statement_List> statements;

        Default_Case_Statement(Statement_List* statements): Statement({}, AST_Node_Type::default_case_statement), statements(statements) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Switch_Statement: public Statement {
        std::vector<Owning_Ptr<Statement>> cases;

        Switch_Statement(): Statement({}, AST_Node_Type::switch_statement) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }

        void append(Case_Statement* case_stmt) {
            cases.emplace_back(case_stmt);
        }

        void append(Default_Case_Statement* case_stmt) {
            cases.emplace_back(case_stmt);
        }

        i64 case_count() const {
            return cases.size();
        }
    };

    struct For_Statement: public Statement {
        Owning_Ptr<Variable_Declaration> declaration;
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> post_expression;
        Owning_Ptr<Block_Statement> block;

        For_Statement(Variable_Declaration* declaration, Expression* condition, Expression* post_expression, Block_Statement* block)
            : Statement({}, AST_Node_Type::for_statement), declaration(declaration), condition(condition), post_expression(post_expression), block(block) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Block_Statement> block;

        While_Statement(Expression* condition, Block_Statement* block): Statement({}, AST_Node_Type::while_statement), condition(condition), block(block) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Do_While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Block_Statement> block;

        Do_While_Statement(Expression* condition, Block_Statement* block)
            : Statement({}, AST_Node_Type::do_while_statement), condition(condition), block(block) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Return_Statement: public Statement {
        Owning_Ptr<Expression> return_expr;

        Return_Statement(Expression* return_expr): Statement({}, AST_Node_Type::return_statement), return_expr(return_expr) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Break_Statement: public Statement {
        Break_Statement(): Statement({}, AST_Node_Type::break_statement) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Continue_Statement: public Statement {
        Continue_Statement(): Statement({}, AST_Node_Type::continue_statement) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Declaration_Statement: public Statement {
        Owning_Ptr<Variable_Declaration> var_decl;

        Declaration_Statement(Variable_Declaration* var_decl): Statement({}, AST_Node_Type::declaration_statement), var_decl(var_decl) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };

    struct Expression_Statement: public Statement {
        Owning_Ptr<Expression> expr;

        Expression_Statement(Expression* expression): Statement({}, AST_Node_Type::expression_statement), expr(expression) {}

        virtual void visit(AST_Visitor& visitor) override {
            visitor.visit(*this);
        }
    };
} // namespace vush