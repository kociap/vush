#pragma once

#include <owning_ptr.hpp>
#include <vush/vush.hpp>

namespace vush {
    enum struct AST_Node_Type {
        identifier,
        builtin_type,
        user_defined_type,
        declaration_list,
        declaration_if,
        import_decl,
        variable_declaration,
        constant_declaration,
        struct_decl,
        function_body,
        function_param_list,
        function_param_if,
        ordinary_function_param,
        function_declaration,
        sourced_function_param,
        vertex_input_param,
        pass_stage_declaration,
        expression_if,
        identifier_expression,
        assignment_expression,
        arithmetic_assignment_expression,
        elvis_expr,
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
        prefix_inc_expr,
        prefix_dec_expr,
        argument_list,
        function_call_expression,
        member_access_expression,
        array_access_expression,
        postfix_inc_expr,
        postfix_dec_expr,
        paren_expr,
        string_literal,
        bool_literal,
        integer_literal,
        float_literal,
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

    struct Source_Info {
        anton::String_View file_path;
        i64 line;
        i64 column;
        i64 file_offset;
    };

    struct AST_Node {
        Source_Info source_info;
        AST_Node_Type node_type;

        AST_Node(Source_Info const& source_info, AST_Node_Type node_type): source_info(source_info), node_type(node_type) {}
        virtual ~AST_Node() = default;
    };

    struct Declaration;
    struct Expression;
    struct Statement;

    struct Identifier: public AST_Node {
        anton::String identifier;

        Identifier(anton::String string): AST_Node({}, AST_Node_Type::identifier), identifier(anton::move(string)) {}
    };

    struct Type: public AST_Node {
        using AST_Node::AST_Node;
    };

    enum struct Builtin_GLSL_Type {
        glsl_void,
        glsl_bool,
        glsl_int,
        glsl_uint,
        glsl_float,
        glsl_double,
        glsl_vec2,
        glsl_vec3,
        glsl_vec4,
        glsl_dvec2,
        glsl_dvec3,
        glsl_dvec4,
        glsl_bvec2,
        glsl_bvec3,
        glsl_bvec4,
        glsl_ivec2,
        glsl_ivec3,
        glsl_ivec4,
        glsl_uvec2,
        glsl_uvec3,
        glsl_uvec4,
        glsl_mat2,
        glsl_mat3,
        glsl_mat4,
        glsl_mat2x3,
        glsl_mat2x4,
        glsl_mat3x2,
        glsl_mat3x4,
        glsl_mat4x2,
        glsl_mat4x3,
        glsl_dmat2,
        glsl_dmat3,
        glsl_dmat4,
        glsl_dmat2x3,
        glsl_dmat2x4,
        glsl_dmat3x2,
        glsl_dmat3x4,
        glsl_dmat4x2,
        glsl_dmat4x3,
    };

    constexpr anton::String_View stringify(Builtin_GLSL_Type type) {
        switch(type) {
            case Builtin_GLSL_Type::glsl_void:
                return u8"void";
            case Builtin_GLSL_Type::glsl_bool:
                return u8"bool";
            case Builtin_GLSL_Type::glsl_int:
                return u8"int";
            case Builtin_GLSL_Type::glsl_uint:
                return u8"uint";
            case Builtin_GLSL_Type::glsl_float:
                return u8"float";
            case Builtin_GLSL_Type::glsl_double:
                return u8"double";
            case Builtin_GLSL_Type::glsl_vec2:
                return u8"vec2";
            case Builtin_GLSL_Type::glsl_vec3:
                return u8"vec3";
            case Builtin_GLSL_Type::glsl_vec4:
                return u8"vec4";
            case Builtin_GLSL_Type::glsl_dvec2:
                return u8"dvec2";
            case Builtin_GLSL_Type::glsl_dvec3:
                return u8"dvec3";
            case Builtin_GLSL_Type::glsl_dvec4:
                return u8"dvec4";
            case Builtin_GLSL_Type::glsl_bvec2:
                return u8"bvec2";
            case Builtin_GLSL_Type::glsl_bvec3:
                return u8"bvec3";
            case Builtin_GLSL_Type::glsl_bvec4:
                return u8"bvec4";
            case Builtin_GLSL_Type::glsl_ivec2:
                return u8"ivec2";
            case Builtin_GLSL_Type::glsl_ivec3:
                return u8"ivec3";
            case Builtin_GLSL_Type::glsl_ivec4:
                return u8"ivec4";
            case Builtin_GLSL_Type::glsl_uvec2:
                return u8"uvec2";
            case Builtin_GLSL_Type::glsl_uvec3:
                return u8"uvec3";
            case Builtin_GLSL_Type::glsl_uvec4:
                return u8"uvec4";
            case Builtin_GLSL_Type::glsl_mat2:
                return u8"mat2";
            case Builtin_GLSL_Type::glsl_mat3:
                return u8"mat3";
            case Builtin_GLSL_Type::glsl_mat4:
                return u8"mat4";
            case Builtin_GLSL_Type::glsl_mat2x3:
                return u8"mat2x3";
            case Builtin_GLSL_Type::glsl_mat2x4:
                return u8"mat2x4";
            case Builtin_GLSL_Type::glsl_mat3x2:
                return u8"mat3x2";
            case Builtin_GLSL_Type::glsl_mat3x4:
                return u8"mat3x4";
            case Builtin_GLSL_Type::glsl_mat4x2:
                return u8"mat4x2";
            case Builtin_GLSL_Type::glsl_mat4x3:
                return u8"mat4x3";
            case Builtin_GLSL_Type::glsl_dmat2:
                return u8"dmat2";
            case Builtin_GLSL_Type::glsl_dmat3:
                return u8"dmat3";
            case Builtin_GLSL_Type::glsl_dmat4:
                return u8"dmat4";
            case Builtin_GLSL_Type::glsl_dmat2x3:
                return u8"dmat2x3";
            case Builtin_GLSL_Type::glsl_dmat2x4:
                return u8"dmat2x4";
            case Builtin_GLSL_Type::glsl_dmat3x2:
                return u8"dmat3x2";
            case Builtin_GLSL_Type::glsl_dmat3x4:
                return u8"dmat3x4";
            case Builtin_GLSL_Type::glsl_dmat4x2:
                return u8"dmat4x2";
            case Builtin_GLSL_Type::glsl_dmat4x3:
                return u8"dmat4x3";
        }
    }

    struct Builtin_Type: public Type {
        Builtin_GLSL_Type type;

        Builtin_Type(Builtin_GLSL_Type type): Type({}, AST_Node_Type::builtin_type), type(type) {}
    };

    struct User_Defined_Type: public Type {
        anton::String name;

        User_Defined_Type(anton::String name): Type({}, AST_Node_Type::user_defined_type), name(name) {}
    };

    struct Declaration: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Declaration_List: public AST_Node {
        anton::Array<Owning_Ptr<Declaration>> declarations;

        Declaration_List(): AST_Node({}, AST_Node_Type::declaration_list) {}

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
    };

    struct Import_Decl: public Declaration {
        anton::String path;

        Import_Decl(anton::String path): Declaration({}, AST_Node_Type::import_decl), path(anton::move(path)) {}
    };

    struct Variable_Declaration: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Expression> initializer;

        Variable_Declaration(Type* type, Identifier* identifier, Expression* initializer)
            : Declaration({}, AST_Node_Type::variable_declaration), type(type), identifier(identifier), initializer(initializer) {}
    };

    struct Constant_Declaration: public Declaration {
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Expression> initializer;

        Constant_Declaration(Type* type, Identifier* identifier, Expression* initializer)
            : Declaration({}, AST_Node_Type::constant_declaration), type(type), identifier(identifier), initializer(initializer) {}
    };

    struct Struct_Decl: public Declaration {
        Owning_Ptr<Identifier> name;
        anton::Array<Owning_Ptr<Variable_Declaration>> members;

        Struct_Decl(Identifier* name): Declaration({}, AST_Node_Type::struct_decl), name(name) {}

        void append(Variable_Declaration* decl) {
            members.emplace_back(decl);
        }

        i64 size() const {
            return members.size();
        }
    };

    struct Statement_List;

    struct Function_Body: public AST_Node {
        Owning_Ptr<Statement_List> statement_list;

        Function_Body(Statement_List* statement_list): AST_Node({}, AST_Node_Type::function_body), statement_list(statement_list) {}
    };

    struct Function_Param: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Function_Param_List: public AST_Node {
        anton::Array<Owning_Ptr<Function_Param>> params;

        Function_Param_List(): AST_Node({}, AST_Node_Type::function_param_list) {}

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
    };

    struct Ordinary_Function_Param: public Function_Param {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> type;

        Ordinary_Function_Param(Identifier* identifier, Type* type)
            : Function_Param({}, AST_Node_Type::ordinary_function_param), identifier(identifier), type(type) {}
    };

    struct Function_Declaration: public Declaration {
        Owning_Ptr<Identifier> name;
        Owning_Ptr<Function_Param_List> param_list;
        Owning_Ptr<Type> return_type;
        Owning_Ptr<Function_Body> body;

        Function_Declaration(Identifier* name, Function_Param_List* function_param_list, Type* return_type, Function_Body* body)
            : Declaration({}, AST_Node_Type::function_declaration), name(name), param_list(function_param_list), return_type(return_type), body(body) {}
    };

    struct Sourced_Function_Param: public Function_Param {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> type;
        Owning_Ptr<Identifier> source;

        Sourced_Function_Param(Identifier* identifier, Type* type, Identifier* source)
            : Function_Param({}, AST_Node_Type::sourced_function_param), identifier(identifier), type(type), source(source) {}
    };

    struct Vertex_Input_Param: public Function_Param {
        Owning_Ptr<Identifier> identifier;
        Owning_Ptr<Type> type;

        Vertex_Input_Param(Identifier* identifier, Type* type): Function_Param({}, AST_Node_Type::vertex_input_param), identifier(identifier), type(type) {}
    };

    enum struct Pass_Stage_Type {
        vertex,
        fragment,
        compute,
    };

    constexpr anton::String_View stringify(Pass_Stage_Type type) {
        switch(type) {
            case Pass_Stage_Type::vertex:
                return u8"vertex";
            case Pass_Stage_Type::fragment:
                return u8"fragment";
            case Pass_Stage_Type::compute:
                return u8"compute";
        }
    }

    struct Pass_Stage_Declaration: public Declaration {
        Owning_Ptr<Identifier> pass;
        Pass_Stage_Type stage;
        Owning_Ptr<Function_Param_List> param_list;
        Owning_Ptr<Type> return_type;
        Owning_Ptr<Function_Body> body;

        Pass_Stage_Declaration(Identifier* pass, Pass_Stage_Type stage, Function_Param_List* parameter_list, Type* return_type, Function_Body* body)
            : Declaration({}, AST_Node_Type::pass_stage_declaration), pass(pass), stage(stage), param_list(parameter_list), return_type(return_type),
              body(body) {}
    };

    struct Expression: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Expression_If: public Expression {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> true_expr;
        Owning_Ptr<Expression> false_expr;

        Expression_If(Expression* condition, Expression* true_expr, Expression* false_expr)
            : Expression({}, AST_Node_Type::expression_if), condition(condition), true_expr(true_expr), false_expr(false_expr) {}
    };

    struct Identifier_Expression: public Expression {
        Owning_Ptr<Identifier> identifier;

        Identifier_Expression(Identifier* identifier): Expression({}, AST_Node_Type::identifier_expression), identifier(identifier) {}
    };

    struct Assignment_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Assignment_Expression(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::assignment_expression), lhs(lhs), rhs(rhs) {}
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
    };

    struct Elvis_Expr: public Expression {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Expression> true_expr;
        Owning_Ptr<Expression> false_expr;

        Elvis_Expr(Expression* condition, Expression* true_expr, Expression* false_expr)
            : Expression({}, AST_Node_Type::elvis_expr), condition(condition), true_expr(true_expr), false_expr(false_expr) {}
    };

    struct Logic_Or_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_Or_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::logic_or_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Logic_Xor_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_Xor_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::logic_xor_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Logic_And_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Logic_And_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::logic_and_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Relational_Equality_Expression: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;
        bool is_equality;

        Relational_Equality_Expression(bool is_equality, Expression* lhs, Expression* rhs)
            : Expression({}, AST_Node_Type::relational_equality_expression), lhs(lhs), rhs(rhs), is_equality(is_equality) {}
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
    };

    struct Bit_Or_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_Or_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::bit_or_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Bit_Xor_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_Xor_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::bit_xor_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Bit_And_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Bit_And_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::bit_and_expr), lhs(lhs), rhs(rhs) {}
    };

    struct LShift_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        LShift_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::lshift_expr), lhs(lhs), rhs(rhs) {}
    };

    struct RShift_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        RShift_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::rshift_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Add_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Add_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::add_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Sub_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Sub_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::sub_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Mul_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Mul_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::mul_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Div_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Div_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::div_expr), lhs(lhs), rhs(rhs) {}
    };

    struct Mod_Expr: public Expression {
        Owning_Ptr<Expression> lhs;
        Owning_Ptr<Expression> rhs;

        Mod_Expr(Expression* lhs, Expression* rhs): Expression({}, AST_Node_Type::mod_expr), lhs(lhs), rhs(rhs) {}
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
    };

    struct Prefix_Inc_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Prefix_Inc_Expr(Expression* expression): Expression({}, AST_Node_Type::prefix_inc_expr), expression(expression) {}
    };

    struct Prefix_Dec_Expr: public Expression {
        Owning_Ptr<Expression> expression;

        Prefix_Dec_Expr(Expression* expression): Expression({}, AST_Node_Type::prefix_dec_expr), expression(expression) {}
    };

    struct Argument_List: public AST_Node {
        anton::Array<Owning_Ptr<Expression>> arguments;

        Argument_List(): AST_Node({}, AST_Node_Type::argument_list) {}

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
    };

    struct Member_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Identifier> member;

        Member_Access_Expression(Expression* base, Identifier* member): Expression({}, AST_Node_Type::member_access_expression), base(base), member(member) {}
    };

    struct Array_Access_Expression: public Expression {
        Owning_Ptr<Expression> base;
        Owning_Ptr<Expression> index;

        Array_Access_Expression(Expression* base, Expression* index): Expression({}, AST_Node_Type::array_access_expression), base(base), index(index) {}
    };

    struct Postfix_Inc_Expr: public Expression {
        Owning_Ptr<Expression> base;

        Postfix_Inc_Expr(Expression* base): Expression({}, AST_Node_Type::postfix_inc_expr), base(base) {}
    };

    struct Postfix_Dec_Expr: public Expression {
        Owning_Ptr<Expression> base;

        Postfix_Dec_Expr(Expression* base): Expression({}, AST_Node_Type::postfix_dec_expr), base(base) {}
    };

    struct Paren_Expr: public Expression {
        Owning_Ptr<Expression> expr;

        Paren_Expr(Expression* expr): Expression({}, AST_Node_Type::paren_expr), expr(expr) {}
    };

    struct String_Literal: public Expression {
        anton::String value;

        String_Literal(anton::String value): Expression({}, AST_Node_Type::string_literal), value(anton::move(value)) {}
    };

    struct Bool_Literal: public Expression {
        bool value;

        Bool_Literal(bool value, Source_Info const& source_info): Expression(source_info, AST_Node_Type::bool_literal), value(value) {}
    };

    struct Integer_Literal: public Expression {
        anton::String value;

        Integer_Literal(anton::String value): Expression({}, AST_Node_Type::integer_literal), value(anton::move(value)) {}
    };

    struct Float_Literal: public Expression {
        anton::String value;

        Float_Literal(anton::String value): Expression({}, AST_Node_Type::float_literal), value(anton::move(value)) {}
    };

    struct Statement: public AST_Node {
        using AST_Node::AST_Node;
    };

    struct Statement_List: public AST_Node {
        anton::Array<Owning_Ptr<Statement>> statements;

        Statement_List(): AST_Node({}, AST_Node_Type::statement_list) {}

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
    };

    struct If_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Statement> true_statement;
        Owning_Ptr<Statement> false_statement;

        If_Statement(Expression* condition, Statement* true_statement, Statement* false_statement)
            : Statement({}, AST_Node_Type::if_statement), condition(condition), true_statement(true_statement), false_statement(false_statement) {}
    };

    struct Case_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Statement_List> statements;

        Case_Statement(Expression* condition, Statement_List* statements)
            : Statement({}, AST_Node_Type::case_statement), condition(condition), statements(statements) {}
    };

    struct Default_Case_Statement: public Statement {
        Owning_Ptr<Statement_List> statements;

        Default_Case_Statement(Statement_List* statements): Statement({}, AST_Node_Type::default_case_statement), statements(statements) {}
    };

    struct Switch_Statement: public Statement {
        anton::Array<Owning_Ptr<Statement>> cases;
        Owning_Ptr<Expression> match_expr;

        Switch_Statement(Expression* match_expr): Statement({}, AST_Node_Type::switch_statement), match_expr(match_expr) {}

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

        For_Statement(Variable_Declaration* declaration, Expression* condition, Expression* post_expression, Block_Statement* block, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::for_statement), declaration(declaration), condition(condition), post_expression(post_expression), block(block) {}
    };

    struct While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Block_Statement> block;

        While_Statement(Expression* condition, Block_Statement* block, Source_Info const& source_info): Statement(source_info, AST_Node_Type::while_statement), condition(condition), block(block) {}
    };

    struct Do_While_Statement: public Statement {
        Owning_Ptr<Expression> condition;
        Owning_Ptr<Block_Statement> block;

        Do_While_Statement(Expression* condition, Block_Statement* block, Source_Info const& source_info)
            : Statement(source_info, AST_Node_Type::do_while_statement), condition(condition), block(block) {}
    };

    struct Return_Statement: public Statement {
        Owning_Ptr<Expression> return_expr;

        Return_Statement(Expression* return_expr, Source_Info const& source_info): Statement(source_info, AST_Node_Type::return_statement), return_expr(return_expr) {}
    };

    struct Break_Statement: public Statement {
        Break_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::break_statement) {}
    };

    struct Continue_Statement: public Statement {
        Continue_Statement(Source_Info const& source_info): Statement(source_info, AST_Node_Type::continue_statement) {}
    };

    struct Declaration_Statement: public Statement {
        Owning_Ptr<Declaration> declaration;

        Declaration_Statement(Declaration* declaration): Statement({}, AST_Node_Type::declaration_statement), declaration(declaration) {}
    };

    struct Expression_Statement: public Statement {
        Owning_Ptr<Expression> expr;

        Expression_Statement(Expression* expression): Statement({}, AST_Node_Type::expression_statement), expr(expression) {}
    };
} // namespace vush
