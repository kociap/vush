#include <passes.hpp>

#include <anton/algorithm/sort.hpp>
#include <anton/flat_hash_map.hpp>

#include <ast2.hpp>
#include <context.hpp>
#include <diagnostics.hpp>

namespace vush {
    using namespace anton::literals;

    // check_and_add_symbol
    // Checks whether a symbol already exists and if not, adds it to the symbol table.
    // Otherwise returns an error diagnostic.
    //
    [[nodiscard]] static anton::Expected<void, Error> check_and_add_symbol(Context& ctx, Symbol const& symbol) {
        Symbol const* const original_symbol = ctx.find_symbol(symbol.get_name());
        if(original_symbol != nullptr) {
            auto get_symbol_name = [](Symbol const& symbol) -> Source_Info {
                switch(symbol.get_kind()) {
                    case Symbol_Kind::variable: {
                        auto const v = static_cast<ast::Variable const*>(symbol.get_node());
                        return v->identifier->source_info;
                    }

                    case Symbol_Kind::func_parameter: {
                        auto const v = static_cast<ast::Func_Parameter const*>(symbol.get_node());
                        return v->identifier->source_info;
                    }

                    case Symbol_Kind::decl_overloaded_function: {
                        // TODO: This won't work because overloaded function has
                        //       no source information for the identifier.
                        // auto const v = static_cast<ast::Decl_Overloaded_Function const*>(symbol.get_node());
                        // return v->identifier->source_info;
                        return Source_Info{};
                    }

                    default:
                        ANTON_FAIL(false, "unknown symbol type");
                }
            };

            Source_Info const old_name = get_symbol_name(*original_symbol);
            Source_Info const new_name = get_symbol_name(symbol);
            return {anton::expected_error, err_symbol_redefinition(ctx, old_name, new_name)};
        }

        ctx.add_symbol(symbol);
        return {anton::expected_value};
    }

    [[nodiscard]] static anton::Expected<void, Error> check_type_exists(Context const& ctx, ast::Type const& type) {
        switch(type.node_kind) {
            case ast::Node_Kind::type_array: {
                ast::Type_Array const& t = static_cast<ast::Type_Array const&>(type);
                return check_type_exists(ctx, *t.base);
            }

            case ast::Node_Kind::type_user_defined: {
                ast::Type_User_Defined const& udt = static_cast<ast::Type_User_Defined const&>(type);
                if(ctx.find_symbol(udt.value) != nullptr) {
                    return {anton::expected_value};
                } else {
                    return {anton::expected_error, err_undefined_symbol(ctx, type.source_info)};
                }
            }

            case ast::Node_Kind::type_builtin: {
                return {anton::expected_value};
            }

            default: {
                ANTON_ASSERT(false, "unhandled ast::Node_Kind");
                ANTON_UNREACHABLE();
            }
        }
    }

    [[nodiscard]] static anton::Expected<void, Error> check_array_is_sized(Context const& ctx, ast::Type const& type) {
        if(ast::is_unsized_array(type)) {
            return {anton::expected_error, err_unsized_array_not_allowed(ctx, type.source_info)};
        }

        if(type.node_kind == ast::Node_Kind::type_array) {
            ast::Type_Array const& t = static_cast<ast::Type_Array const&>(type);
            return check_array_is_sized(ctx, *t.base);
        }

        return {anton::expected_value};
    }

    [[nodiscard]] static anton::Expected<void, Error> validate_expression(Context& ctx, ast::Expr const* const expression) {
        switch(expression->node_kind) {
            case ast::Node_Kind::lt_integer: {
                ast::Lt_Integer const* const node = static_cast<ast::Lt_Integer const*>(expression);
                // Integer literals must require at most 32 bits.
                // TODO: Literals may include leading 0s.
                // TODO: Validate overflow.
                // The max allowed value is 4294967295
                switch(node->base) {
                    case ast::Lt_Integer_Base::dec: {
                    } break;

                    case ast::Lt_Integer_Base::bin: {
                    } break;

                    case ast::Lt_Integer_Base::hex: {
                    } break;
                }
                return {anton::expected_value};
            }

            case ast::Node_Kind::lt_float: {
                // TODO: Implement validation.
                return {anton::expected_value};
            }

            case ast::Node_Kind::lt_bool: {
                // No validation to be done.
                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_identifier: {
                ast::Expr_Identifier const* const node = static_cast<ast::Expr_Identifier const*>(expression);
                Symbol const* const symbol = ctx.find_symbol(node->value);
                if(!symbol) {
                    return {anton::expected_error, err_undefined_symbol(ctx, node->source_info)};
                }

                ctx.add_definition(node, symbol->get_node());

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_prefix: {
                ast::Expr_Prefix const* const node = static_cast<ast::Expr_Prefix const*>(expression);
                anton::Expected<void, Error> res = validate_expression(ctx, node->expression);
                if(!res) {
                    return ANTON_MOV(res);
                }

                // TODO: Typecheck type of expression and kind of prefix.

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_postfix: {
                ast::Expr_Postfix const* const node = static_cast<ast::Expr_Postfix const*>(expression);
                anton::Expected<void, Error> res = validate_expression(ctx, node->expression);
                if(!res) {
                    return ANTON_MOV(res);
                }

                // TODO: Typecheck type of expression and kind of postfix.

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_call: {
                ast::Expr_Call const* const node = static_cast<ast::Expr_Call const*>(expression);
                for(ast::Expr const* const arg: node->arguments) {
                    anton::Expected<void, Error> res = validate_expression(ctx, arg);
                    if(!res) {
                        return ANTON_MOV(res);
                    }
                }

                // TODO: Add array constructors
                // [0, 1, 2, 3] // [int; 4] with elements 0, 1, 2, 3
                // [vec2(0); 6] // [vec2; 6] with all elements vec2(0)

                // If the identifier is a builtin glsl type, it's a constructor call
                if(anton::Optional<ast::GLSL_Type> res = ast::enumify_glsl_type(node->identifier->value)) {
                    return {anton::expected_value};
                }

                // Otherwise we look up the symbol to verify that it exists
                Symbol const* const symbol = ctx.find_symbol(node->identifier->value);
                if(!symbol) {
                    return {anton::expected_error, err_undefined_symbol(ctx, node->identifier->source_info)};
                }

                if(symbol->get_kind() != Symbol_Kind::decl_struct && symbol->get_kind() != Symbol_Kind::decl_overloaded_function) {
                    // Not a user defined type constructor call and not a function call.
                    return {anton::expected_error, err_called_symbol_does_not_name_function(ctx, node->identifier->source_info)};
                }

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_member_access: {
                ast::Expr_Member_Access const* const node = static_cast<ast::Expr_Member_Access const*>(expression);
                anton::Expected<void, Error> res = validate_expression(ctx, node->base);
                if(!res) {
                    return ANTON_MOV(res);
                }

                // TODO: Validate member exists. We need to have the type of the base
                //       expression in order to look up the symbol and access the memebrs.
                //       Include special symbols for builtin types (e.g. vecs) and swizzles.

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_array_access: {
                ast::Expr_Array_Access const* const node = static_cast<ast::Expr_Array_Access const*>(expression);
                anton::Expected<void, Error> base_res = validate_expression(ctx, node->base);
                if(!base_res) {
                    return ANTON_MOV(base_res);
                }

                // TODO: Validate base is an array type.

                anton::Expected<void, Error> index_res = validate_expression(ctx, node->index);
                if(!index_res) {
                    return ANTON_MOV(index_res);
                }

                // TODO: Validate index is an integer.

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_parentheses: {
                ast::Expr_Parentheses const* const node = static_cast<ast::Expr_Parentheses const*>(expression);
                anton::Expected<void, Error> res = validate_expression(ctx, node->expression);
                if(!res) {
                    return ANTON_MOV(res);
                }

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_binary: {
                ast::Expr_Binary const* const node = static_cast<ast::Expr_Binary const*>(expression);
                if(anton::Expected<void, Error> lhs = validate_expression(ctx, node->lhs); !lhs) {
                    return ANTON_MOV(lhs);
                }

                if(anton::Expected<void, Error> rhs = validate_expression(ctx, node->rhs); !rhs) {
                    return ANTON_MOV(rhs);
                }

                // TODO: Typecheck binary. Consider including function definitions
                //       for 'operatorX' as palceholders for builtin operations.
                //       May later be used to implement operator overloading.

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_if: {
                ast::Expr_If const* const node = static_cast<ast::Expr_If const*>(expression);
                anton::Expected<void, Error> expr_res = validate_expression(ctx, node->condition);
                if(!expr_res) {
                    return ANTON_MOV(expr_res);
                }

                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_reinterpret: {
                // TODO: Implement once we implement transform for reinterpret.
                // Owning_Ptr<Reinterpret_Expression>& node = (Owning_Ptr<Reinterpret_Expression>&)expression;
                // anton::Expected<void, anton::String> index_res = validate_expression(ctx, node->index);
                // if(!index_res) {
                //     return {anton::expected_error, ANTON_MOV(index_res.error())};
                // }

                // anton::Expected<void, anton::String> source_res = validate_expression(ctx, node->source);
                // if(!source_res) {
                //     return {anton::expected_error, ANTON_MOV(source_res.error())};
                // }
                return {anton::expected_value};
            }

            case ast::Node_Kind::expr_default: {
                // TODO: There most likely is no validation to be done here, however, we
                //       need special handling of this expression in typecheck.
                return {anton::expected_value};
            }

            default:
                ANTON_ASSERT(false, "unhandled ast::Node_Kind");
                ANTON_UNREACHABLE();
        }
    }

    // TODO: Add an options struct to disallow continue, break statements in certain contexts (e.g. switch).

    [[nodiscard]] static anton::Expected<void, Error> validate_statements(Context& ctx, ast::Node_List const statements) {
        // We push new scope and pop it only at the end of the function. We do not pop the scope when we fail
        // because an error always leads to termination.
        ctx.push_scope();

        for(ast::Node const* const stmt: statements) {
            switch(stmt->node_kind) {
                case ast::Node_Kind::variable: {
                    ast::Variable const* const node = static_cast<ast::Variable const*>(stmt);
                    anton::Expected<void, Error> symbol_res = check_and_add_symbol(ctx, Symbol(node->identifier->value, node));
                    if(!symbol_res) {
                        return ANTON_MOV(symbol_res);
                    }

                    bool const immutable = !node->type->qualifiers.mut;
                    if(immutable && node->initializer == nullptr) {
                        return {anton::expected_error, err_immutable_variable_missing_initializer(ctx, node->source_info)};
                    }

                    if(node->initializer != nullptr) {
                        anton::Expected<void, Error> res = validate_expression(ctx, node->initializer);
                        if(!res) {
                            return ANTON_MOV(res);
                        }

                        // TODO: Typecheck initializer and variable.
                    }
                } break;

                case ast::Node_Kind::stmt_block: {
                    ast::Stmt_Block const* const node = static_cast<ast::Stmt_Block const*>(stmt);
                    anton::Expected<void, Error> res = validate_statements(ctx, node->statements);
                    if(!res) {
                        return ANTON_MOV(res);
                    }
                } break;

                case ast::Node_Kind::stmt_if: {
                    ast::Stmt_If const* const node = static_cast<ast::Stmt_If const*>(stmt);
                    if(anton::Expected<void, Error> res = validate_expression(ctx, node->condition); !res) {
                        return ANTON_MOV(res);
                    }

                    // TODO: Typecheck condition is convertible to bool.

                    if(anton::Expected<void, Error> res = validate_statements(ctx, node->then_branch); !res) {
                        return ANTON_MOV(res);
                    }

                    if(anton::Expected<void, Error> res = validate_statements(ctx, node->else_branch); !res) {
                        return ANTON_MOV(res);
                    }
                } break;

                case ast::Node_Kind::stmt_loop: {
                    ast::Stmt_Loop const* const node = static_cast<ast::Stmt_Loop const*>(stmt);
                    if(node->condition) {
                        anton::Expected<void, Error> res = validate_expression(ctx, node->condition);
                        if(!res) {
                            return ANTON_MOV(res);
                        }
                    }

                    // TODO: Maybe validate continuation has no continue statements.
                    anton::Expected<void, Error> continuation_result = validate_statements(ctx, node->continuation);
                    if(!continuation_result) {
                        return ANTON_MOV(continuation_result);
                    }

                    anton::Expected<void, Error> statements_result = validate_statements(ctx, node->statements);
                    if(!statements_result) {
                        return ANTON_MOV(statements_result);
                    }
                } break;

                case ast::Node_Kind::stmt_switch: {
                    ast::Stmt_Switch const* const node = static_cast<ast::Stmt_Switch const*>(stmt);
                    anton::Expected<void, Error> switch_expr_res = validate_expression(ctx, node->expression);
                    if(!switch_expr_res) {
                        return ANTON_MOV(switch_expr_res);
                    }

                    ast::Expr const* default_label = nullptr;
                    Array<ast::Lt_Integer const*> labels{ctx.allocator};
                    for(ast::Switch_Arm const* const arm: node->arms) {
                        for(ast::Expr const* const label: arm->labels) {
                            anton::Expected<void, Error> label_res = validate_expression(ctx, label);
                            if(!label_res) {
                                return ANTON_MOV(label_res);
                            }

                            if(label->node_kind == ast::Node_Kind::expr_default) {
                                // Ensure that the default label is unique
                                if(default_label == nullptr) {
                                    default_label = label;
                                } else {
                                    return {anton::expected_error, err_duplicate_default_label(ctx, default_label->source_info, label->source_info)};
                                }

                            } else if(label->node_kind == ast::Node_Kind::lt_integer) {
                                labels.push_back(static_cast<ast::Lt_Integer const*>(label));
                            } else {
                                Source_Info const& src = label->source_info;
                                return {anton::expected_error, err_invalid_switch_arm_expression(ctx, src)};
                            }
                        }

                        // TODO: prevent break from being used within switch

                        anton::Expected<void, Error> res = validate_statements(ctx, arm->statements);
                        if(!res) {
                            return ANTON_MOV(res);
                        }
                    }

                    // Ensure there are no duplicate labels
                    if(labels.size() > 0) {
                        anton::merge_sort(labels.begin(), labels.end(),
                                          [](ast::Lt_Integer const* const v1, ast::Lt_Integer const* const v2) { return compare(v1->value, v2->value) == -1; });
                        for(auto i = labels.begin(), j = labels.begin() + 1, e = labels.end(); j != e; ++i, ++j) {
                            if((*i)->value == (*j)->value) {
                                Source_Info const& src1 = (*i)->source_info;
                                Source_Info const& src2 = (*j)->source_info;
                                return {anton::expected_error, err_duplicate_label(ctx, src1, src2)};
                            }
                        }
                    }
                } break;

                case ast::Node_Kind::stmt_return: {
                    ast::Stmt_Return const* const node = static_cast<ast::Stmt_Return const*>(stmt);
                    if(node->expression) {
                        anton::Expected<void, Error> res = validate_expression(ctx, node->expression);
                        if(!res) {
                            return ANTON_MOV(res);
                        }
                    }
                } break;

                case ast::Node_Kind::stmt_expression: {
                    ast::Stmt_Expression const* const node = static_cast<ast::Stmt_Expression const*>(stmt);
                    anton::Expected<void, Error> res = validate_expression(ctx, node->expression);
                    if(!res) {
                        return ANTON_MOV(res);
                    }
                } break;

                default:
                    // stmt_break, stmt_continue, stmt_discard need no validation.
                    break;
            }
        }

        ctx.pop_scope();
        return {anton::expected_value};
    }

    // validate_struct_member_type
    // Validate that a type is a complete type, i.e. not opaque and is not a recursive definition.
    //
    [[nodiscard]] static anton::Expected<void, Error> validate_struct_member_type(Context const& ctx, ast::Type const& type,
                                                                                  ast::Identifier const& struct_identifier) {
        switch(type.node_kind) {
            case ast::Node_Kind::type_builtin: {
                ast::Type_Builtin const& t = static_cast<ast::Type_Builtin const&>(type);
                if(ast::is_opaque_glsl_type(t.value)) {
                    return {anton::expected_error, err_opaque_type_in_struct(ctx, t.source_info)};
                }

                return {anton::expected_value};
            } break;

            case ast::Node_Kind::type_user_defined: {
                ast::Type_User_Defined const& t = static_cast<ast::Type_User_Defined const&>(type);
                if(t.value == struct_identifier.value) {
                    return {anton::expected_error, err_recursive_type_definition(ctx, struct_identifier.source_info, t.source_info)};
                }

                Symbol const* const symbol = ctx.find_symbol(t.value);
                if(symbol == nullptr) {
                    return {anton::expected_error, err_undefined_symbol(ctx, t.source_info)};
                }

                return {anton::expected_value};
            } break;

            case ast::Node_Kind::type_array: {
                ast::Type_Array const& t = static_cast<ast::Type_Array const&>(type);
                return validate_struct_member_type(ctx, *t.base, struct_identifier);
            } break;

            default:
                ANTON_ASSERT(false, "unreachable");
                ANTON_UNREACHABLE();
        }
    }

    [[nodiscard]] static anton::Expected<void, Error> validate_struct(Context const& ctx, ast::Decl_Struct const* const dstruct) {
        if(dstruct->members.size() == 0) {
            return {anton::expected_error, err_empty_struct(ctx, dstruct->identifier->source_info)};
        }

        // Member names must be unique.
        {
            anton::Flat_Hash_Map<anton::String_View, ast::Identifier const*> member_identifiers;
            for(ast::Struct_Member const* const member: dstruct->members) {
                auto iter = member_identifiers.find(member->identifier->value);
                if(iter != member_identifiers.end()) {
                    return {anton::expected_error, err_duplicate_struct_member(ctx, iter->value->source_info, member->source_info)};
                } else {
                    member_identifiers.emplace(member->identifier->value, member->identifier);
                }
            }
        }

        // Member types must be complete types and must not be self.
        for(ast::Struct_Member const* const member: dstruct->members) {
            anton::Expected<void, Error> result = validate_struct_member_type(ctx, *member->type, *dstruct->identifier);
            if(!result) {
                return result;
            }
        }

        // TODO: Attributes and initializers.
        // noperspective, flat, smooth, invariant

        return {anton::expected_value};
    }

    [[nodiscard]] static anton::Expected<void, Error> validate_constant(Context const& ctx, ast::Variable const* const constant) {
        //             Constant_Declaration& node = static_cast<Constant_Declaration&>(*ast_node);
        //             if(!node.initializer) {
        //                 return {anton::expected_error, format_constant_missing_initializer(ctx, node.source_info)};
        //             }

        //             if(anton::Expected<void, anton::String> res = validate_expression(ctx, node.initializer); !res) {
        //                 return {anton::expected_error, ANTON_MOV(res.error())};
        //             }
        return {anton::expected_value};
    }

    [[nodiscard]] static anton::Expected<void, Error> validate_function(Context& ctx, ast::Decl_Function const* const fn) {
        // Validate attributes. Currently attributes are not allowed on ordinary functions.
        {
            for(ast::Attribute const* const attribute: fn->attributes) {
                return {anton::expected_error, err_illegal_attribute(ctx, attribute->identifier->source_info)};
            }
        }

        // Validate the return type:
        // - if the type is an array, it must be sized.
        {
            if(anton::Expected<void, Error> result = check_type_exists(ctx, *fn->return_type); !result) {
                return result;
            }

            if(anton::Expected<void, Error> result = check_array_is_sized(ctx, *fn->return_type); !result) {
                return result;
            }
        }

        // Push new scope for the function body and parameters.
        ctx.push_scope();

        // Validate parameters:
        // - only ordinary parameters are allowed.
        {
            for(ast::Func_Parameter const* const p: fn->parameters) {
                if(anton::Expected<void, Error> result = check_and_add_symbol(ctx, Symbol(p->identifier->value, p)); !result) {
                    return result;
                }

                if(ast::is_sourced_parameter(*p)) {
                    return {anton::expected_error, err_fn_sourced_parameter_not_allowed(ctx, p->source_info)};
                }
            }
        }

        if(anton::Expected<void, Error> result = validate_statements(ctx, fn->body); !result) {
            return result;
        }

        ctx.pop_scope();

        return {anton::expected_value};
    }

    [[nodiscard]] static anton::Expected<void, Error> validate_stage_function(Context& ctx, ast::Decl_Stage_Function const* const fn) {
        // Validate attributes:
        // - compute stage might have the workgroup attribute (at most 1).
        // - other stages must not have any attributes.
        {
            switch(fn->stage.value) {
                case Stage_Kind::compute: {
                    ast::Attribute const* workgroup = nullptr;
                    for(ast::Attribute const* const attribute: fn->attributes) {
                        if(attribute->identifier->value == "workgroup"_sv) {
                            if(!workgroup) {
                                workgroup = attribute;
                            } else {
                                return {anton::expected_error,
                                        err_duplicate_attribute(ctx, workgroup->identifier->source_info, attribute->identifier->source_info)};
                            }
                        } else {
                            return {anton::expected_error, err_illegal_attribute(ctx, attribute->identifier->source_info)};
                        }
                    }
                } break;

                default: {
                    for(ast::Attribute const* const attribute: fn->attributes) {
                        return {anton::expected_error, err_illegal_attribute(ctx, attribute->source_info)};
                    }
                } break;
            }
        }

        // Validate the return type:
        // - vertex: must be builtin or UDT.
        // - fragment: must be builtin or UDT.
        // - compute: must be void.
        {
            if(anton::Expected<void, Error> result = check_type_exists(ctx, *fn->return_type); !result) {
                return result;
            }

            bool const void_return = ast::is_void(*fn->return_type);
            bool const builtin_return = fn->return_type->node_kind == ast::Node_Kind::type_builtin;
            bool const udt_return = fn->return_type->node_kind == ast::Node_Kind::type_user_defined;
            switch(fn->stage.value) {
                case Stage_Kind::vertex: {
                    if(!builtin_return && !udt_return) {
                        return {anton::expected_error,
                                err_stage_return_must_be_builtin_or_udt(ctx, fn->pass->value, fn->stage.source_info, fn->return_type->source_info)};
                    }
                } break;

                case Stage_Kind::fragment: {
                    if(!builtin_return && !udt_return) {
                        return {anton::expected_error,
                                err_stage_return_must_be_builtin_or_udt(ctx, fn->pass->value, fn->stage.source_info, fn->return_type->source_info)};
                    }
                } break;

                case Stage_Kind::compute: {
                    if(!void_return) {
                        return {anton::expected_error, err_compute_return_must_be_void(ctx, fn->pass->value, fn->return_type->source_info)};
                    }
                } break;
            }
        }

        // Push new scope for the function body and parameters.
        ctx.push_scope();

        // Validate parameters:
        // - all parameters must be builtin or UDT. Arrays are not supported yet.
        // - vertex: only vertex input parameters and sourced parameters are allowed.
        //   vertex input parameters must not be opaque.
        // - fragment: all parameters must be sourced with the exception of the first
        //   one which might be an ordinary parameter that is used as an input from
        //   the previous stage.
        // - compute: only sourced parameters are allowed.
        {
            bool first = true;
            for(ast::Func_Parameter const* const p: fn->parameters) {
                if(anton::Expected<void, Error> result = check_and_add_symbol(ctx, Symbol(p->identifier->value, p)); !result) {
                    return result;
                }

                {
                    bool const builtin_type = fn->return_type->node_kind == ast::Node_Kind::type_builtin;
                    bool const udt_type = fn->return_type->node_kind == ast::Node_Kind::type_user_defined;
                    if(!builtin_type && !udt_type) {
                        return {anton::expected_error, err_stage_parameter_must_be_builtin_or_udt(ctx, p->type->source_info)};
                    }

                    if(anton::Expected<void, Error> result = check_type_exists(ctx, *fn->return_type); !result) {
                        return result;
                    }
                }

                switch(fn->stage.value) {
                    case Stage_Kind::vertex: {
                        if(!ast::is_sourced_parameter(*p)) {
                            return {anton::expected_error, err_vertex_ordinary_parameter_not_allowed(ctx, p->source_info)};
                        }

                        if(is_vertex_input_parameter(*p)) {
                            if(is_opaque_type(*p->type)) {
                                return {anton::expected_error, err_vertex_vin_must_not_be_opaque(ctx, p->type->source_info)};
                            }

                            if(p->type->node_kind == ast::Node_Kind::type_array) {
                                return {anton::expected_error, err_vertex_vin_must_not_be_array(ctx, p->type->source_info)};
                            }
                        }
                    } break;

                    case Stage_Kind::fragment: {
                        bool const ordinary_parameter = !ast::is_sourced_parameter(*p);
                        if(!first && ordinary_parameter) {
                            return {anton::expected_error, err_fragment_ordinary_parameter_not_allowed(ctx, p->source_info)};
                        }

                        if(ast::is_vertex_input_parameter(*p)) {
                            return {anton::expected_error, err_fragment_vin_not_allowed(ctx, p->source->source_info)};
                        }
                    } break;

                    case Stage_Kind::compute: {
                        if(!ast::is_sourced_parameter(*p)) {
                            return {anton::expected_error, err_compute_ordinary_parameter_not_allowed(ctx, p->source_info)};
                        }

                        if(ast::is_vertex_input_parameter(*p)) {
                            return {anton::expected_error, err_compute_vin_not_allowed_on_stage(ctx, p->source_info)};
                        }
                    } break;
                }

                first = false;
            }
        }

        if(anton::Expected<void, Error> result = validate_statements(ctx, fn->body); !result) {
            return result;
        }

        ctx.pop_scope();

        return {anton::expected_value};
    }

    [[nodiscard]] static anton::Expected<void, Error> validate_overloaded_function(Context& ctx, ast::Decl_Overloaded_Function const* const fn) {
        // Ensure all overloads have different sets of parameters.
        // Compare all with all. O(n^2) comparisons.
        for(i64 i = 0; i < fn->overloads.size(); ++i) {
            ast::Decl_Function const* const overload1 = fn->overloads[i];
            for(i64 j = i + 1; j < fn->overloads.size(); ++j) {
                ast::Decl_Function const* const overload2 = fn->overloads[j];
                if(overload1->parameters.size() != overload2->parameters.size()) {
                    continue;
                }

                bool identical = true;
                anton::Zip_Iterator begin{overload1->parameters.begin(), overload2->parameters.begin()};
                anton::Zip_Iterator end{overload1->parameters.end(), overload2->parameters.end()};
                // TODO: Not using mov on begin and end causes a compilation error. Investigate.
                for(auto const [p1, p2]: anton::Range(ANTON_MOV(begin), ANTON_MOV(end))) {
                    if(!ast::compare_types_equal(*p1->type, *p2->type)) {
                        identical = false;
                        break;
                    }
                }

                if(identical) {
                    if(!ast::compare_types_equal(*overload1->return_type, *overload2->return_type)) {
                        return {anton::expected_error, err_overload_on_return_type(ctx, overload1->identifier->source_info, overload1->return_type->source_info,
                                                                                   overload2->identifier->source_info, overload2->return_type->source_info)};
                    } else {
                        return {anton::expected_error, err_symbol_redefinition(ctx, overload1->identifier->source_info, overload2->identifier->source_info)};
                    }
                }
            }
        }
        return {anton::expected_value};
    }

    anton::Expected<void, Error> run_ast_validation_pass(Context& ctx, ast::Node_List const ast) {
        ctx.push_scope();

        // Add global symbols before we do any validation.
        for(ast::Node const* const decl: ast) {
            if(decl->node_kind == ast::Node_Kind::variable) {
                ast::Variable const* const d = static_cast<ast::Variable const*>(decl);
                if(anton::Expected<void, Error> result = check_and_add_symbol(ctx, Symbol(d->identifier->value, d)); !result) {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }
            } else if(decl->node_kind == ast::Node_Kind::decl_struct) {
                ast::Decl_Struct const* const d = static_cast<ast::Decl_Struct const*>(decl);
                if(anton::Expected<void, Error> result = check_and_add_symbol(ctx, Symbol(d->identifier->value, d)); !result) {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }
            } else if(decl->node_kind == ast::Node_Kind::decl_overloaded_function) {
                ast::Decl_Overloaded_Function const* const d = static_cast<ast::Decl_Overloaded_Function const*>(decl);
                if(anton::Expected<void, Error> result = check_and_add_symbol(ctx, Symbol(d->identifier, d)); !result) {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }
            }
        }

        // There is yet no support for struct member initializers, however, for
        // future considerations, validating structs and constants separately
        // prevents us from properly validating both. Currently we validate
        // structs first as that seems to be the best solution giving us the
        // option to conduct the most thorough analysis.

        // Validate structs.
        for(ast::Node const* const decl: ast) {
            if(decl->node_kind != ast::Node_Kind::decl_struct) {
                continue;
            }

            ast::Decl_Struct const* const dstruct = static_cast<ast::Decl_Struct const*>(decl);
            if(anton::Expected<void, Error> result = validate_struct(ctx, dstruct); !result) {
                return {anton::expected_error, ANTON_MOV(result.error())};
            }
        }

        // Validate constants.
        for(ast::Node const* const decl: ast) {
            if(decl->node_kind != ast::Node_Kind::variable) {
                continue;
            }

            ast::Variable const* const constant = static_cast<ast::Variable const*>(decl);
            if(anton::Expected<void, Error> result = validate_constant(ctx, constant); !result) {
                return {anton::expected_error, ANTON_MOV(result.error())};
            }
        }

        // Validate functions.
        for(ast::Node const* const decl: ast) {
            if(decl->node_kind != ast::Node_Kind::decl_overloaded_function) {
                continue;
            }

            ast::Decl_Overloaded_Function const* const ofn = static_cast<ast::Decl_Overloaded_Function const*>(decl);
            if(anton::Expected<void, Error> result = validate_overloaded_function(ctx, ofn); !result) {
                return {anton::expected_error, ANTON_MOV(result.error())};
            }

            for(ast::Decl_Function const* const fn: ofn->overloads) {
                if(anton::Expected<void, Error> result = validate_function(ctx, fn); !result) {
                    return {anton::expected_error, ANTON_MOV(result.error())};
                }
            }
        }

        // Validate stage functions.
        for(ast::Node const* const decl: ast) {
            if(decl->node_kind != ast::Node_Kind::decl_stage_function) {
                continue;
            }

            ast::Decl_Stage_Function const* const fn = static_cast<ast::Decl_Stage_Function const*>(decl);
            if(anton::Expected<void, Error> result = validate_stage_function(ctx, fn); !result) {
                return {anton::expected_error, ANTON_MOV(result.error())};
            }
        }

        ctx.pop_scope();
        return {anton::expected_value};
    }
} // namespace vush
