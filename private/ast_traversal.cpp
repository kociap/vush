#include <ast.hpp>
#include <ast_traversal.hpp>

#include <anton/intrinsics.hpp>

namespace vush {
    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Identifier const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Builtin_Type const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(User_Defined_Type const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Array_Type const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Declaration_If const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Import_Declaration const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Variable_Declaration const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Constant_Declaration const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Struct_Member const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Struct_Declaration const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Settings_Declaration const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Workgroup_Attribute const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Function_Param_If const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Image_Layout_Qualifier const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Function_Parameter const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Function_Declaration const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Overloaded_Function_Declaration const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Pass_Stage_Declaration const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Expression_If const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Identifier_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Assignment_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Arithmetic_Assignment_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Elvis_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Binary_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Unary_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Prefix_Increment_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Prefix_Decrement_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Function_Call_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Member_Access_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Array_Access_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Postfix_Increment_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Postfix_Decrement_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Parenthesised_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Reinterpret_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Default_Expression const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(String_Literal const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Bool_Literal const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Integer_Literal const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Float_Literal const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Block_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(If_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Case_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Switch_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(For_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(While_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Do_While_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Return_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Break_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Continue_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Discard_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Declaration_Statement const&) {
        return {};
    }

    Recursive_AST_Matcher::Match_Result Recursive_AST_Matcher::match(Expression_Statement const&) {
        return {};
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Identifier> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Builtin_Type> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<User_Defined_Type> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Array_Type> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Declaration_If> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Import_Declaration> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Variable_Declaration> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Constant_Declaration> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Struct_Member> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Struct_Declaration> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Settings_Declaration> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Workgroup_Attribute> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Function_Param_If> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Image_Layout_Qualifier> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Function_Parameter> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Function_Declaration> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Overloaded_Function_Declaration> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Pass_Stage_Declaration> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Expression_If> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Identifier_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Assignment_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Arithmetic_Assignment_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Elvis_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Binary_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Unary_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Prefix_Increment_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Prefix_Decrement_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Function_Call_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Member_Access_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Array_Access_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Postfix_Increment_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Postfix_Decrement_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Parenthesised_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Reinterpret_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Default_Expression> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<String_Literal> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Bool_Literal> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Integer_Literal> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Float_Literal> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Block_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<If_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Case_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Switch_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<For_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<While_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Do_While_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Return_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Break_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Continue_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Discard_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Declaration_Statement> node) {
        return ANTON_MOV(node);
    }

    Owning_Ptr<AST_Node> AST_Action::execute(Owning_Ptr<Expression_Statement> node) {
        return ANTON_MOV(node);
    }

    using Match_Result = Recursive_AST_Matcher::Match_Result;

    [[nodiscard]] static bool traverse_node_internal(Recursive_AST_Matcher& matcher, AST_Action& action, Owning_Ptr<AST_Node>& node) {
        switch(node->node_type) {
            case AST_Node_Type::identifier: {
                Identifier& n = static_cast<Identifier&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Identifier> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::builtin_type: {
                Builtin_Type& n = static_cast<Builtin_Type&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Builtin_Type> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::user_defined_type: {
                User_Defined_Type& n = static_cast<User_Defined_Type&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<User_Defined_Type> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::array_type: {
                Array_Type& n = static_cast<Array_Type&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Array_Type> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.base)) {
                        return true;
                    }
                    if(n.size) {
                        if(traverse_node_internal(matcher, action, n.size)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::declaration_if: {
                Declaration_If& n = static_cast<Declaration_If&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Declaration_If> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.condition)) {
                        return true;
                    }

                    for(auto& declaration: n.true_declarations) {
                        if(traverse_node_internal(matcher, action, declaration)) {
                            return true;
                        }
                    }

                    for(auto& declaration: n.false_declarations) {
                        if(traverse_node_internal(matcher, action, declaration)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::import_declaration: {
                Import_Declaration& n = static_cast<Import_Declaration&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Import_Declaration> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.path)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::variable_declaration: {
                Variable_Declaration& n = static_cast<Variable_Declaration&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Variable_Declaration> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.type)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.identifier)) {
                        return true;
                    }
                    if(n.initializer) {
                        if(traverse_node_internal(matcher, action, n.initializer)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::constant_declaration: {
                Constant_Declaration& n = static_cast<Constant_Declaration&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Constant_Declaration> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.type)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.identifier)) {
                        return true;
                    }
                    if(n.initializer) {
                        if(traverse_node_internal(matcher, action, n.initializer)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::struct_member: {
                Struct_Member& n = static_cast<Struct_Member&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Struct_Member> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.type)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.identifier)) {
                        return true;
                    }
                    if(n.initializer) {
                        if(traverse_node_internal(matcher, action, n.initializer)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::struct_declaration: {
                Struct_Declaration& n = static_cast<Struct_Declaration&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Struct_Declaration> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.identifier)) {
                        return true;
                    }

                    for(auto& member: n.members) {
                        if(traverse_node_internal(matcher, action, member)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::settings_declaration: {
                Settings_Declaration& n = static_cast<Settings_Declaration&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Settings_Declaration> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.pass_name)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::workgroup_attribute: {
                Workgroup_Attribute& n = static_cast<Workgroup_Attribute&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Workgroup_Attribute> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.x)) {
                        return true;
                    }
                    if(n.y) {
                        if(traverse_node_internal(matcher, action, n.y)) {
                            return true;
                        }
                    }
                    if(n.z) {
                        if(traverse_node_internal(matcher, action, n.z)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::function_param_if: {
                Function_Param_If& n = static_cast<Function_Param_If&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Function_Param_If> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.condition)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.true_param)) {
                        return true;
                    }
                    if(n.false_param) {
                        if(traverse_node_internal(matcher, action, n.false_param)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::image_layout_qualifier: {
                Image_Layout_Qualifier& n = static_cast<Image_Layout_Qualifier&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Image_Layout_Qualifier> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::function_parameter: {
                Function_Parameter& n = static_cast<Function_Parameter&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Function_Parameter> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.type)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.identifier)) {
                        return true;
                    }
                    if(n.source) {
                        if(traverse_node_internal(matcher, action, n.source)) {
                            return true;
                        }
                    }
                    if(n.image_layout) {
                        if(traverse_node_internal(matcher, action, n.image_layout)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::function_declaration: {
                Function_Declaration& n = static_cast<Function_Declaration&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Function_Declaration> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    for(auto& attribute: n.attributes) {
                        if(traverse_node_internal(matcher, action, attribute)) {
                            return true;
                        }
                    }

                    if(traverse_node_internal(matcher, action, n.identifier)) {
                        return true;
                    }

                    for(auto& parameter: n.parameters) {
                        if(traverse_node_internal(matcher, action, parameter)) {
                            return true;
                        }
                    }

                    if(traverse_node_internal(matcher, action, n.return_type)) {
                        return true;
                    }

                    for(auto& statement: n.body) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::overloaded_function_declaration: {
                Overloaded_Function_Declaration& n = static_cast<Overloaded_Function_Declaration&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Overloaded_Function_Declaration> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.identifier)) {
                        return true;
                    }

                    for(auto& overload: n.overloads) {
                        if(traverse_node_internal(matcher, action, overload)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::pass_stage_declaration: {
                Pass_Stage_Declaration& n = static_cast<Pass_Stage_Declaration&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Pass_Stage_Declaration> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    for(auto& attribute: n.attributes) {
                        if(traverse_node_internal(matcher, action, attribute)) {
                            return true;
                        }
                    }

                    if(traverse_node_internal(matcher, action, n.pass_name)) {
                        return true;
                    }

                    for(auto& parameter: n.parameters) {
                        if(traverse_node_internal(matcher, action, parameter)) {
                            return true;
                        }
                    }

                    if(traverse_node_internal(matcher, action, n.return_type)) {
                        return true;
                    }

                    for(auto& statement: n.body) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::expression_if: {
                Expression_If& n = static_cast<Expression_If&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Expression_If> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.condition)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.true_expression)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.false_expression)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::identifier_expression: {
                Identifier_Expression& n = static_cast<Identifier_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Identifier_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::assignment_expression: {
                Assignment_Expression& n = static_cast<Assignment_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Assignment_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.lhs)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.rhs)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::arithmetic_assignment_expression: {
                Arithmetic_Assignment_Expression& n = static_cast<Arithmetic_Assignment_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Arithmetic_Assignment_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.lhs)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.rhs)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::elvis_expression: {
                Elvis_Expression& n = static_cast<Elvis_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Elvis_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.condition)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.true_expression)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.false_expression)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::binary_expression: {
                Binary_Expression& n = static_cast<Binary_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Binary_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.lhs)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.rhs)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::unary_expression: {
                Unary_Expression& n = static_cast<Unary_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Unary_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.expression)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::prefix_increment_expression: {
                Prefix_Increment_Expression& n = static_cast<Prefix_Increment_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Prefix_Increment_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.expression)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::prefix_decrement_expression: {
                Prefix_Decrement_Expression& n = static_cast<Prefix_Decrement_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Prefix_Decrement_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.expression)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::function_call_expression: {
                Function_Call_Expression& n = static_cast<Function_Call_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Function_Call_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    for(auto& arg: n.arguments) {
                        if(traverse_node_internal(matcher, action, arg)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::member_access_expression: {
                Member_Access_Expression& n = static_cast<Member_Access_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Member_Access_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.base)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.member)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::array_access_expression: {
                Array_Access_Expression& n = static_cast<Array_Access_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Array_Access_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.base)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.index)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::postfix_increment_expression: {
                Postfix_Increment_Expression& n = static_cast<Postfix_Increment_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Postfix_Increment_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.expression)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::postfix_decrement_expression: {
                Postfix_Decrement_Expression& n = static_cast<Postfix_Decrement_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Postfix_Decrement_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.expression)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::parenthesised_expression: {
                Parenthesised_Expression& n = static_cast<Parenthesised_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Parenthesised_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.expression)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::reinterpret_expression: {
                Reinterpret_Expression& n = static_cast<Reinterpret_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Reinterpret_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.target_type)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.source)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.index)) {
                        return true;
                    }
                }
                return false;
            } break;

            case AST_Node_Type::default_expression: {
                Default_Expression& n = static_cast<Default_Expression&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Default_Expression> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::string_literal: {
                String_Literal& n = static_cast<String_Literal&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<String_Literal> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::bool_literal: {
                Bool_Literal& n = static_cast<Bool_Literal&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Bool_Literal> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::integer_literal: {
                Integer_Literal& n = static_cast<Integer_Literal&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Integer_Literal> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::float_literal: {
                Float_Literal& n = static_cast<Float_Literal&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Float_Literal> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }
                return result.break_matching;
            } break;

            case AST_Node_Type::block_statement: {
                Block_Statement& n = static_cast<Block_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Block_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    for(auto& statement: n.statements) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::if_statement: {
                If_Statement& n = static_cast<If_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<If_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.condition)) {
                        return true;
                    }
                    for(auto& statement: n.true_statements) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                    for(auto& statement: n.false_statements) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::case_statement: {
                Case_Statement& n = static_cast<Case_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Case_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    for(auto& expression: n.labels) {
                        if(traverse_node_internal(matcher, action, expression)) {
                            return true;
                        }
                    }
                    for(auto& statement: n.statements) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::switch_statement: {
                Switch_Statement& n = static_cast<Switch_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Switch_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.match_expression)) {
                        return true;
                    }

                    for(auto& statement: n.cases) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::for_statement: {
                For_Statement& n = static_cast<For_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<For_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.declaration)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.condition)) {
                        return true;
                    }
                    if(traverse_node_internal(matcher, action, n.post_expression)) {
                        return true;
                    }

                    for(auto& statement: n.statements) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::while_statement: {
                While_Statement& n = static_cast<While_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<While_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.condition)) {
                        return true;
                    }

                    for(auto& statement: n.statements) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::do_while_statement: {
                Do_While_Statement& n = static_cast<Do_While_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Do_While_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.condition)) {
                        return true;
                    }

                    for(auto& statement: n.statements) {
                        if(traverse_node_internal(matcher, action, statement)) {
                            return true;
                        }
                    }
                }
                return false;
            } break;

            case AST_Node_Type::return_statement: {
                Return_Statement& n = static_cast<Return_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Return_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(n.return_expression) {
                        if(traverse_node_internal(matcher, action, n.return_expression)) {
                            return true;
                        }
                    }
                }

                return false;
            } break;

            case AST_Node_Type::break_statement: {
                Break_Statement& n = static_cast<Break_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Break_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                return result.break_matching;
            } break;

            case AST_Node_Type::continue_statement: {
                Continue_Statement& n = static_cast<Continue_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Continue_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                return result.break_matching;
            } break;

            case AST_Node_Type::discard_statement: {
                Discard_Statement& n = static_cast<Discard_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Discard_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                return result.break_matching;
            } break;

            case AST_Node_Type::declaration_statement: {
                Declaration_Statement& n = static_cast<Declaration_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Declaration_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.declaration)) {
                        return true;
                    }
                }

                return false;
            } break;

            case AST_Node_Type::expression_statement: {
                Expression_Statement& n = static_cast<Expression_Statement&>(*node);
                Match_Result const result = matcher.match(n);
                if(result.matched) {
                    Owning_Ptr<Expression_Statement> node_typed{downcast, ANTON_MOV(node)};
                    node = action.execute(ANTON_MOV(node_typed));
                }

                if(result.break_matching) {
                    return true;
                }

                if(!result.ignore_children) {
                    if(traverse_node_internal(matcher, action, n.expression)) {
                        return true;
                    }
                }

                return false;
            } break;

            default:
                ANTON_ASSERT(false, "Unhandled AST_Node_Type");
                ANTON_UNREACHABLE();
                return false;
        }
    }

    void traverse_node(Recursive_AST_Matcher& matcher, AST_Action& action, Owning_Ptr<AST_Node>& node) {
        [[maybe_unused]] bool const result = traverse_node_internal(matcher, action, node);
    }
} // namespace vush
