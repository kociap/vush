#include <codegen.hpp>

#include <ast.hpp>
#include <utility.hpp>

namespace vush {
    void stringify(anton::String& out, Syntax_Tree_Node& ast_node) {
        switch(ast_node.node_type) {
            case AST_Node_Type::identifier: {
                Identifier& node = (Identifier&)ast_node;
                out += node.identifier;
                return;
            }

            case AST_Node_Type::builtin_type: {
                Builtin_Type& node = (Builtin_Type&)ast_node;
                out += stringify(node.type);
                return;
            }

            case AST_Node_Type::user_defined_type: {
                User_Defined_Type& node = (User_Defined_Type&)ast_node;
                out += node.name;
                return;
            }

            case AST_Node_Type::constant_declaration: {
                Constant_Declaration& node = (Constant_Declaration&)ast_node;
                out += u8"const ";
                stringify(out, *node.type);
                out += u8" ";
                stringify(out, *node.identifier);
                out += u8" = ";
                stringify(out, *node.initializer);
                out += u8";\n";
                return;
            }

            case AST_Node_Type::struct_decl: {
                Struct_Decl& node = (Struct_Decl&)ast_node;
                out += u8"struct ";
                stringify(out, *node.name);
                out += u8" {\n";
                for(auto& member: node.members) {
                    out += u8"    ";
                    // member->
                }
                return;
            }

            case AST_Node_Type::bool_literal: {
                Bool_Literal& node = (Bool_Literal&)ast_node;
                out += node.value ? u8"true" : u8"false";
                return;
            }

            case AST_Node_Type::integer_literal: {
                Integer_Literal& node = (Integer_Literal&)ast_node;
                out += node.value;
                return;
            }

            case AST_Node_Type::float_literal: {
                Float_Literal& node = (Float_Literal&)ast_node;
                out += node.value;
                return;
            }
        }
    }

    void stringify_function_forward_decl(anton::String& out, Function_Declaration& node) {}

    Expected<anton::Array<GLSL_File>, anton::String> generate_glsl(Declaration_List& node) {
        anton::Array<Declaration*> structs_and_consts;
        anton::Array<Declaration*> functions;
        anton::Array<Declaration*> pass_stages;
        for(auto& decl: node.declarations) {
            switch(decl->node_type) {
                case AST_Node_Type::struct_decl:
                case AST_Node_Type::constant_declaration: {
                    structs_and_consts.emplace_back(decl.get());
                } break;

                case AST_Node_Type::function_declaration: {
                    functions.emplace_back(decl.get());
                } break;

                case AST_Node_Type::pass_stage_declaration: {
                    pass_stages.emplace_back(decl.get());
                } break;
            }
        }

        anton::String common;
        common += "#version 450 core\n";

        for(Declaration* decl: structs_and_consts) {
            stringify(common, *decl);
        }

        for(Declaration* decl: functions) {
            stringify_function_forward_decl(common, (Function_Declaration&)*decl);
        }

        anton::Array<GLSL_File> files(anton::reserve, pass_stages.size());
        for(Declaration* decl: pass_stages) {
            // Pass_Stage_Declaration& stage = (Pass_Stage_Declaration&)*decl;
            files.emplace_back(common);
        }

        return {expected_value, anton::move(files)};
    }
} // namespace vush
