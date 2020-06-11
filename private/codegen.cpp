#include <codegen.hpp>

#include <ast.hpp>
#include <filesystem.hpp>
#include <utility.hpp>

#include <fstream>
#include <string>

namespace vush {
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

        std::string common;
        common += "#version 450 core\n";

        anton::Array<GLSL_File> files(anton::reserve, pass_stages.size());
        for(Declaration* decl: pass_stages) {
            Pass_Stage_Declaration& stage = (Pass_Stage_Declaration&)*decl;
        }

        return {expected_value, anton::move(files)};
    }
} // namespace vush
