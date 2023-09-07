#include <vush.hpp>

#include <anton/algorithm.hpp>
#include <anton/expected.hpp>
#include <anton/filesystem.hpp>
#include <anton/flat_hash_set.hpp>
#include <anton/format.hpp>
#include <anton/iterators.hpp>
#include <anton/iterators/range.hpp>
#include <anton/iterators/zip.hpp>
#include <anton/optional.hpp>
#include <anton/string_stream.hpp>

#include <vush_ast/ast.hpp>
#include <vush_ast_passes/passes.hpp>
#include <vush_core/context.hpp>
#include <vush_core/memory.hpp>
#include <vush_diagnostics/diagnostics.hpp>
#include <vush_parser/parser.hpp>
#include <vush_syntax_lowering/syntax_lowering.hpp>
#include <vush_typecheck/typecheck.hpp>

namespace vush {
  using namespace anton::literals;

  // struct Function_Call_Aggregator: public Recursive_AST_Matcher, public AST_Action {
  //     Array<Function_Call_Expression*> function_calls;

  //     Function_Call_Aggregator(Allocator* allocator): function_calls(allocator) {}

  //     [[nodiscard]] virtual Match_Result match(Function_Call_Expression const&) override {
  //         return {true};
  //     }

  //     virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Function_Call_Expression> function_call) override {
  //         function_calls.push_back(function_call.get());
  //         return function_call;
  //     }
  // };

  // struct Replacement_Rule {
  //     anton::String_View identifier;
  //     Expression* replacement;
  // };

  // struct Identifier_Expression_Replacer: public Recursive_AST_Matcher, public AST_Action {
  // private:
  //     anton::Slice<Replacement_Rule const> replacements;
  //     Allocator* allocator;

  // public:
  //     Identifier_Expression_Replacer(anton::Slice<Replacement_Rule const> const replacements, Allocator* const allocator)
  //         : replacements(replacements), allocator(allocator) {}

  //     [[nodiscard]] virtual Match_Result match(Identifier_Expression const&) override {
  //         return {true, true};
  //     }

  //     virtual Owning_Ptr<AST_Node> execute(Owning_Ptr<Identifier_Expression> n) override {
  //         anton::String_View identifier = n->value;
  //         auto iterator =
  //             anton::find_if(replacements.begin(), replacements.end(), [identifier](Replacement_Rule const& rule) { return rule.identifier == identifier; });
  //         if(iterator != replacements.end()) {
  //             return iterator->replacement->clone(allocator);
  //         } else {
  //             return n;
  //         }
  //     }
  // };

  // perform_function_instantiations
  // Searches all passes for Function_Call_Expression nodes that have unsized array arguments
  // and creates instances of the corresponding functions with those parameters removed and
  // all corresponding identifiers replaced.
  // Removes unsized array arguments from the Function_Call_Expression nodes.
  //
  // IMPORTANT:
  // This function performs 2 transformations on the ast that do NOT preserve the symbol table:
  //  - functions (instances) with symbols (the unsized arguments) that are undefined are created.
  //  - function call expressions are renamed, but the corresponding functions are not added
  //    to the symbol table.
  // After calling this function it is not safe to lookup non-global or function symbols anymore!
  //
  // static void perform_function_instantiations(Context& ctx, anton::Slice<Pass_Context> const passes, Array<Owning_Ptr<Function_Declaration>>& functions) {
  //     Function_Call_Aggregator fn_call_aggregator{ctx.allocator};
  //     Array<Function_Call_Expression*>& function_calls = fn_call_aggregator.function_calls;
  //     // Stores hashes of the stringified signatures of the instances of the functions.
  //     anton::Flat_Hash_Set<u64> instantiated_functions;
  //     // Stores functions that do no require instantiation that have been added to a pass.
  //     anton::Flat_Hash_Set<Function_Declaration*> noninstantiable_functions;
  //     for(Pass_Context& pass: passes) {
  //         if(pass.vertex_context) {
  //             Owning_Ptr<AST_Node> declaration{pass.vertex_context.declaration, nullptr};
  //             traverse_node(fn_call_aggregator, fn_call_aggregator, declaration);
  //             [[maybe_unused]] void* p = declaration.release();
  //         }

  //         if(pass.fragment_context) {
  //             Owning_Ptr<AST_Node> declaration{pass.fragment_context.declaration, nullptr};
  //             traverse_node(fn_call_aggregator, fn_call_aggregator, declaration);
  //             [[maybe_unused]] void* p = declaration.release();
  //         }

  //         if(pass.compute_context) {
  //             Owning_Ptr<AST_Node> declaration{pass.compute_context.declaration, nullptr};
  //             traverse_node(fn_call_aggregator, fn_call_aggregator, declaration);
  //             [[maybe_unused]] void* p = declaration.release();
  //         }

  //         for(i64 i = 0; i < function_calls.size(); ++i) {
  //             Function_Call_Expression& function_call = *function_calls[i];
  //             Symbol* symbol = find_symbol(ctx, function_call.identifier->value);
  //             // Builtin types do not have a symbol, but their constructors still produce function calls.
  //             // Builtin functions do not have a symbol.
  //             if(!symbol || symbol->node_type != Symbol_Type::overloaded_function_declaration) {
  //                 continue;
  //             }

  //             Overloaded_Function_Declaration& fn = static_cast<Overloaded_Function_Declaration&>(*symbol);
  //             if(fn.builtin) {
  //                 continue;
  //             }

  //             for(auto& overload: fn.overloads) {
  //                 Function_Declaration& fn_template = *overload;
  //                 anton::String stringified_signature = stringify_type(ctx.allocator, *fn_template.return_type) + fn_template.identifier->value;
  //                 anton::String instance_name = anton::String(fn_template.identifier->value, ctx.allocator);
  //                 // Check whether the function requires instantiation, i.e. has any unsized array parameters.
  //                 // Stringify the signature and generate instance name.
  //                 bool requires_instantiation = false;
  //                 for(i64 i = 0; i < fn_template.parameters.size(); ++i) {
  //                     Function_Parameter& p = static_cast<Function_Parameter&>(*fn_template.parameters[i]);
  //                     stringified_signature += stringify_type(ctx.allocator, *p.type);
  //                     if(is_unsized_array(*p.type)) {
  //                         ANTON_ASSERT(function_call.arguments[i]->node_type == AST_Node_Type::identifier_expression,
  //                                      "unsized array argument must be an identifier expression");
  //                         Identifier_Expression& expr = static_cast<Identifier_Expression&>(*function_call.arguments[i]);
  //                         instance_name += "_";
  //                         instance_name += expr.value;
  //                         requires_instantiation = true;
  //                     }
  //                 }

  //                 if(!requires_instantiation) {
  //                     // We want to add the function to the pass only once, therefore we store
  //                     // it in a set to keep track of which functions we have already added.
  //                     auto iter = noninstantiable_functions.find(&fn_template);
  //                     if(iter == noninstantiable_functions.end()) {
  //                         noninstantiable_functions.emplace(&fn_template);
  //                         pass.functions.emplace_back(&fn_template);
  //                     }
  //                     continue;
  //                 }

  //                 // Rename the function call
  //                 function_call.identifier->value = instance_name;

  //                 // Guard against multiple instantiations
  //                 u64 const signature_hash = anton::hash(stringified_signature);
  //                 auto iter = instantiated_functions.find(signature_hash);
  //                 if(iter != instantiated_functions.end()) {
  //                     // Function already instantiated.
  //                     // Remove the unsized array arguments from the function call.
  //                     for(i64 i = 0, j = 0; i < fn_template.parameters.size(); ++i) {
  //                         Function_Parameter& p = static_cast<Function_Parameter&>(*fn_template.parameters[i]);
  //                         if(is_unsized_array(*p.type)) {
  //                             auto arg_begin = function_call.arguments.begin();
  //                             function_call.arguments.erase(arg_begin + j, arg_begin + j + 1);
  //                         } else {
  //                             ++j;
  //                         }
  //                     }

  //                     continue;
  //                 }

  //                 instantiated_functions.emplace(signature_hash);

  //                 Owning_Ptr<Function_Declaration> instance = fn_template.clone(ctx.allocator);
  //                 // Rename the instance
  //                 instance->identifier->value = instance_name;
  //                 // Build replacements table
  //                 Array<Replacement_Rule> replacements{ctx.allocator};
  //                 for(i64 i = 0; i < instance->parameters.size(); ++i) {
  //                     Function_Parameter& p = static_cast<Function_Parameter&>(*instance->parameters[i]);
  //                     if(is_unsized_array(*p.type)) {
  //                         ANTON_ASSERT(function_call.arguments[i]->node_type == AST_Node_Type::identifier_expression,
  //                                      "unsized array argument must be an identifier expression");
  //                         Identifier_Expression* argument = static_cast<Identifier_Expression*>(function_call.arguments[i].get());
  //                         replacements.emplace_back(p.identifier->value, argument);
  //                     }
  //                 }

  //                 Identifier_Expression_Replacer identifier_replacer(replacements, ctx.allocator);
  //                 traverse_node(identifier_replacer, identifier_replacer, instance);

  //                 // Remove the unsized array parameters and arguments from the instance and the function call
  //                 for(i64 i = 0; i < instance->parameters.size();) {
  //                     Function_Parameter& p = static_cast<Function_Parameter&>(*instance->parameters[i]);
  //                     if(is_unsized_array(*p.type)) {
  //                         auto arg_begin = function_call.arguments.begin();
  //                         function_call.arguments.erase(arg_begin + i, arg_begin + i + 1);
  //                         auto param_begin = instance->parameters.begin();
  //                         instance->parameters.erase(param_begin + i, param_begin + i + 1);
  //                     } else {
  //                         ++i;
  //                     }
  //                 }

  //                 traverse_node(fn_call_aggregator, fn_call_aggregator, instance);
  //                 pass.functions.emplace_back(instance.get());
  //                 functions.emplace_back(ANTON_MOV(instance));
  //             }
  //         }

  //         function_calls.clear();
  //         // Passes are independent
  //         instantiated_functions.clear();
  //         noninstantiable_functions.clear();
  //     }
  // }

  // static void gather_settings(Allocator* const allocator, Array<Pass_Settings>& settings,
  //                             anton::Slice<Owning_Ptr<Settings_Declaration> const> setting_declarations) {
  //     for(auto& declaration: setting_declarations) {
  //         Pass_Settings* pass_iter = anton::find_if(
  //             settings.begin(), settings.end(), [&pass_name = declaration->pass_name->value](Pass_Settings const& v) { return v.pass_name == pass_name; });
  //         if(pass_iter == settings.end()) {
  //             Pass_Settings& v =
  //                 settings.emplace_back(Pass_Settings{anton::String(declaration->pass_name->value, allocator), Array<Setting_Key_Value>(allocator)});
  //             pass_iter = &v;
  //         }

  //         Array<Setting_Key_Value>& pass_settings = pass_iter->settings;
  //         // N^2 loop to overwrite duplicates in the order of occurence
  //         for(Setting_Key_Value const& kv_new: declaration->settings) {
  //             auto end = pass_settings.end();
  //             auto i = anton::find_if(pass_settings.begin(), end, [&kv_new](Setting_Key_Value const& v) { return kv_new.key == v.key; });
  //             if(i != end) {
  //                 i->value = kv_new.value;
  //             } else {
  //                 pass_settings.emplace_back(kv_new);
  //             }
  //         }
  //     }
  // }

  // [[nodiscard]] static anton::Expected<Array<Pass_Context>, anton::String>
  // build_pass_contexts(Context const& ctx, anton::Slice<Owning_Ptr<Pass_Stage_Declaration> const> const stage_declarations) {
  //     Array<Pass_Context> passes{ctx.allocator};
  //     for(auto const& stage_declaration: stage_declarations) {
  //         anton::String_View const pass_name = stage_declaration->pass_name->value;
  //         Pass_Context* pass = anton::find_if(passes.begin(), passes.end(), [pass_name](Pass_Context const& v) { return v.name == pass_name; });
  //         if(pass == passes.end()) {
  //             Pass_Context& v = passes.push_back(Pass_Context{ctx.allocator, pass_name});
  //             pass = &v;
  //         }

  //         // Ensure there is only 1 stage of each type
  //         switch(stage_declaration->stage_kind) {
  //             case Stage_Kind::vertex: {
  //                 if(pass->vertex_context) {
  //                     Source_Info const& src1 = pass->vertex_context.declaration->source_info;
  //                     Source_Info const& src2 = stage_declaration->source_info;
  //                     return {anton::expected_error, format_duplicate_pass_stage_error(ctx, src1, src2, pass->name, Stage_Kind::vertex)};
  //                 }

  //                 pass->vertex_context.declaration = stage_declaration.get();
  //             } break;

  //             case Stage_Kind::fragment: {
  //                 if(pass->fragment_context) {
  //                     Source_Info const& src1 = pass->fragment_context.declaration->source_info;
  //                     Source_Info const& src2 = stage_declaration->source_info;
  //                     return {anton::expected_error, format_duplicate_pass_stage_error(ctx, src1, src2, pass->name, Stage_Kind::fragment)};
  //                 }

  //                 pass->fragment_context.declaration = stage_declaration.get();
  //             } break;

  //             case Stage_Kind::compute: {
  //                 if(pass->compute_context) {
  //                     Source_Info const& src1 = pass->compute_context.declaration->source_info;
  //                     Source_Info const& src2 = stage_declaration->source_info;
  //                     return {anton::expected_error, format_duplicate_pass_stage_error(ctx, src1, src2, pass->name, Stage_Kind::compute)};
  //                 }

  //                 pass->compute_context.declaration = stage_declaration.get();
  //             } break;
  //         }
  //     }

  //     return {anton::expected_value, ANTON_MOV(passes)};
  // }

  // // validate_passes
  // // Checks whether a pass has either a compute stage or a vertex stage and optionally a fragment stage.
  // // Checks whether vertex/fragment have matching return/parameter. Checks whether the type is struct or void.
  // // Checks whether the return type of the fragment stage is void or struct.
  // //
  // [[nodiscard]] static anton::Expected<void, anton::String> validate_passes(Context const& ctx, anton::Slice<Pass_Context const> const passes) {
  //     for(Pass_Context const& pass: passes) {
  //         // Validate that a pass has either a compute stage or a vertex stage and optionally a fragment stage
  //         if(!pass.compute_context) {
  //             if(!pass.vertex_context) {
  //                 return {anton::expected_error, format_missing_vertex_stage_error(ctx, pass.name)};
  //             }
  //         } else {
  //             if(pass.vertex_context || pass.fragment_context) {
  //                 return {anton::expected_error, format_graphics_and_compute_stages(ctx, pass.name)};
  //             }
  //         }

  //         // Check whether vertex/fragment stages have matching return/parameter.
  //         if(pass.vertex_context) {
  //             Type const& vertex_return_type = *pass.vertex_context.declaration->return_type;
  //             bool const vertex_void_return = is_void(vertex_return_type);
  //             if(pass.fragment_context) {
  //                 if(!vertex_void_return) {
  //                     // The fragment stage must have an unsourced stage input parameter of matching type.
  //                     // Consider cases:
  //                     // - fragment stage has no parameters at all.
  //                     // - fragment stage has sourced parameter as the first parameter.
  //                     // - the first parameter of the fragment stage does not have a matching type.
  //                     Parameter_List const& parameters = pass.fragment_context.declaration->parameters;
  //                     if(parameters.size() < 1) {
  //                         return {anton::expected_error, "TODO"_s};
  //                     }
  //                     Function_Parameter const& first_parameter = static_cast<Function_Parameter const&>(*parameters[0]);
  //                     bool const previous_stage_input = !is_sourced_parameter(first_parameter);
  //                     if(!previous_stage_input) {
  //                         return {anton::expected_error, "TODO"_s};
  //                     }
  //                     if(*first_parameter.type != vertex_return_type) {
  //                         return {anton::expected_error, "TODO"_s};
  //                     }
  //                 } else {
  //                     // The fragment stage must NOT have an unsourced stage input parameter.
  //                     Parameter_List const& parameters = pass.fragment_context.declaration->parameters;
  //                     bool const previous_stage_input =
  //                         parameters.size() > 0 && !is_sourced_parameter(static_cast<Function_Parameter const&>(*parameters[0]));
  //                     if(previous_stage_input) {
  //                         return {anton::expected_error, "TODO"_s};
  //                     }
  //                 }
  //             } else {
  //                 // The return type must be void when there is no fragment stage present in the pass.
  //                 if(!vertex_void_return) {
  //                     return {anton::expected_error, "TODO"_s};
  //                 }
  //             }
  //         }
  //     }
  //     return {anton::expected_value};
  // }

  namespace spirv {
    anton::Expected<Build_Result, Error> compile(Configuration const& config, Allocator& allocator,
                                                 source_import_callback callback, void* user_data)
    {
      Context ctx(&allocator);
      ctx.source_import_cb = callback;
      ctx.source_import_user_data = user_data;
      ctx.source_definition_cb = config.source_definition_cb;
      ctx.source_definition_user_data = config.source_definition_user_data;
      ctx.diagnostics = config.diagnostics;

      // TODO: Fix constant defines.
      // Create symbols for the constant defines passed via config.
      // for(Constant_Define const& define: config.defines) {
      // Source_Info src = {"<defines>", 1, 1, 0};
      // Owning_Ptr decl = allocate_owning<Constant_Declaration>(
      //     ctx.allocator, allocate_owning<Builtin_Type>(ctx.allocator, Builtin_Type_Builtin_Kind::e_int, src),
      //     allocate_owning<Identifier>(ctx.allocator, anton::String(define.name), src),
      //     allocate_owning<Integer_Literal>(ctx.allocator, anton::to_string(define.value), Integer_Literal_Type::i32, Integer_Literal_Base::dec, src),
      //     src);
      // ast::Variable const* const node = nullptr;
      // ctx.add_symbol(Symbol{define.name, node});
      // }

      anton::Expected<ast::Node_List, Error> import_result =
        import_source_code(ctx, config.source_name);
      if(!import_result) {
        return {anton::expected_error, import_result.error()};
      }

      ast::Node_List const ast_nodes = import_result.value();
      ctx.overload_groups = run_overload_group_pass(ctx.allocator, ast_nodes);
      {
        anton::Expected<void, Error> result = run_namebind_pass(ctx, ast_nodes);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }
      }
      {
        anton::Expected<void, Error> result = run_ast_validation_pass(ctx, ast_nodes);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }
      }
      {
        anton::Expected<void, Error> result = run_ast_typecheck_pass(ctx, ast_nodes);
        if(!result) {
          return {anton::expected_error, ANTON_MOV(result.error())};
        }
      }

      return {anton::expected_error,
              Error{.diagnostic = anton::String("end of compiler reached", &allocator)}};

      // gather_settings(ctx.allocator, result.settings, settings);

      // anton::Expected<Array<Pass_Context>, anton::String> pass_build_res = build_pass_contexts(ctx, ast.stages);
      // if(!pass_build_res) {
      //     return {anton::expected_error, ANTON_MOV(pass_build_res.error())};
      // }

      // anton::Slice<Pass_Context> const passes = pass_build_res.value();
      // anton::Expected<void, anton::String> pass_validation_res = validate_passes(ctx, passes);
      // if(!pass_validation_res) {
      //     return {anton::expected_error, ANTON_MOV(pass_validation_res.error())};
      // }

      // anton::Slice<Pass_Settings const> const settings = ast.settings;
      // // Request source definitions for implicit sources.
      // for(Pass_Context& pass: passes) {
      //     Pass_Settings const* const this_pass_settings =
      //         anton::find_if(settings.begin(), settings.end(), [&name = pass.name](Pass_Settings const& settings) { return settings.pass_name == name; });
      //     anton::Slice<Setting_Key_Value const> skv;
      //     if(this_pass_settings != settings.end()) {
      //         skv = this_pass_settings->settings;
      //     }

      //     anton::Expected<void, anton::String> result = request_source_definitions(ctx, pass, skv);
      //     if(!result) {
      //         return {anton::expected_error, ANTON_MOV(result.error())};
      //     }
      // }

      // // TODO: Temporary
      // // Copy structs and constants over to each pass regardless of whether they are used or not.
      // for(Pass_Context& pass: passes) {
      //     for(auto& node: ast.structs_and_constants) {
      //         pass.structs_and_constants.emplace_back(node.get());
      //     }
      // }

      // // We persist the instantiated functions until the code generation is complete.
      // Array<Owning_Ptr<Function_Declaration>> instantiated_functions{ctx.allocator};
      // // BREAKS THE SYMBOL TABLE!
      // // Make sure everything that requires the symbol table is done at this point.
      // perform_function_instantiations(ctx, passes, instantiated_functions);

      // Codegen_Data codegen_data{config.extensions, passes};
      // anton::Expected<Array<Pass_Data>, anton::String> codegen_res = generate_glsl(ctx, codegen_data);
      // if(!codegen_res) {
      //     return {anton::expected_error, ANTON_MOV(codegen_res.error())};
      // }

      // return {anton::expected_value, Build_Result{ANTON_MOV(ast.settings), ANTON_MOV(codegen_res.value())}};
    }

    [[nodiscard]] static anton::Expected<anton::String, anton::String>
    resolve_import_path(Allocator& allocator, anton::String_View const source_name,
                        anton::Slice<anton::String const> const import_directories)
    {
      bool found = false;
      anton::String out_path(&allocator);
      for(anton::String const& path: import_directories) {
        anton::String resolved_path = anton::fs::concat_paths(&allocator, path, source_name);
        bool const exists = anton::fs::exists(resolved_path);
        if(exists) {
          if(!found) {
            found = true;
            out_path = ANTON_MOV(resolved_path);
          } else {
            return {anton::expected_error,
                    anton::format(&allocator, u8"ambiguous source '{}' matches '{}' and '{}'",
                                  source_name, out_path, resolved_path)};
          }
        }
      }

      if(found) {
        return {anton::expected_value, ANTON_MOV(out_path)};
      } else {
        return {anton::expected_error,
                anton::format(&allocator, u8"could not find the source file '{}'", source_name)};
      }
    }

    [[nodiscard]] static anton::Expected<Source_Import_Result, anton::String>
    file_read_callback(Allocator& allocator, anton::String_View const source_name, void* user_data)
    {
      anton::Slice<anton::String const> const& import_directories =
        *(anton::Slice<anton::String const> const*)user_data;
      anton::Expected<anton::String, anton::String> res =
        resolve_import_path(allocator, source_name, import_directories);
      if(!res) {
        return {anton::expected_error, ANTON_MOV(res.error())};
      }

      anton::String const& path = res.value();
      anton::fs::Input_File_Stream file;
      if(!file.open(path)) {
        return {anton::expected_error,
                anton::format(&allocator, u8"could not open '{}' for reading", path)};
      }

      file.seek(anton::Seek_Dir::end, 0);
      i64 size = file.tell();
      file.seek(anton::Seek_Dir::beg, 0);
      anton::String file_contents{anton::reserve, size, &allocator};
      file_contents.force_size(size);
      file.read(file_contents.data(), size);
      return {anton::expected_value,
              Source_Import_Result{ANTON_MOV(res.value()), ANTON_MOV(file_contents)}};
    }

    anton::Expected<Build_Result, Error>
    compile(Configuration const& config, Allocator& allocator,
            anton::Slice<anton::String const> const& import_directories)
    {
      return compile(config, allocator, file_read_callback, (void*)&import_directories);
    }
  } // namespace spirv
} // namespace vush
