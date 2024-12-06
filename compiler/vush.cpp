#include <vush.hpp>

#include <anton/algorithm.hpp>
#include <anton/expected.hpp>
#include <anton/filesystem.hpp>
#include <anton/flat_hash_set.hpp>
#include <anton/format.hpp>
#include <anton/iterators.hpp>
#include <anton/iterators/zip.hpp>
#include <anton/optional.hpp>
#include <anton/stdio.hpp>
#include <anton/string_stream.hpp>

#include <vush_ast/ast.hpp>
#include <vush_ast_lowering/lower_ast.hpp>
#include <vush_ast_opt/opts.hpp>
#include <vush_core/context.hpp>
#include <vush_core/memory.hpp>
#include <vush_core/source_registry.hpp>
#include <vush_diagnostics/diagnostics.hpp>
#include <vush_expansion/expansion.hpp>
#include <vush_ir/ir.hpp>
#include <vush_ir/prettyprint.hpp>
#include <vush_parser/parser.hpp>
#include <vush_sema/sema.hpp>
#include <vush_spirv/lower_ir.hpp>
#include <vush_spirv/prettyprint.hpp>
#include <vush_spirv/spirv.hpp>
#include <vush_syntax_lowering/lower_syntax.hpp>

namespace vush {
  using namespace anton::literals;

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

#define RETURN_ON_FAIL(variable, fn, ...)                        \
  auto variable = fn(__VA_ARGS__);                               \
  if(!variable) {                                                \
    return {anton::expected_error, ANTON_MOV(variable.error())}; \
  }

  anton::Expected<Build_Result, Error>
  compile_to_spirv(Configuration const& config, Allocator& allocator,
                   Source_Callbacks callbacks)
  {
    Source_Registry registry(&allocator);

    Context ctx{
      .allocator = &allocator,
      .source_registry = &registry,
      .diagnostics = config.diagnostics,
      .buffer_definition_cb = config.buffer_definition_cb,
      .buffer_definition_user_data = config.buffer_definition_user_data,

      .query_source_cb = callbacks.query_source_cb,
      .query_main_source_user_data = callbacks.query_main_source_user_data,
      .query_source_user_data = callbacks.query_source_user_data,
      .import_source_cb = callbacks.import_source_cb,
      .import_main_source_user_data = callbacks.import_main_source_user_data,
      .import_source_user_data = callbacks.import_source_user_data,
    };

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

    RETURN_ON_FAIL(import_result, import_main_source, ctx, config.source_name);

    RETURN_ON_FAIL(lex_result, lex_source, ctx, config.source_name,
                   anton::String7_View{import_result->bytes_begin(),
                                       import_result->bytes_end()});

    Parse_Syntax_Options parse_options{.include_whitespace_and_comments =
                                         false};
    RETURN_ON_FAIL(parse_result, parse_tokens, ctx, config.source_name,
                   import_result->bytes_begin(), lex_result.value(),
                   parse_options);

    RETURN_ON_FAIL(expand_result, full_expand, ctx,
                   ANTON_MOV(parse_result.value()));

    RETURN_ON_FAIL(syntax_lower_result, lower_syntax, ctx,
                   expand_result.value());

    ast::Node_List const ast_nodes = syntax_lower_result.value();
    {
      anton::Expected<void, Error> result = run_sema(ctx, ast_nodes);
      if(!result) {
        return {anton::expected_error, ANTON_MOV(result.error())};
      }
    }

    {
      bool changed = false;
      do {
        changed = false;
        changed |= run_opt_ast_fold_swizzles(ctx.allocator, ast_nodes);
      } while(changed);
    }

    Array<ir::Module> ir_modules = lower_ast_to_ir(ctx.allocator, ast_nodes);
    Array<Shader> shaders{&allocator};
    for(ir::Module const& ir_module: ir_modules) {
      spirv::Module spirv_module = lower_ir_module(ctx.allocator, &ir_module);
      shaders.push_back(
        Shader{anton::String{ir_module.pass_identifier, &allocator},
               ir_module.stage, ANTON_MOV(spirv_module)});
    }

    return {anton::expected_value,
            Build_Result{Array<Pass_Settings>{&allocator}, ANTON_MOV(shaders)}};
  }

  [[nodiscard]] static anton::Expected<anton::String, anton::String>
  resolve_import_path(Allocator* allocator,
                      anton::String_View const source_name, void* user_data)
  {
    anton::Slice<anton::String const> const import_directories =
      *reinterpret_cast<anton::Slice<anton::String const> const*>(user_data);
    bool found = false;
    anton::String out_path(allocator);
    for(anton::String const& path: import_directories) {
      anton::String resolved_path =
        anton::fs::concat_paths(allocator, path, source_name);
      bool const exists = anton::fs::exists(resolved_path);
      if(exists) {
        if(!found) {
          found = true;
          out_path = ANTON_MOV(resolved_path);
        } else {
          return {anton::expected_error,
                  anton::format(
                    allocator, "ambiguous source '{}' matches '{}' and '{}'"_sv,
                    source_name, out_path, resolved_path)};
        }
      }
    }

    if(found) {
      return {anton::expected_value, ANTON_MOV(out_path)};
    } else {
      return {anton::expected_error,
              anton::format(allocator, "could not find the source file '{}'"_sv,
                            source_name)};
    }
  }

  [[nodiscard]] static anton::Expected<anton::String, anton::String>
  file_read_callback(Allocator* allocator, anton::String_View const source_path,
                     void* user_data)
  {
    ANTON_UNUSED(user_data);

    anton::String path(source_path, allocator);
    anton::fs::Input_File_Stream file;
    if(!file.open(path)) {
      return {
        anton::expected_error,
        anton::format(allocator, "could not open '{}' for reading"_sv, path)};
    }

    file.seek(anton::Seek_Dir::end, 0);
    i64 size = file.tell();
    file.seek(anton::Seek_Dir::beg, 0);
    anton::String file_contents{anton::reserve, size, allocator};
    file_contents.force_size(size);
    file.read(file_contents.data(), size);
    return {anton::expected_value, ANTON_MOV(file_contents)};
  }

  anton::Expected<Build_Result, Error>
  compile_to_spirv(Configuration const& config, Allocator& allocator,
                   anton::String_View const current_working_directory,
                   anton::Slice<anton::String const> import_directories)
  {
    anton::String cwd{current_working_directory, &allocator};
    anton::Slice<anton::String const> cwd_import_directories{&cwd, (&cwd) + 1};
    Source_Callbacks callbacks{
      .query_source_cb = resolve_import_path,
      .query_main_source_user_data = (void*)&cwd_import_directories,
      .query_source_user_data = (void*)&import_directories,
      .import_source_cb = file_read_callback,
      .import_main_source_user_data = nullptr,
      .import_source_user_data = nullptr,
    };
    return compile_to_spirv(config, allocator, callbacks);
  }
} // namespace vush
