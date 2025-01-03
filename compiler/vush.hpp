#pragma once

#include <anton/array.hpp>
#include <anton/expected.hpp>
#include <anton/slice.hpp>
#include <anton/string.hpp>
#include <anton/string_view.hpp>

#include <vush_core/types.hpp>
#include <vush_diagnostics/error.hpp>
#include <vush_spirv/spirv.hpp>

namespace vush {
  struct Constant_Define {
    anton::String name;
    i32 value;
  };

  struct Sourced_Variable {
    anton::String name;
    anton::String type;
    bool unsized;
  };

  struct Sourced_Opaque_Variable {
    anton::String name;
    anton::String type;
    anton::String image_qualifier;
    bool unsized;
  };

  struct Buffer_Definition {
    anton::String declaration;
    anton::String bind;
  };

  struct Setting_Key_Value {
    anton::String key;
    anton::String value;
  };

  struct Pass_Settings {
    anton::String pass_name;
    Array<Setting_Key_Value> settings;
  };

  struct Stage_Sourced_Data {
    anton::Slice<Sourced_Variable const> variables;
    anton::Slice<Sourced_Opaque_Variable const> opaque_variables;
  };

  struct Buffer_Definition_Context {
    anton::String_View pass_name;
    anton::String_View buffer_name;
    anton::Slice<Setting_Key_Value const> settings;
    // Per stage sourced data. Index by casting Stage_Kind to i64.
    anton::Slice<Stage_Sourced_Data const> sourced_data;
    void* user_data;
  };

  // buffer_definition_callback
  //
  // Parameters:
  //     context - the definition context of a source.
  // definitions - output parameter for per-stage buffer definitions. The slice
  //               is always presized to stage_kind_count.
  //
  using buffer_definition_callback = anton::Expected<void, anton::String> (*)(
    Buffer_Definition_Context const& context,
    anton::Slice<Buffer_Definition> definitions);

  struct Diagnostics_Options {
    // Whether to provide extended diagnostic messages that include more
    // thorough explanation of the error and source code snippets with the exact
    // location highlighted.
    bool extended = true;
    // Whether to display line numbers on the left side of code snippets.
    bool display_line_numbers = true;
  };

  using source_query_callback =
    anton::Expected<anton::String, anton::String> (*)(Allocator* allocator,
                                                      anton::String_View name,
                                                      void* user_data);
  using source_import_callback =
    anton::Expected<anton::String, anton::String> (*)(Allocator* allocator,
                                                      anton::String_View path,
                                                      void* user_data);

  struct Configuration {
    anton::String source_name;
    Array<Constant_Define> defines;
    buffer_definition_callback buffer_definition_cb = nullptr;
    void* buffer_definition_user_data = nullptr;
    Diagnostics_Options diagnostics;
  };

  struct Source_Callbacks {
    source_query_callback query_source_cb;
    void* query_main_source_user_data;
    void* query_source_user_data;
    source_import_callback import_source_cb;
    void* import_main_source_user_data;
    void* import_source_user_data;
  };

  struct Shader {
    anton::String pass_identifier;
    Stage_Kind stage_kind;
    // TODO: Replace with binary spirv once we have an assembler and disassembler.
    spirv::Module spirv;
  };

  struct Build_Result {
    Array<Pass_Settings> settings;
    Array<Shader> shaders;
  };

  // compile_to_spirv
  //
  // Compiles the given vush shader to SPIR-V. Uses the callback to import
  // sources.
  //
  // Returns:
  // Compiled SPIR-V files or an error.
  //
  anton::Expected<Build_Result, Error>
  compile_to_spirv(Configuration const& config, Allocator& allocator,
                   Allocator& bump_allocator, Source_Callbacks callbacks);

  // compile_to_spirv
  //
  // Compiles the given vush shader to SPIR-V. Reads the source files from the
  // disk. Uses the import paths provided in import_directories to resolve
  // import directives.
  //
  // Returns:
  // Compiled SPIR-V files or an error.
  //
  anton::Expected<Build_Result, Error>
  compile_to_spirv(Configuration const& config, Allocator& allocator,
                   Allocator& bump_allocator,
                   anton::String_View current_working_directory,
                   anton::Slice<anton::String const> import_directories);
} // namespace vush
