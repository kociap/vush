#include <stdlib.h>
// TODO: non-portable
#include <unistd.h>

#include <anton/filesystem.hpp>
#include <anton/format.hpp>
#include <anton/optional.hpp>
#include <anton/stdio.hpp>
#include <anton/string7_view.hpp>
#include <anton/string_view.hpp>

#include <vush.hpp>
#include <vush_spirv/prettyprint.hpp>

namespace vush {
  using namespace anton::literals;

#define EXIT_HELP 2

  [[noreturn]] void usage(anton::String7_View const executable)
  {
    anton::print("Usage: "_sv);
    anton::print(executable);
    anton::print(" [OPTION]... FILE\n"_sv);
    anton::print(
      "\n"
      "The Vush Compiler (vushc)\n"
      "\n"
      "Options:\n"
      "  -h, --help  Print this help page.\n"
      "  -I DIR      Add DIR to the end of the list of import search paths\n"_sv);

    exit(EXIT_HELP);
  }

  [[noreturn]] void error(anton::String7_View const executable,
                          anton::String_View message)
  {
    anton::print(executable);
    anton::print(": error: "_sv);
    anton::print(message);
    anton::print(" (use -h for help)\n"_sv);
    exit(EXIT_FAILURE);
  }

  struct Option_Definition {
    anton::String7_View name;
    i32 id = 0;
    bool mandatory = false;
  };

  struct Option {
    i32 id = 0;
    anton::String7_View value;
  };

  struct Parse_Result {
    anton::Array<Option> options;
    anton::Array<anton::String7_View> arguments;
  };

  struct Option_Tokeniser {
  private:
    i32 argc;
    char const* const* argv;
    char const* arg;
    anton::String7_View token;

    char8 peek_char()
    {
      return *arg;
    }

    void advance_char()
    {
      arg += 1;
      if(*arg == '\0') {
        if(argc > 0) {
          argc -= 1;
          argv += 1;
          arg = *argv;
        } else {
          arg = "";
        }
      }
    }

  public:
    Option_Tokeniser(i32 argc, char const* const* argv)
      : argc(argc - 1), argv(argv + 1)
    {
      if(argc > 0) {
        arg = *argv;
      } else {
        arg = "";
      }
    }

    anton::String7_View peek_token()
    {
      return token;
    }

    void advance_token() {}

    char const* const* get_current_argv_pointer() const
    {
      return argv;
    }

    i32 get_current_argc() const
    {
      return argc;
    }
  };

  [[nodiscard]] static anton::Expected<Parse_Result, anton::String>
  parse_options(Allocator* const allocator,
                anton::Slice<Option_Definition const> const short_options,
                anton::Slice<Option_Definition const> const long_options,
                i32 const argc, char const* const* const argv)
  {
    Parse_Result result{.options = anton::Array<Option>{allocator},
                        .arguments =
                          anton::Array<anton::String7_View>{allocator}};
    i32 i = 1;
    while(i < argc) {
      bool recognised = false;
      anton::String7_View option(argv[i]);
      // Not an option. End parsing.
      if(!begins_with(option, "-"_sv7)) {
        break;
      }

      // -- ends option parsing.
      if(option == "--"_sv7) {
        break;
      }

      if(begins_with(option, "--"_sv7)) {
        // Long options.
        // TODO: Currently relies on whitespace. Add --opt=val format.
        option = anton::shrink_front(option, 2);
        for(Option_Definition const& d: long_options) {
          if(d.name != option) {
            continue;
          }

          recognised = true;
          i += 1;

          if(!d.mandatory) {
            result.options.push_back(Option{d.id, ""_sv7});
            break;
          }

          // Parse the mandatory argument.
          if(i >= argc) {
            return {anton::expected_error,
                    anton::format(allocator, "missing mandatory argument to {}",
                                  option)};
          }

          anton::String7_View argument(argv[i]);
          result.options.push_back(Option{d.id, argument});
          i += 1;
          break;
        }

      } else {
        // Short options.
        option = anton::shrink_front(option, 1);
        auto name = anton::substring_front(option, 1);
        auto argument = anton::shrink_front(option, 1);
        for(Option_Definition const& d: short_options) {
          if(d.name != name) {
            continue;
          }

          recognised = true;
          i += 1;

          if(!d.mandatory) {
            result.options.push_back(Option{d.id, ""_sv7});
            break;
          }

          // Parse the mandatory argument.
          if(argument.size() > 0) {
            result.options.push_back(Option{d.id, argument});
          } else {
            if(i >= argc) {
              return {anton::expected_error,
                      anton::format(
                        allocator, "missing mandatory argument to {}", option)};
            }

            argument = argv[i];
            result.options.push_back(Option{d.id, argument});
            i += 1;
          }

          break;
        }
      }

      if(!recognised) {
        return {anton::expected_error,
                anton::format(allocator, "unrecognised option: {}", option)};
      }
    }

    while(i < argc) {
      result.arguments.push_back(argv[i]);
      i += 1;
    }

    return {anton::expected_value, result};
  }

  [[nodiscard]] static anton::String
  string7_to_string(anton::String7_View string, Allocator* allocator)
  {
    return anton::String(string.begin(), string.end(), allocator);
  }

  anton::String get_cwd(Allocator* allocator)
  {
    char* buffer = getcwd(nullptr, 0);
    if(buffer == nullptr) {
      return anton::String{allocator};
    }

    anton::String cwd(buffer, allocator);
    free(buffer);
    return ANTON_MOV(cwd);
  }

  i32 vushc_main(i32 const argc, char const* const* const argv)

  {
    anton::set_stdout_binary();
    vush::Arena_Allocator allocator(262144);

    anton::String7_View const executable{argv[0]};

    vush::Array<anton::String> import_directories{&allocator};

    vush::Configuration config;
    config.buffer_definition_cb = nullptr;
    config.diagnostics.display_line_numbers = true;

    enum {
      option_help,
      option_import,
    };

    Option_Definition const short_options[] = {
      {"h", option_help, false},
      {"I", option_import, true},
    };
    Option_Definition const long_options[] = {
      {"help", option_help, false},
    };
    anton::Expected<Parse_Result, anton::String> options_result =
      parse_options(&allocator, short_options, long_options, argc, argv);
    if(!options_result) {
      error(executable, options_result.error());
    }

    auto const& arguments = options_result.value().arguments;
    auto const& options = options_result.value().options;
    for(Option const& option: options) {
      switch(option.id) {
      case option_help:
        usage(executable);

      case option_import:
        import_directories.push_back(
          string7_to_string(option.value, &allocator));
        break;
      }
    }

    if(arguments.size() < 1) {
      error(executable, "missing FILE"_sv);
    }

    config.source_name = string7_to_string(arguments[0], &allocator);

    anton::String_View const cwd = get_cwd(&allocator);
    anton::Expected<vush::Build_Result, vush::Error> compilation_result =
      vush::compile_to_spirv(config, allocator, cwd, import_directories);
    if(!compilation_result) {
      anton::print(compilation_result.error().format(&allocator, true));
      return EXIT_FAILURE;
    }

    for(auto const& shader: compilation_result->shaders) {
      anton::STDOUT_Stream stdout;
      spirv::Prettyprint_Options options;
      spirv::prettyprint(&allocator, stdout, options, shader.spirv);
    }

    return EXIT_SUCCESS;
  }
} // namespace vush

int main(int argc, char** argv)
{
  return vush::vushc_main(argc, argv);
}
