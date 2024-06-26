cmake_minimum_required(VERSION 3.18)
include(FetchContent)

project(vush CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_COLOR_DIAGNOSTICS ON)

string(FIND "${CMAKE_CXX_COMPILER}" "clang++" VUSH_COMPILER_CLANGPP)
if(VUSH_COMPILER_CLANGPP GREATER -1)
  set(VUSH_COMPILER_CLANGPP ON)
else()
  set(VUSH_COMPILER_CLANGPP OFF)
endif()

if(VUSH_COMPILER_CLANGPP)
  set(VUSH_COMPILE_FLAGS
    # -march=native
    -fno-math-errno
    -fno-char8_t # Yea, just no.
    -fno-rtti
    -fno-exceptions
    -Wall -Wextra -pedantic
    -ferror-limit=1

    -ftime-trace

    -Werror=return-type
    -Werror=uninitialized

    -Wnon-virtual-dtor
    -Wnewline-eof
    # -Wunreachable-code-break

    -Wno-reorder-init-list # Supress warnings about reordering in designated initializers
    -Wno-invalid-unevaluated-string # Fires in asserts causing spam
    -Wno-missing-field-initializers # TODO: Temporarily because of spam from vush_diagnostics/diagnostics.hpp
  )

  set(VUSH_BUILTINS_NO_OPTIMISATIONS
    -O0
  )
elseif(MSVC)
  set(VUSH_COMPILE_FLAGS
    /GF # Kill duplicated strings
  )

  set(VUSH_BUILTINS_NO_OPTIMISATIONS
    /Od
  )
endif()

set(ANTON_BUILD_DEBUG $<CONFIG:Debug>)
set(ANTON_ITERATOR_DEBUG $<CONFIG:Debug>)
set(ANTON_OPTIONAL_CHECK_VALUE $<CONFIG:Debug>)
set(ANTON_UNREACHABLE_ASSERTS ON)

# Add anton_core
FetchContent_Declare(
  anton_core
  GIT_REPOSITORY https://github.com/kociap/anton_core.git
  GIT_TAG 28358bd4d2135113957bf0a5eb70b8f2c022a904
)
FetchContent_MakeAvailable(anton_core)

add_library(vush)
set_target_properties(vush PROPERTIES CXX_STANDARD 20 CXX_EXTENSIONS OFF)
target_compile_options(vush PRIVATE ${VUSH_COMPILE_FLAGS})
target_link_libraries(vush PUBLIC anton_core)
target_include_directories(vush PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/compiler")
# Disable optimisations in the builtins source file.
set_source_files_properties(
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/builtin_functions.cpp"
  PROPERTIES
  COMPILE_OPTIONS ${VUSH_BUILTINS_NO_OPTIMISATIONS}
)
target_sources(vush
  PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast/ast.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast/ast.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast/ast_fwd.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast_lowering/ast_lowering.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast_lowering/ast_lowering.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/builtin_functions.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/builtin_types.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/builtin_symbols.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/syntax_accessors.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/syntax_accessors.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/context.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/either.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/memory.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/scoped_map.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/source_info.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/source_registry.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/source_registry.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/types.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/diagnostics.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/diagnostics.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/error.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/error.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/utility.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/utility.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ir/ir.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ir/ir.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ir/ir_fwd.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ir/opcodes.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ir/types.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ir/types.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_lexer/diagnostics.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_lexer/diagnostics.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_lexer/lexer.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_lexer/lexer.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_parser/parser.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_parser/parser.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_sema/diagnostics.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_sema/diagnostics.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_sema/sema.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_sema/sema.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_sema/typeconv.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_sema/typeconv.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_syntax/syntax.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_syntax/syntax.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_syntax_lowering/syntax_lowering.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_syntax_lowering/syntax_lowering.hpp"
)
