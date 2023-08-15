cmake_minimum_required(VERSION 3.15)
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

    -Werror=return-type
    -Werror=uninitialized

    -Wnon-virtual-dtor
    -Wnewline-eof
    # -Wunreachable-code-break

    -Wno-reorder-init-list # Supress warnings about reordering in designated initializers
  )
elseif(MSVC)
  set(VUSH_COMPILE_FLAGS
    /GF # Kill duplicated strings
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
)
FetchContent_MakeAvailable(anton_core)

add_library(vush)
set_target_properties(vush PROPERTIES CXX_STANDARD 20 CXX_EXTENSIONS OFF)
target_compile_options(vush PRIVATE ${VUSH_COMPILE_FLAGS})
target_link_libraries(vush PUBLIC anton_core)
target_include_directories(vush PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/compiler")
target_sources(vush
  PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_arena/arena.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_arena/arena.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast/ast.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast/ast.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast/ast_fwd.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast_transform/overload_construction.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast_transform/transforms.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast_validation/validation.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_ast_validation/validation.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/builtin_symbols.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/builtin_symbols.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/syntax_accessors.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_autogen/syntax_accessors.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/context.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/context.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/either.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/memory.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/source_info.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_core/types.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/diagnostics.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/diagnostics.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/error.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/error.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/lexer.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/lexer.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/utility.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_diagnostics/utility.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_lexer/lexer.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_lexer/lexer.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_namebind/namebind.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_namebind/namebind.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_parser/parser.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_parser/parser.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_syntax/syntax.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_syntax/syntax.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_syntax_lowering/syntax_lowering.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_syntax_lowering/syntax_lowering.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_typecheck/diagnostics.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_typecheck/diagnostics.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_typecheck/typecheck.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_typecheck/typecheck.hpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_typecheck/typeconv.cpp"
  "${CMAKE_CURRENT_SOURCE_DIR}/compiler/vush_typecheck/typeconv.hpp"
)
