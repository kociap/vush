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
target_include_directories(vush PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/public" PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/private")
target_sources(vush
    PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/private/allocator.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/ast_fwd.hpp"
    # "${CMAKE_CURRENT_SOURCE_DIR}/private/ast_traversal.cpp"
    # "${CMAKE_CURRENT_SOURCE_DIR}/private/ast_traversal.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/ast.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/ast2.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/builtin_symbols_autogen.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/builtin_symbols.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/codegen.hpp"
    # "${CMAKE_CURRENT_SOURCE_DIR}/private/const_expr_eval.cpp"
    # "${CMAKE_CURRENT_SOURCE_DIR}/private/const_expr_eval.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/context.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/diagnostics.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/diagnostics.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/either.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/lexer.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/lexer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/memory.hpp"
    # "${CMAKE_CURRENT_SOURCE_DIR}/private/sources.hpp"
    # "${CMAKE_CURRENT_SOURCE_DIR}/private/sources.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/owning_ptr.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/parser2.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/passes.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/syntax_accessors.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/syntax_ast_transform.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/vush.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/diagnostics/lexer.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/diagnostics/lexer.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/diagnostics/utility.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/diagnostics/utility.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/passes/ast_construction.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/passes/ast_validation.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/passes/ast_defcheck.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/public/vush/allocator.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/public/vush/types.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/public/vush/vush.hpp"
)
