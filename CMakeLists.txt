cmake_minimum_required(VERSION 3.13)

project(vush)

string(FIND "${CMAKE_CXX_COMPILER}" "clang++" VUSH_COMPILER_CLANGPP)
if(VUSH_COMPILER_CLANGPP GREATER -1)
  set(VUSH_COMPILER_CLANGPP ON)
else()
  set(VUSH_COMPILER_CLANGPP OFF)
endif()

if(VUSH_COMPILER_CLANGPP)
    set(VUSH_COMPILE_FLAGS
        -Wall -Wextra -pedantic
        -ferror-limit=1
        -Wnon-virtual-dtor
        -Werror=return-type
        -fno-char8_t # Yea, just no.
        -fno-rtti 
        -fno-exceptions
    )
elseif(MSVC)
  set(VUSH_COMPILE_FLAGS
        /GF
    )
endif()

add_subdirectory(ext/anton_core)

add_library(vush)
set_target_properties(vush PROPERTIES CXX_STANDARD 17 CXX_EXTENSIONS OFF)
target_compile_options(vush PRIVATE ${VUSH_COMPILE_FLAGS})
target_link_libraries(vush PUBLIC anton_core)
target_include_directories(vush PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/public" PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/private")
target_sources(vush 
    PRIVATE
    "${CMAKE_CURRENT_SOURCE_DIR}/private/ast.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/ast.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/codegen_glsl.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/codegen.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/const_expr_eval.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/const_expr_eval.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/context.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/context.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/owning_ptr.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/parser.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/parser.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/string_stream.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/string_stream.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/private/vush.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/public/vush/types.hpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/public/vush/vush.hpp"
)

add_executable(vush_standalone "private/main.cpp")
set_target_properties(vush_standalone PROPERTIES CXX_STANDARD 17 CXX_EXTENSIONS OFF)
target_compile_options(vush_standalone PRIVATE ${VUSH_COMPILE_FLAGS})
target_link_libraries(vush_standalone PRIVATE vush)
