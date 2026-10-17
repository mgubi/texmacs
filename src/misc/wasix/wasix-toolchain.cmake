# CMake toolchain of the WASIX build (wasixcc, https://github.com/wasix-org/wasixcc),
# for the libraries built with CMake (SDL3): build-deps.sh passes it as
#   -DCMAKE_TOOLCHAIN_FILE=misc/wasix/wasix-toolchain.cmake
# the system CMake sees: WASI, or Linux for the projects which know only the
# systems they support (SDL3: build-deps.sh passes -DWASIX_SYSTEM_NAME=Linux)
if (DEFINED WASIX_SYSTEM_NAME)
  set (CMAKE_SYSTEM_NAME ${WASIX_SYSTEM_NAME})
else ()
  set (CMAKE_SYSTEM_NAME WASI)
endif ()
set (CMAKE_SYSTEM_VERSION 1)
set (CMAKE_SYSTEM_PROCESSOR wasm32)
set (triple wasm32-wasmer-wasi)
set (CMAKE_C_COMPILER wasixcc)
set (CMAKE_CXX_COMPILER wasix++)
set (CMAKE_ASM_COMPILER wasixcc)
set (CMAKE_AR wasixar CACHE FILEPATH "")
set (CMAKE_RANLIB wasixranlib CACHE FILEPATH "")
set (CMAKE_C_COMPILER_TARGET ${triple})
set (CMAKE_CXX_COMPILER_TARGET ${triple})
set (CMAKE_FIND_ROOT_PATH $ENV{WASIX_SYSROOT})
set (CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set (CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set (CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set (CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
# the checks of CMake link a test program, which wasixcc can
set (CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
