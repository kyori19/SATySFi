cmake_minimum_required(VERSION 3.22)

project(satysfi-jupyter-ext VERSION 0.0.10)

add_library(ocaml INTERFACE)
execute_process(COMMAND opam var lib OUTPUT_VARIABLE OPAM_LIB_PATH OUTPUT_STRIP_TRAILING_WHITESPACE COMMAND_ERROR_IS_FATAL ANY)
target_include_directories(ocaml INTERFACE "${OPAM_LIB_PATH}/ocaml")

find_package(PkgConfig REQUIRED)
find_library(b64 b64 REQUIRED)
pkg_check_modules(poppler-cpp REQUIRED poppler-cpp)
find_package(xeus REQUIRED)
find_package(xeus-zmq REQUIRED)

add_library(satysfi-jupyter-ext SHARED
  main.cpp
)
target_compile_features(satysfi-jupyter-ext PRIVATE
  cxx_std_23
)
target_link_libraries(satysfi-jupyter-ext PRIVATE
  b64
  ocaml
  poppler-cpp
  xeus
  xeus-zmq
)
