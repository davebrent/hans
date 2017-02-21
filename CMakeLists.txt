cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
project(hans)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

find_package(PkgConfig REQUIRED)
find_package(OpenGL REQUIRED)
find_package(FreeImage REQUIRED)

pkg_search_module(GLFW REQUIRED glfw3)
pkg_search_module(EPOXY REQUIRED epoxy)
pkg_search_module(RTMIDI REQUIRED rtmidi)

pkg_search_module(PORTAUDIO QUIET portaudio-2.0)
if(PORTAUDIO_FOUND)
  include_directories(${PORTAUDIO_INCLUDE_DIRS})
  link_directories(${PORTAUDIO_LIBRARY_DIRS})
endif(PORTAUDIO_FOUND)

pkg_search_module(JACK QUIET jack)
if(JACK_FOUND)
  include_directories(${JACK_INCLUDE_DIRS})
  link_directories(${JACK_LIBRARY_DIRS})
endif(JACK_FOUND)

include_directories(${GLFW_INCLUDE_DIRS})
include_directories(${EPOXY_INCLUDE_DIRS})
include_directories(${RTMIDI_INCLUDE_DIRS})
include_directories(${LIBEFSW_INCLUDE_DIRS})

link_directories(${GLFW_LIBRARY_DIRS})
link_directories(${EPOXY_LIBRARY_DIRS})
link_directories(${RTMIDI_LIBRARY_DIRS})
link_directories(${LIBEFSW_LIBRARY_DIRS})

include(FindUBSan)
include(FindASan)
include(FindMSan)
include(FindTSan)
include(FindLibEFSW)
include(FindLibVpx)
include(FindLibWebP)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wno-sign-compare")
# set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${CMAKE_CXX_FLAGS_UBSAN}")
# set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${CMAKE_CXX_FLAGS_MSAN}")
# set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${CMAKE_CXX_FLAGS_ASAN}")
# set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${CMAKE_CXX_FLAGS_TSAN}")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib")
set(SCM_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/hans")

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/include/hans/config.hpp.in"
  "${CMAKE_CURRENT_BINARY_DIR}/include/hans/config.hpp")

include_directories(include)
include_directories("${CMAKE_CURRENT_BINARY_DIR}/include")
add_subdirectory(src)
add_subdirectory(test)
