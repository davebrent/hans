find_path(LIBEFSW_INCLUDE_DIRS "efsw.hpp"
  PATHS
    ${CMAKE_SOURCE_DIR}/include
    /usr/local/include/efsw
    /usr/include/efsw)

find_library(LIBEFSW_LIBRARY_DIRS libefsw libefsw.so libefsw.dylib
  PATHS
    ${CMAKE_SOURCE_DIR}/lib
    /usr/local/lib
    /usr/lib)

SET(LIBEFSW_LIBRARIES ${LIBEFSW_LIBRARY_DIRS})

set(LIBEFSW_FOUND FALSE)
if(LIBEFSW_LIBRARY_DIRS AND LIBEFSW_INCLUDE_DIRS)
  set(LIBEFSW_FOUND TRUE)
endif(LIBEFSW_LIBRARY_DIRS AND LIBEFSW_INCLUDE_DIRS)
