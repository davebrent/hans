# - Try to find LibWebP
# Once done this will define
#
#  WEBP_FOUND - system has LibWebP
#  WEBP_INCLUDE_DIRS - the LibWebP include directory
#  WEBP_LIBRARIES - the libraries needed to use LibWebP

find_path(WEBP_INCLUDE_DIRS NAMES webp/decode.h)
find_library(WEBP_LIBRARIES NAMES webp)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LIBWEBP REQUIRED_VARS WEBP_INCLUDE_DIRS WEBP_LIBRARIES)

mark_as_advanced(WEBP_INCLUDE_DIRS WEBP_LIBRARIES)

set(WEBP_FOUND FALSE)
if(WEBP_LIBRARIES AND WEBP_INCLUDE_DIRS)
  set(WEBP_FOUND TRUE)
endif(WEBP_LIBRARIES AND WEBP_INCLUDE_DIRS)
