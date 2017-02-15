# - Try to find LibVpx
# Once done this will define
#
#  VPX_FOUND - system has LibVpx
#  VPX_INCLUDE_DIRS - the LibVpx include directory
#  VPX_LIBRARIES - the libraries needed to use LibVpx

find_path(VPX_INCLUDE_DIRS NAMES vpx/vp8.h)
find_library(VPX_LIBRARIES NAMES vpx)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(LIBVPX REQUIRED_VARS VPX_INCLUDE_DIRS VPX_LIBRARIES)

mark_as_advanced(VPX_INCLUDE_DIRS VPX_LIBRARIES)

set(VPX_FOUND FALSE)
if(VPX_LIBRARIES AND VPX_INCLUDE_DIRS)
  set(VPX_FOUND TRUE)
endif(VPX_LIBRARIES AND VPX_INCLUDE_DIRS)
