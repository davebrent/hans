find_package(LibNoise QUIET)

if(LIBNOISE_FOUND)
  include_directories(${LIBNOISE_INCLUDE_DIRS})
  link_directories(${LIBNOISE_LIBRARY_DIRS})

  add_library(hans.gfx.noise SHARED main.cpp)
  target_link_libraries(hans.gfx.noise
    hansplugin
    ${LIBNOISE_LIBRARIES})
  install(TARGETS hans.gfx.noise DESTINATION lib)
endif(LIBNOISE_FOUND)
