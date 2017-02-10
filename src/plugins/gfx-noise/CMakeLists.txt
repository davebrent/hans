find_package(LibNoise QUIET)

if(LIBNOISE_FOUND)
  include_directories(${LIBNOISE_INCLUDE_DIRS})
  link_directories(${LIBNOISE_LIBRARY_DIRS})

  set(RESOURCES objects.scm)

  foreach(RESOURCE ${RESOURCES})
    configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
  endforeach(RESOURCE)

  add_library(hans.gfx.noise SHARED main.cpp)
  target_link_libraries(hans.gfx.noise
    hansplugin
    ${LIBNOISE_LIBRARIES})
  install(TARGETS hans.gfx.noise DESTINATION lib)
endif(LIBNOISE_FOUND)
