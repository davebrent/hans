pkg_search_module(AUBIO QUIET aubio)

if(AUBIO_FOUND)
  set(RESOURCES objects.scm)

  foreach(RESOURCE ${RESOURCES})
    configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
  endforeach(RESOURCE)

  include_directories(${AUBIO_INCLUDE_DIRS})
  link_directories(${AUBIO_LIBRARY_DIRS})
  add_library(hans.snd.analysis SHARED main.cpp)
  target_link_libraries(hans.snd.analysis hansplugin ${AUBIO_LIBRARIES})
  install(TARGETS hans.snd.analysis DESTINATION lib)
endif(AUBIO_FOUND)
