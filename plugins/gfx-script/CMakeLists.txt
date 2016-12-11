pkg_search_module(NANOVG QUIET libnanovg)

if(NANOVG_FOUND)
  set(RESOURCES objects.scm)

  foreach(RESOURCE ${RESOURCES})
    configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
  endforeach(RESOURCE)

  include_directories(${NANOVG_INCLUDE_DIRS})
  link_directories(${NANOVG_LIBRARY_DIRS})

  add_library(hans.gfx.script SHARED
    IMRenderer.cpp
    main.cpp)

  target_link_libraries(hans.gfx.script
    hansplugin
    ${GUILE_LIBRARIES}
    ${NANOVG_LIBRARIES})

  install(TARGETS hans.gfx.script DESTINATION lib)
endif(NANOVG_FOUND)
