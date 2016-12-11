set(RESOURCES
  objects.scm
  oscilloscope.frag
  oscilloscope.vert
  phasescope.frag
  phasescope.vert)

foreach(RESOURCE ${RESOURCES})
  configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
endforeach(RESOURCE)

add_library(hans.gfx.scopes SHARED main.cpp)
target_link_libraries(hans.gfx.scopes hansplugin)
install(TARGETS hans.gfx.scopes DESTINATION lib)
