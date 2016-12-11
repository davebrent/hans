set(RESOURCES
  objects.scm
  superformula.frag
  superformula.vert)

foreach(RESOURCE ${RESOURCES})
  configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
endforeach(RESOURCE)

add_library(hans.gfx.superformula SHARED main.cpp)
target_link_libraries(hans.gfx.superformula hansplugin)
install(TARGETS hans.gfx.superformula DESTINATION lib)
