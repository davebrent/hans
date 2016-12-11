set(RESOURCES
  objects.scm
  quad.frag
  quad.vert)

foreach(RESOURCE ${RESOURCES})
  configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
endforeach(RESOURCE)

add_library(hans.gfx.quad SHARED main.cpp)
target_link_libraries(hans.gfx.quad hansplugin)
install(TARGETS hans.gfx.quad DESTINATION lib)
