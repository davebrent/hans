set(RESOURCES
  objects.scm
  filter.frag.glsl
  filter.vert.glsl)

foreach(RESOURCE ${RESOURCES})
  configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
endforeach(RESOURCE)

add_library(hans.gfx.filter SHARED main.cpp)
target_link_libraries(hans.gfx.filter hansplugin)
install(TARGETS hans.gfx.filter DESTINATION lib)
