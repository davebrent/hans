set(RESOURCES
  objects.scm
  particle.frag
  particle.vert)

foreach(RESOURCE ${RESOURCES})
  configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
endforeach(RESOURCE)

add_library(hans.gfx.attractors SHARED main.cpp)
target_link_libraries(hans.gfx.attractors hansplugin)
install(TARGETS hans.gfx.attractors DESTINATION lib)
