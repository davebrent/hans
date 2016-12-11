set(RESOURCES objects.scm)

foreach(RESOURCE ${RESOURCES})
  configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
endforeach(RESOURCE)

add_library(hans.gfx.sndtex SHARED main.cpp)
target_link_libraries(hans.gfx.sndtex hansplugin)
install(TARGETS hans.gfx.sndtex DESTINATION lib)
