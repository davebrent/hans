set(RESOURCES objects.scm)

foreach(RESOURCE ${RESOURCES})
  configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
endforeach(RESOURCE)

add_library(hans.snd.oscillator SHARED main.cpp)
target_link_libraries(hans.snd.oscillator hansplugin)
install(TARGETS hans.snd.oscillator DESTINATION lib)
