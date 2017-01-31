set(RESOURCES
  objects.scm
  shaders/cgadisplay.frag
  shaders/chromatic-aberration.frag.glsl
  shaders/dotscreen.frag
  shaders/gamma.frag
  shaders/greyscale.frag
  shaders/halftone.frag
  shaders/invert.frag
  shaders/passthrough.frag
  shaders/pixelate.frag
  shaders/quad.vert
  shaders/rgbsplit.frag
  shaders/sepia.frag
  shaders/sobel.frag
  shaders/zoomblur.frag)

foreach(RESOURCE ${RESOURCES})
  configure_file("${PLUGIN_PATH}/${RESOURCE}" "${PLUGIN_DEST}/${RESOURCE}")
endforeach(RESOURCE)

add_library(hans.gfx.filter SHARED main.cpp)
target_link_libraries(hans.gfx.filter hansplugin)
install(TARGETS hans.gfx.filter DESTINATION lib)
