set(PLUGINS
  gfx-attractors
  gfx-filter
  gfx-noise
  gfx-quad
  gfx-scopes
  gfx-script
  gfx-sndtex
  gfx-superformula
  snd-analysis
  snd-io
  snd-oscillator
  snd-ringbuffer
  snd-utils)

foreach(PLUGIN_DIR ${PLUGINS})
  set(PLUGIN_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${PLUGIN_DIR}")
  set(PLUGIN_DEST "${SCM_OUTPUT_DIRECTORY}/plugin/${PLUGIN_DIR}")
  add_subdirectory(${PLUGIN_DIR})
endforeach(PLUGIN_DIR)
