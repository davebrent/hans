add_subdirectory(plugins)

add_library(hansplugin SHARED
  audio_buffers.cpp
  audio_buses.cpp
  context.cpp
  fbos.cpp
  hasher.cpp
  strings.cpp
  linear_allocator.cpp
  registers.cpp
  ring_buffers.cpp
  parameters.cpp
  plugins.cpp
  shaders.cpp)

target_link_libraries(hansplugin ${EPOXY_LIBRARIES})
install(TARGETS hansplugin DESTINATION lib)

add_library(hans SHARED
  audio_backend.cpp
  audio_backend_jack.cpp
  audio_backend_portaudio.cpp
  engine.cpp
  graphics_debug.cpp
  image.cpp
  interpreter.cpp
  midi.cpp
  modulation.cpp
  replay.cpp
  sequencer.cpp
  tasks.cpp
  user_config_compiler.cpp
  user_config_loader.cpp
  video.cpp
  window.cpp)

target_link_libraries(hans
  hansplugin
  ${EPOXY_LIBRARIES}
  ${FREEIMAGE_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${JACK_LIBRARIES}
  ${LIBEFSW_LIBRARIES}
  ${OPENGL_LIBRARIES}
  ${PORTAUDIO_LIBRARIES}
  ${RTMIDI_LIBRARIES}
  ${VPX_LIBRARIES})

install(TARGETS hans DESTINATION lib)

add_executable(hans-bin main.cpp)
target_link_libraries(hans-bin hans)
set_target_properties(hans-bin PROPERTIES OUTPUT_NAME hans)
install(TARGETS hans DESTINATION lib)
