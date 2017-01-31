add_library(hansengine SHARED
  audio_backend_jack.cpp
  audio_backend_portaudio.cpp
  image.cpp
  video.cpp
  window.cpp
  plugins.cpp
  modulation.cpp
  replay.cpp
  engine.cpp)

add_library(hansplugin SHARED
  hasher.cpp
  strings.cpp
  linear_allocator.cpp
  audio_buffers.cpp
  audio_buses.cpp
  context.cpp
  ring_buffers.cpp
  registers.cpp
  parameters.cpp
  shaders.cpp
  fbos.cpp)

target_link_libraries(hansplugin
  ${EPOXY_LIBRARIES})

install(TARGETS hansplugin DESTINATION lib)

target_link_libraries(hansengine
  hansplugin
  ${FREEIMAGE_LIBRARIES}
  ${OPENGL_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${EPOXY_LIBRARIES}
  ${PORTAUDIO_LIBRARIES})

install(TARGETS hansengine DESTINATION lib)
