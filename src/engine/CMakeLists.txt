add_library(hansengine SHARED
  AudioBackendPortAudio.cpp
  AudioBufferManager.cpp
  AudioBusManager.cpp
  RingBufferManager.cpp
  Window.cpp
  ShaderManager.cpp
  FrameBufferManager.cpp
  PluginManager.cpp
  ModulationManager.cpp
  ParameterManager.cpp
  RegisterManager.cpp
  replay.cpp
  Engine.cpp)

target_link_libraries(hansengine
  hanscommon
  ${GUILE_LIBRARIES}
  ${OPENGL_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${EPOXY_LIBRARIES}
  ${PORTAUDIO_LIBRARIES})

install(TARGETS hansengine DESTINATION lib)
