add_library(hansengine SHARED
  ../common/DataLoader.cpp
  ../common/hasher.cpp
  ../common/StringManager.cpp
  ../common/LinearAllocator.cpp
  AudioDevices.cpp
  AudioStream.cpp
  AudioBufferManager.cpp
  AudioBusManager.cpp
  RingBufferManager.cpp
  Window.cpp
  ShaderManager.cpp
  FrameBufferManager.cpp
  LibraryManager.cpp
  ModulationManager.cpp
  ParameterManager.cpp
  RegisterManager.cpp
  Engine.cpp)

target_link_libraries(hansengine
  ${GUILE_LIBRARIES}
  ${OPENGL_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${EPOXY_LIBRARIES}
  ${PORTAUDIO_LIBRARIES})

install(TARGETS hansengine DESTINATION lib)
