add_library(hansengine SHARED
  ../audio/AudioDevices.cpp
  ../audio/AudioStream.cpp
  ../audio/AudioBufferManager.cpp
  ../audio/AudioBusManager.cpp
  ../audio/RingBufferManager.cpp
  ../common/DataLoader.cpp
  ../common/hasher.cpp
  ../common/StringManager.cpp
  ../common/LinearAllocator.cpp
  ../graphics/Window.cpp
  ../graphics/ShaderManager.cpp
  ../graphics/FrameBufferManager.cpp
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
