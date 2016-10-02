add_subdirectory(control)
add_subdirectory(compiler)
add_subdirectory(engine)
add_subdirectory(sequencer)

add_library(hansmodule SHARED
  audio/AudioBufferManager.cpp
  audio/AudioBusManager.cpp
  audio/RingBufferManager.cpp
  engine/RegisterManager.cpp
  engine/ParameterManager.cpp
  graphics/ShaderManager.cpp
  graphics/FrameBufferManager.cpp
  common/hasher.cpp
  common/StringManager.cpp
  common/LinearAllocator.cpp)

target_link_libraries(hansmodule
  ${OPENGL_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${EPOXY_LIBRARIES})

install(TARGETS hansmodule DESTINATION lib)
