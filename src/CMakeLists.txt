add_subdirectory(connect)
add_subdirectory(compiler)
add_subdirectory(engine)
add_subdirectory(sequencer)

add_library(hansmodule SHARED
  engine/AudioBufferManager.cpp
  engine/AudioBusManager.cpp
  engine/RingBufferManager.cpp
  engine/RegisterManager.cpp
  engine/ParameterManager.cpp
  engine/ShaderManager.cpp
  engine/FrameBufferManager.cpp
  common/hasher.cpp
  common/StringManager.cpp
  common/LinearAllocator.cpp)

target_link_libraries(hansmodule
  ${OPENGL_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${EPOXY_LIBRARIES})

install(TARGETS hansmodule DESTINATION lib)
