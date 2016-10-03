add_library(hansmodule SHARED
  ../engine/AudioBufferManager.cpp
  ../engine/AudioBusManager.cpp
  ../engine/RingBufferManager.cpp
  ../engine/RegisterManager.cpp
  ../engine/ParameterManager.cpp
  ../engine/ShaderManager.cpp
  ../engine/FrameBufferManager.cpp)

target_link_libraries(hansmodule
  hanscommon
  ${OPENGL_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${EPOXY_LIBRARIES})

install(TARGETS hansmodule DESTINATION lib)
