add_library(hansplugin SHARED
  ../engine/AudioBufferManager.cpp
  ../engine/AudioBusManager.cpp
  ../engine/RingBufferManager.cpp
  ../engine/RegisterManager.cpp
  ../engine/ParameterManager.cpp
  ../engine/ShaderManager.cpp
  ../engine/FrameBufferManager.cpp)

target_link_libraries(hansplugin
  hanscommon
  ${EPOXY_LIBRARIES})

install(TARGETS hansplugin DESTINATION lib)
