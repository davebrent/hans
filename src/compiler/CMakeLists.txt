add_library(hanscompiler SHARED
  Compiler.cpp
  ../engine/LibraryManager.cpp
  ../engine/ShaderManager.cpp)

target_link_libraries(hanscompiler
  hanscommon
  ${GUILE_LIBRARIES}
  ${OPENGL_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${EPOXY_LIBRARIES})

install(TARGETS hanscompiler DESTINATION lib)
