add_library(hanscompiler SHARED
  Compiler.cpp
  ../common/DataLoader.cpp
  ../common/LinearAllocator.cpp
  ../common/StringManager.cpp
  ../common/hasher.cpp
  ../engine/LibraryManager.cpp
  ../engine/ShaderManager.cpp)

target_link_libraries(hanscompiler
  ${GUILE_LIBRARIES}
  ${OPENGL_LIBRARIES}
  ${GLFW_LIBRARIES}
  ${EPOXY_LIBRARIES})

install(TARGETS hanscompiler DESTINATION lib)
