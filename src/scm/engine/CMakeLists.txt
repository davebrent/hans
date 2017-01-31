add_library(hans.scm.engine SHARED
  extension.cpp)

target_link_libraries(hans.scm.engine
  hansscm
  hansengine)

install(TARGETS hans.scm.engine DESTINATION lib)
