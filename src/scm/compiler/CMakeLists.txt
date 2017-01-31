add_library(hans.scm.compiler SHARED
  extension.cpp)

target_link_libraries(hans.scm.compiler
  hansscm
  hansengine)

install(TARGETS hans.scm.compiler DESTINATION lib)
