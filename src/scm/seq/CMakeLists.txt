add_library(hans.scm.seq SHARED
  extension.cpp)

target_link_libraries(hans.scm.seq
  hansscm
  hanssequencer)

install(TARGETS hans.scm.seq DESTINATION lib)
