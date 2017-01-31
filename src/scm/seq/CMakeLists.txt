add_library(hans.scm.seq SHARED
  extension.cpp)

target_link_libraries(hans.scm.seq
  hansscm
  hansseq)

install(TARGETS hans.scm.seq DESTINATION lib)
