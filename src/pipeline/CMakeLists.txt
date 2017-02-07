add_library(hanspipeline SHARED
  input.cpp
  output.cpp)

target_link_libraries(hanspipeline
  hansengine)

install(TARGETS hanspipeline DESTINATION lib)
