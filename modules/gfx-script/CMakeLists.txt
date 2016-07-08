add_library(hans.gfx.script SHARED
  IMRenderer.cpp
  main.cpp)

target_link_libraries(hans.gfx.script
  hansmodule
  ${GUILE_LIBRARIES}
  ${NANOVG_LIBRARIES})

install(TARGETS hans.gfx.script DESTINATION lib)
