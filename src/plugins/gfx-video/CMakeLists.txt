add_library(hans.gfx.video SHARED main.cpp)
target_link_libraries(hans.gfx.video
  hansplugin
  ${VPX_LIBRARIES})
install(TARGETS hans.gfx.video DESTINATION lib)
