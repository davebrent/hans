add_library(hans.gfx.displace SHARED main.cpp)

target_link_libraries(hans.gfx.displace
  hansplugin
  ${NANOVG_LIBRARIES})

install(TARGETS hans.gfx.displace DESTINATION lib)
