add_library(hans.gfx.planes SHARED main.cpp)

target_link_libraries(hans.gfx.planes
  hansplugin
  ${NANOVG_LIBRARIES})

install(TARGETS hans.gfx.planes DESTINATION lib)
