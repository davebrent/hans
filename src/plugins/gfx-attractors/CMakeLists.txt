add_library(hans.gfx.attractors SHARED main.cpp)
target_link_libraries(hans.gfx.attractors hansplugin)
install(TARGETS hans.gfx.attractors DESTINATION lib)
