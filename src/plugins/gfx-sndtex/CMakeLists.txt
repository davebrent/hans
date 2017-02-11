add_library(hans.gfx.sndtex SHARED main.cpp)
target_link_libraries(hans.gfx.sndtex hansplugin)
install(TARGETS hans.gfx.sndtex DESTINATION lib)
