add_library(hans.gfx.sndtex SHARED main.cpp)
install(TARGETS hans.gfx.sndtex DESTINATION lib)
target_link_libraries(hans.gfx.sndtex hansmodule)
