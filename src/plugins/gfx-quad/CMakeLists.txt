add_library(hans.gfx.quad SHARED main.cpp)
target_link_libraries(hans.gfx.quad hansplugin)
install(TARGETS hans.gfx.quad DESTINATION lib)
