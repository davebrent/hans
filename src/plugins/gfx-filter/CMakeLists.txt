add_library(hans.gfx.filter SHARED main.cpp)
target_link_libraries(hans.gfx.filter hansplugin)
install(TARGETS hans.gfx.filter DESTINATION lib)
