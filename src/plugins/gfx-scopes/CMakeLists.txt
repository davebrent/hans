add_library(hans.gfx.scopes SHARED main.cpp)
target_link_libraries(hans.gfx.scopes hansplugin)
install(TARGETS hans.gfx.scopes DESTINATION lib)
