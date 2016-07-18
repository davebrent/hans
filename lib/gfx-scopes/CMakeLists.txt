add_library(hans.gfx.scopes SHARED main.cpp)
install(TARGETS hans.gfx.scopes DESTINATION lib)
target_link_libraries(hans.gfx.scopes hansmodule)
