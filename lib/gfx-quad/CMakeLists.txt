add_library(hans.gfx.quad SHARED main.cpp)
target_link_libraries(hans.gfx.quad hansmodule)
install(TARGETS hans.gfx.quad DESTINATION lib)
