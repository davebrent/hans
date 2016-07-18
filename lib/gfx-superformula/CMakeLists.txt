add_library(hans.gfx.superformula SHARED main.cpp)
target_link_libraries(hans.gfx.superformula hansmodule)
install(TARGETS hans.gfx.superformula DESTINATION lib)
