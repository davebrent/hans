add_library(hans.gfx.superformula SHARED main.cpp)
target_link_libraries(hans.gfx.superformula
    hansmodule
    ${OPENGL_LIBRARIES}
    ${GLFW_LIBRARIES}
    ${EPOXY_LIBRARIES}
)

install(TARGETS hans.gfx.superformula DESTINATION lib)
