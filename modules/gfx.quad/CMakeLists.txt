add_library(hans.gfx.quad SHARED main.cpp)
target_link_libraries(hans.gfx.quad
    hansmodule
    ${OPENGL_LIBRARIES}
    ${GLFW_LIBRARIES}
    ${EPOXY_LIBRARIES}
)

install(TARGETS hans.gfx.quad DESTINATION lib)
