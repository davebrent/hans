add_library(hans.gfx.filter SHARED main.cpp)
target_link_libraries(hans.gfx.filter
    hansmodule
    ${OPENGL_LIBRARIES}
    ${GLFW_LIBRARIES}
    ${EPOXY_LIBRARIES}
)

install(TARGETS hans.gfx.filter DESTINATION lib)
