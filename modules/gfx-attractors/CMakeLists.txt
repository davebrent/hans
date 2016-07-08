add_library(hans.gfx.attractors SHARED main.cpp)
install(TARGETS hans.gfx.attractors DESTINATION lib)
target_link_libraries(hans.gfx.attractors
    hansmodule
    ${OPENGL_LIBRARIES}
    ${GLFW_LIBRARIES}
    ${EPOXY_LIBRARIES})
