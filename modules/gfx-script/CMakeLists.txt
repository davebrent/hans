add_library(hans.gfx.script SHARED
    IMRenderer.cpp
    main.cpp)

target_link_libraries(hans.gfx.script
    hansmodule
    ${OPENGL_LIBRARIES}
    ${GLFW_LIBRARIES}
    ${EPOXY_LIBRARIES}
    ${GUILE_LIBRARIES}
    ${NANOVG_LIBRARIES}
)

install(TARGETS hans.gfx.script DESTINATION lib)
