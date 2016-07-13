add_executable(hans-engine main.cpp)

target_link_libraries(hans-engine
    hans
    hansmodule
    ${OPENGL_LIBRARIES}
    ${GUILE_LIBRARIES}
    ${GLFW_LIBRARIES}
    ${EPOXY_LIBRARIES}
    ${LIBAVCODEC_LIBRARIES}
    ${LIBAVDEVICE_LIBRARIES}
    ${LIBAVFILTER_LIBRARIES}
    ${LIBAVFORMAT_LIBRARIES}
    ${LIBAVRESAMPLE_LIBRARIES}
    ${LIBSWSCALE_LIBRARIES}
    ${LIBAVUTIL_LIBRARIES}
    ${PORTAUDIO_LIBRARIES}
)

install(TARGETS hans-engine DESTINATION bin)
