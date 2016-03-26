add_library(hans.snd.loadfile SHARED main.cpp)

target_link_libraries(hans.snd.loadfile
    hansmodule
    ${LIBAVCODEC_LIBRARIES}
    ${LIBAVDEVICE_LIBRARIES}
    ${LIBAVFILTER_LIBRARIES}
    ${LIBAVFORMAT_LIBRARIES}
    ${LIBAVRESAMPLE_LIBRARIES}
    ${LIBSWSCALE_LIBRARIES}
    ${LIBAVUTIL_LIBRARIES}
)

install(TARGETS hans.snd.loadfile DESTINATION lib)
