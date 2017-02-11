add_library(hans.snd.ringbuffer SHARED main.cpp)
target_link_libraries(hans.snd.ringbuffer hansplugin)
install(TARGETS hans.snd.ringbuffer DESTINATION lib)
