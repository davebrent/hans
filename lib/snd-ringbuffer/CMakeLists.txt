add_library(hans.snd.ringbuffer SHARED main.cpp)
install(TARGETS hans.snd.ringbuffer DESTINATION lib)
target_link_libraries(hans.snd.ringbuffer hansmodule)
