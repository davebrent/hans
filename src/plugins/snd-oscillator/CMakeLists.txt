add_library(hans.snd.oscillator SHARED main.cpp)
target_link_libraries(hans.snd.oscillator hansplugin)
install(TARGETS hans.snd.oscillator DESTINATION lib)
