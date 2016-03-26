add_library(hans.snd.oscillator SHARED main.cpp)
install(TARGETS hans.snd.oscillator DESTINATION lib)
target_link_libraries(hans.snd.oscillator hansmodule)
