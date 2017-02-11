add_library(hans.snd.io SHARED main.cpp)
target_link_libraries(hans.snd.io hansplugin)
install(TARGETS hans.snd.io DESTINATION lib)
