add_library(hans.snd.io SHARED main.cpp)
install(TARGETS hans.snd.io DESTINATION lib)
target_link_libraries(hans.snd.io hansmodule)
