add_library(hans.snd.utils SHARED main.cpp)
install(TARGETS hans.snd.utils DESTINATION lib)
target_link_libraries(hans.snd.utils hansmodule)
