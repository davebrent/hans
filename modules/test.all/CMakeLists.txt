add_library(hans.test.all SHARED main.cpp)
target_link_libraries(hans.test.all hansmodule)
install(TARGETS hans.test.all DESTINATION lib)
