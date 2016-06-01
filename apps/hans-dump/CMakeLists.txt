add_executable(hans-dump main.cpp)
target_link_libraries(hans-dump hansmodule hans)
install(TARGETS hans-dump DESTINATION bin)
