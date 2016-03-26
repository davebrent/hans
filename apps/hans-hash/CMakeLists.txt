add_executable(hans-hash main.cpp)
target_link_libraries(hans-hash hansmodule)
install(TARGETS hans-hash DESTINATION bin)
