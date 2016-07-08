add_executable(hans-hash 
  ../../lib/common/hasher.cpp
  main.cpp)
target_link_libraries(hans-hash)
install(TARGETS hans-hash DESTINATION bin)
