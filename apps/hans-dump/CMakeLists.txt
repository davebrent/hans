add_executable(hans-dump
  ../../src/common/DataLoader.cpp
  ../../src/common/StringManager.cpp
  ../../src/common/LinearAllocator.cpp
  ../../src/common/hasher.cpp
  main.cpp)

target_link_libraries(hans-dump)
install(TARGETS hans-dump DESTINATION bin)
