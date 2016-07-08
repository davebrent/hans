add_executable(hans-dump
  ../../lib/common/DataLoader.cpp
  ../../lib/common/StringManager.cpp
  ../../lib/common/LinearAllocator.cpp
  ../../lib/common/hasher.cpp
  main.cpp)

target_link_libraries(hans-dump)
install(TARGETS hans-dump DESTINATION bin)
