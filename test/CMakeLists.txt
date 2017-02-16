add_executable(hans-test
  audio_buses.cpp
  linear_allocator.cpp
  modulation.cpp
  object.cpp
  parameters.cpp
  replay.cpp
  ring_buffers.cpp
  strings.cpp
  interpreter.cpp
  main.cpp)

target_link_libraries(hans-test hans hansplugin)
