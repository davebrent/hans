add_executable(hans-test
  engine/audio_buses.cpp
  engine/linear_allocator.cpp
  engine/modulation.cpp
  engine/object.cpp
  engine/parameters.cpp
  engine/replay.cpp
  engine/ring_buffers.cpp
  engine/strings.cpp
  scm/smobs.cpp
  main.cpp)

target_link_libraries(hans-test
  hansengine
  hansplugin
  ${GUILE_LIBRARIES})
