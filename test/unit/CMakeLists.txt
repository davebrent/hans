add_executable(hans-unittest
  engine/AudioBusManager.cpp
  engine/RingBufferManager.cpp
  common/LinearAllocator.cpp
  common/StringManager.cpp
  engine/ParameterManager.cpp
  engine/ModulationManager.cpp
  engine/replay.cpp
  common/smobs.cpp
  main.cpp)

target_link_libraries(hans-unittest
  hanscommon
  hansengine
  hansplugin
  hanssequencer)
