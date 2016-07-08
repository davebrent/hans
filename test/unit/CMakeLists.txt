add_executable(hans-unittest
  audio/AudioBufferManager.cpp
  audio/AudioBusManager.cpp
  audio/RingBufferManager.cpp
  common/LinearAllocator.cpp
  common/StringManager.cpp
  engine/ParameterManager.cpp
  engine/RegisterManager.cpp
  sequencer/Device.cpp
  main.cpp)

target_link_libraries(hans-unittest
  hans
  hansmodule
  hanssequencer)
