add_library(hanssequencer SHARED
  sequencer.cpp
  interpreter.cpp
  midi.cpp)

include_directories(${RTMIDI_INCLUDE_DIRS})
link_directories(${RTMIDI_LIBRARY_DIRS})

target_link_libraries(hanssequencer
  ${RTMIDI_LIBRARIES})

install(TARGETS hanssequencer DESTINATION lib)
