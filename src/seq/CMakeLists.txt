add_library(hansseq SHARED
  sequencer_realtime.cpp
  midi.cpp)

include_directories(${RTMIDI_INCLUDE_DIRS})
link_directories(${RTMIDI_LIBRARY_DIRS})

target_link_libraries(hansseq
  ${RTMIDI_LIBRARIES})

install(TARGETS hansseq DESTINATION lib)
