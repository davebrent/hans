add_library(hansconnect SHARED
  Midi.cpp
  main.cpp)

include_directories(${RTMIDI_INCLUDE_DIRS})
link_directories(${RTMIDI_LIBRARY_DIRS})

target_link_libraries(hansconnect
  ${RTMIDI_LIBRARIES}
  ${GUILE_LIBRARIES})

install(TARGETS hansconnect DESTINATION lib)
