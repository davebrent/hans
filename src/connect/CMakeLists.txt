add_library(hansconnect SHARED
  Midi.cpp
  main.cpp)

target_link_libraries(hansconnect
  ${RTMIDI_LIBRARIES}
  ${GUILE_LIBRARIES})

install(TARGETS hansconnect DESTINATION lib)
