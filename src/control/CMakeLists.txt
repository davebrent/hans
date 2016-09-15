add_library(hanscontrol SHARED
  Midi.cpp
  main.cpp)

target_link_libraries(hanscontrol
  ${RTMIDI_LIBRARIES}
  ${GUILE_LIBRARIES})

install(TARGETS hanscontrol DESTINATION lib)
