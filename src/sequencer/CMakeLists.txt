add_library(hanssequencer SHARED
  Sequencer.cpp
  main.cpp)

target_link_libraries(hanssequencer
  ${RTMIDI_LIBRARIES}
  ${GUILE_LIBRARIES})

install(TARGETS hanssequencer DESTINATION lib)
