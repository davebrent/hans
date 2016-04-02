add_library(hans-sequencer-bindings SHARED
    bindings.cpp
    Device.cpp
    MidiOutBackend.cpp
    ConsoleBackend.cpp)

target_link_libraries(hans-sequencer-bindings
  ${RTMIDI_LIBRARIES}
  ${GUILE_LIBRARIES})

install(TARGETS hans-sequencer-bindings DESTINATION lib)
