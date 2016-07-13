add_library(hanssequencer SHARED
    bindings.cpp
    Device.cpp
    MidiOutBackend.cpp
    ConsoleBackend.cpp)

target_link_libraries(hanssequencer
  ${RTMIDI_LIBRARIES}
  ${GUILE_LIBRARIES})

install(TARGETS hanssequencer DESTINATION lib)
