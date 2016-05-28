set(HANS_TEST_SOURCES 
	audio/AudioBufferManager.cpp
	common/ObjectGraph.cpp
	engine/ParameterManager.cpp
	engine/Program.cpp
	engine/ProgramManager.cpp
	engine/RegisterManager.cpp
	memory/LinearAllocator.cpp
	memory/StringManager.cpp
	sequencer/Device.cpp
	main.cpp
)

add_executable(hans-unittest ${HANS_TEST_SOURCES})

target_link_libraries(hans-unittest
    hans
    hansmodule
    hans-sequencer-bindings
    ${OPENGL_LIBRARIES}
    ${GLFW_LIBRARIES}
    ${EPOXY_LIBRARIES}
    ${PORTAUDIO_LIBRARIES}
    ${SQLITE3_LIBRARIES}
)
