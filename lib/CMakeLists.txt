set(HANS_MODULE_SOURCES
    audio/AudioBufferManager.cpp
    audio/AudioBusManager.cpp
    engine/RegisterManager.cpp
    engine/ParameterManager.cpp
    graphics/ShaderManager.cpp
    graphics/FrameBufferManager.cpp
    common/hasher.cpp
    common/StringManager.cpp
    common/LinearAllocator.cpp
    common/Logging.cpp
)

set(HANS_SOURCES
    common/ObjectGraph.cpp
    audio/AudioDevices.cpp
    audio/AudioStream.cpp
    engine/DataLoader.cpp
    engine/ProgramManager.cpp
    engine/LibraryManager.cpp
    engine/Program.cpp    
    graphics/FrameBufferManager.cpp
    graphics/Window.cpp
)

add_subdirectory(sequencer)

add_library(hansmodule ${HANS_MODULE_SOURCES})
add_library(hans ${HANS_SOURCES})

install(TARGETS hans DESTINATION lib)
install(TARGETS hansmodule DESTINATION lib)
