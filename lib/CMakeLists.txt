set(HANS_MODULE_SOURCES
    audio/AudioBufferManager.cpp
    audio/AudioBusManager.cpp
    audio/RingBufferManager.cpp
    engine/RegisterManager.cpp
    engine/ParameterManager.cpp
    graphics/ShaderManager.cpp
    graphics/FrameBufferManager.cpp
    common/hasher.cpp
    common/StringManager.cpp
    common/LinearAllocator.cpp
)

set(HANS_SOURCES
    common/DataLoader.cpp
    common/Logging.cpp
    audio/AudioDevices.cpp
    audio/AudioStream.cpp
    engine/LibraryManager.cpp
    engine/ProgramManager.cpp
    graphics/FrameBufferManager.cpp
    graphics/Window.cpp
)

add_subdirectory(sequencer)
add_subdirectory(compiler)

add_library(hansmodule ${HANS_MODULE_SOURCES})
add_library(hans ${HANS_SOURCES})

install(TARGETS hans DESTINATION lib)
install(TARGETS hansmodule DESTINATION lib)
