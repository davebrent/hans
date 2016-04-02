set(HANS_MODULE_SOURCES
    audio/AudioBufferManager.cpp
    audio/AudioBusManager.cpp
    engine/RegisterManager.cpp
    engine/ParameterManager.cpp
    graphics/ShaderManager.cpp
    graphics/FrameBufferManager.cpp
    memory/StringManager.cpp
    memory/LinearAllocator.cpp
    common/Logging.cpp
)

set(HANS_SOURCES
    common/ObjectGraph.cpp
    jsonrpc/methods/AudioStart.cpp
    jsonrpc/methods/AudioStop.cpp
    jsonrpc/methods/GetAudioDevices.cpp
    jsonrpc/methods/GetShaders.cpp
    jsonrpc/methods/GetObjects.cpp
    jsonrpc/methods/GetParameters.cpp
    jsonrpc/methods/SetProgram.cpp
    jsonrpc/methods/UseProgram.cpp
    jsonrpc/methods/utils.cpp
    jsonrpc/Handler.cpp
    jsonrpc/Message.cpp
    jsonrpc/Server.cpp
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
