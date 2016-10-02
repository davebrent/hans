#ifndef HANS_ENGINE_ENGINE_H
#define HANS_ENGINE_ENGINE_H

#include "hans/common/DataLoader.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/engine/AudioBufferManager.hpp"
#include "hans/engine/AudioBusManager.hpp"
#include "hans/engine/AudioDevices.hpp"
#include "hans/engine/FrameBufferManager.hpp"
#include "hans/engine/LibraryManager.hpp"
#include "hans/engine/ModulationManager.hpp"
#include "hans/engine/ParameterManager.hpp"
#include "hans/engine/Patcher.hpp"
#include "hans/engine/RegisterManager.hpp"
#include "hans/engine/RingBufferManager.hpp"
#include "hans/engine/ShaderManager.hpp"
#include "hans/engine/Window.hpp"

namespace hans {
namespace engine {

struct Engine {
  common::Config config;
  common::StringManager strings;
  LibraryManager libraries;
  RegisterManager registers;
  ParameterManager parameters;
  ModulationManager modulators;
  Window window;
  ShaderManager shaders;
  FrameBufferManager fbos;
  AudioDevices audio_devices;
  AudioBufferManager audio_buffers;
  AudioBusManager audio_buses;
  RingBufferManager ring_buffers;

  Engine(common::Config& config, common::DataReader* reader);
  Engine(const Engine& other) = delete;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_ENGINE_H
