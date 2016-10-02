#ifndef HANS_ENGINE_ENGINE_H
#define HANS_ENGINE_ENGINE_H

#include "hans/audio/AudioBufferManager.hpp"
#include "hans/audio/AudioBusManager.hpp"
#include "hans/audio/AudioDevices.hpp"
#include "hans/audio/RingBufferManager.hpp"
#include "hans/common/DataLoader.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/engine/LibraryManager.hpp"
#include "hans/engine/ModulationManager.hpp"
#include "hans/engine/ParameterManager.hpp"
#include "hans/engine/Patcher.hpp"
#include "hans/engine/RegisterManager.hpp"
#include "hans/graphics/FrameBufferManager.hpp"
#include "hans/graphics/ShaderManager.hpp"
#include "hans/graphics/Window.hpp"

namespace hans {
namespace engine {

struct Engine {
  common::Config config;
  common::StringManager strings;
  LibraryManager libraries;
  RegisterManager registers;
  ParameterManager parameters;
  ModulationManager modulators;
  graphics::Window window;
  graphics::ShaderManager shaders;
  graphics::FrameBufferManager fbos;
  audio::AudioDevices audio_devices;
  audio::AudioBufferManager audio_buffers;
  audio::AudioBusManager audio_buses;
  audio::RingBufferManager ring_buffers;

  Engine(common::Config& config, common::DataReader* reader);
  Engine(const Engine& other) = delete;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_ENGINE_H
