#ifndef HANS_ENGINE_ENGINE_H
#define HANS_ENGINE_ENGINE_H

#include "hans/audio/AudioBufferManager.hpp"
#include "hans/audio/AudioBusManager.hpp"
#include "hans/audio/RingBufferManager.hpp"
#include "hans/common/StringManager.hpp"
#include "hans/engine/LibraryManager.hpp"
#include "hans/engine/ModulationManager.hpp"
#include "hans/engine/ParameterManager.hpp"
#include "hans/engine/Patcher.hpp"
#include "hans/engine/RegisterManager.hpp"
#include "hans/graphics/FrameBufferManager.hpp"
#include "hans/graphics/ShaderManager.hpp"

namespace hans {
namespace engine {

struct Engine {
  RegisterManager* registers;
  audio::AudioBufferManager* audio_buffers;
  audio::AudioBusManager* audio_buses;
  audio::RingBufferManager* ring_buffers;
  ParameterManager* parameters;
  graphics::FrameBufferManager* fbos;
  graphics::ShaderManager* shaders;
  ModulationManager* modulators;
  common::StringManager* strings;
  common::Config* config;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_ENGINE_H
