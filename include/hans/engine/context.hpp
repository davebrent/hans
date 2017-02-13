#ifndef HANS_ENGINE_CONTEXT_H
#define HANS_ENGINE_CONTEXT_H

#include "hans/engine/audio_buffers.hpp"
#include "hans/engine/audio_buses.hpp"
#include "hans/engine/configurator.hpp"
#include "hans/engine/fbos.hpp"
#include "hans/engine/parameters.hpp"
#include "hans/engine/plugins.hpp"
#include "hans/engine/registers.hpp"
#include "hans/engine/ring_buffers.hpp"
#include "hans/engine/shaders.hpp"
#include "hans/engine/strings.hpp"

namespace hans {
namespace engine {

struct context {
  Settings settings;
  StringManager strings;
  RegisterManager registers;
  ParameterManager parameters;
  ShaderManager shaders;
  FrameBufferManager fbos;
  AudioBuffers audio_buffers;
  AudioBuses& audio_buses;
  RingBufferManager ring_buffers;

  context(EngineData& data, AudioBuses& buses);
  context(const context& other) = delete;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_CONTEXT_H
