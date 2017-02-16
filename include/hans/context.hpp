#ifndef HANS_CONTEXT_H
#define HANS_CONTEXT_H

#include "hans/audio_buffers.hpp"
#include "hans/audio_buses.hpp"
#include "hans/configurator.hpp"
#include "hans/fbos.hpp"
#include "hans/parameters.hpp"
#include "hans/plugins.hpp"
#include "hans/registers.hpp"
#include "hans/ring_buffers.hpp"
#include "hans/shaders.hpp"
#include "hans/strings.hpp"

namespace hans {

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

} // namespace hans

#endif // HANS_CONTEXT_H
