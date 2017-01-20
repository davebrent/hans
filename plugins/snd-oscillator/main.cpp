#include <math.h>
#include <cstdlib>
#include "./buffers.h"
#include "hans/engine/object.hpp"

#define SND_OSC_MAX_CHANNELS 8
#define ARG_PHASE 0x9c38c179d4311f91       /* phase */
#define PARAM_WAVEFORM 0x6c94e62ff18c189f  /* waveform */
#define ARG_CHANNELS 0xc579d4daf43017c2    /* channels */
#define PARAM_FREQUENCY 0xce6a758b9a7e1778 /* frequency */
#define AUDIO_BUFFER 0x635d6522b5c8c630    /* snd/osc/buffer */

using namespace hans;
using namespace hans::engine;

struct OscState {
  unsigned samplerate;
  uint8_t channels;
  unsigned phase;
  Parameter frequency;
  Parameter waveform;
  audio::Buffer buffer;
  Register outlets[SND_OSC_MAX_CHANNELS];

  template <class Archive>
  void serialize(Archive& ar) {
    ar(channels, phase);
  }
};

class OscObject : protected AudioObject {
  friend class hans::engine::PluginManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void callback(Engine& engine) override;

 private:
  OscState state;
};

void OscObject::create(IPatcher& patcher) {
  state.phase = 0;
  state.channels = 1;

  for (const auto& arg : patcher.arguments()) {
    if (arg.name == ARG_CHANNELS && arg.type == Argument::Types::NUMBER) {
      state.channels = arg.number;
    } else if (arg.name == ARG_PHASE && arg.type == Argument::Types::NUMBER) {
      state.phase = arg.number;
    }
  }

  patcher.request(IPatcher::Resources::OUTLET, state.channels);
}

void OscObject::setup(Engine& engine) {
  state.samplerate = engine.settings.samplerate;
  state.waveform = engine.parameters.make(id, PARAM_WAVEFORM);
  state.frequency = engine.parameters.make(id, PARAM_FREQUENCY);
  state.buffer = engine.audio_buffers.make(id, AUDIO_BUFFER);

  for (auto i = 0; i < state.channels; ++i) {
    state.outlets[i] = engine.registers.make(id, Register::Types::OUTLET, i);
  }
}

static void sine_callback(OscState& state, Engine& engine) {
  auto output = engine.audio_buffers.get(state.buffer, 0);
  auto frequency = engine.parameters.get(state.frequency, 0);

  for (int i = 0; i < state.buffer.size; ++i) {
    state.phase += 512.f / (state.samplerate / frequency);

    if (state.phase >= 511) {
      state.phase -= 512;
    }

    auto point_1 = HANS_SINE_BUFFER[1 + state.phase];
    auto point_2 = HANS_SINE_BUFFER[2 + state.phase];
    auto remaining = state.phase - floor(state.phase);
    output[i] = ((1 - remaining) * point_1 + remaining * point_2);
  }
}

static void noise_callback(OscState& state, Engine& engine) {
  auto output = engine.audio_buffers.get(state.buffer, 0);
  for (int i = 0; i < state.buffer.size; ++i) {
    auto noise = rand() / (float)RAND_MAX;
    noise = noise * 2 - 1;
    output[i] = noise;
  }
}

void OscObject::callback(Engine& engine) {
  if (engine.parameters.get(state.waveform, 0) > 0.5) {
    noise_callback(state, engine);
  } else {
    sine_callback(state, engine);
  }

  auto samples = engine.audio_buffers.get(state.buffer, 0);
  for (auto i = 0; i < state.channels; ++i) {
    auto& outlet = state.outlets[i];
    engine.registers.write(outlet, samples);
  }
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<OscState, OscObject>("snd-oscillator");
}
