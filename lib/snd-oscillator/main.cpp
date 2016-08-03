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

struct OscState {
  unsigned samplerate;
  uint8_t channels;
  unsigned phase;
  hans_parameter frequency;
  hans_parameter waveform;
  hans_audio_buffer buffer;
  hans_register outlets[SND_OSC_MAX_CHANNELS];
};

class OscObject : protected AudioObject {
  friend class hans::engine::LibraryManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(ObjectPatcher& patcher) override;
  virtual void setup(hans_object_api& api) override;
  virtual void callback(hans_object_api& api) override;

 private:
  OscState state;
};

void OscObject::create(ObjectPatcher& patcher) {
  state.phase = 0;
  state.channels = 1;

  auto args = patcher.get_args();
  for (auto i = 0; i < args.length; ++i) {
    const auto& arg = args.data[i];
    if (arg.name == ARG_CHANNELS && arg.type == HANS_NUMBER) {
      state.channels = arg.number;
    } else if (arg.name == ARG_PHASE && arg.type == HANS_NUMBER) {
      state.phase = arg.number;
    }
  }

  patcher.request(HANS_OUTLET, state.channels);
}

void OscObject::setup(hans_object_api& api) {
  state.samplerate = api.config->samplerate;
  state.waveform = api.parameters->make(id, PARAM_WAVEFORM);
  state.frequency = api.parameters->make(id, PARAM_FREQUENCY);
  state.buffer = api.audio_buffers->make(id, AUDIO_BUFFER);

  for (auto i = 0; i < state.channels; ++i) {
    state.outlets[i] = api.registers->make(id, HANS_OUTLET, i);
  }
}

static void sine_callback(OscState& state, hans_object_api& api) {
  auto output = api.audio_buffers->get(state.buffer, 0);
  auto frequency = api.parameters->get(state.frequency, 0);

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

static void noise_callback(OscState& state, hans_object_api& api) {
  auto output = api.audio_buffers->get(state.buffer, 0);
  for (int i = 0; i < state.buffer.size; ++i) {
    auto noise = rand() / (float)RAND_MAX;
    noise = noise * 2 - 1;
    output[i] = noise;
  }
}

void OscObject::callback(hans_object_api& api) {
  if (api.parameters->get(state.waveform, 0) > 0.5) {
    noise_callback(state, api);
  } else {
    sine_callback(state, api);
  }

  auto samples = api.audio_buffers->get(state.buffer, 0);
  for (auto i = 0; i < state.channels; ++i) {
    auto& outlet = state.outlets[i];
    api.registers->write(outlet, samples);
  }
}

extern "C" {
void setup(engine::LibraryManager* library) {
  library->add_object<OscState, OscObject>("snd-oscillator");
}
}
