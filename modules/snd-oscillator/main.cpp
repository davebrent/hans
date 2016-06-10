#include <math.h>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include "./buffers.h"
#include "./snd.oscillator_generated.h"
#include "hans/engine/object.hpp"

#define SND_OSC_MAX_CHANNELS 8

typedef struct {
  unsigned samplerate;
  uint8_t channels;
  unsigned phase;
  hans_parameter frequency;
  hans_parameter waveform;
  hans_audio_buffer buffer;
  hans_register outlets[SND_OSC_MAX_CHANNELS];
} hans_osc_data;

static void hans_osc_parse_args(hans_constructor_api* api,
                                hans_osc_data* data) {
  auto args = api->get_arguments(api);
  for (int i = 0; i < args.length; ++i) {
    switch (args.data[i].name) {
    case LIBOSC_ARG_CHANNELS:
      assert(args.data[i].type == HANS_NUMBER);
      data->channels = args.data[i].number;
      break;

    case LIBOSC_ARG_PHASE:
      assert(args.data[i].type == HANS_NUMBER);
      data->phase = args.data[i].number;
      break;
    }
  }
}

// Sine wave oscillator that uses linear interpolation on a 514 point buffer
static void hans_osc_sine_callback(hans_audio_object* self,
                                   hans_object_api* api) {
  auto data = static_cast<hans_osc_data*>(self->data);
  auto output = api->audio_buffers->get(data->buffer, 0);
  auto frequency = api->parameters->get(data->frequency, 0);

  for (int i = 0; i < data->buffer.size; ++i) {
    data->phase += 512.f / (data->samplerate / frequency);

    if (data->phase >= 511) {
      data->phase -= 512;
    }

    auto point_1 = HANS_SINE_BUFFER[1 + data->phase];
    auto point_2 = HANS_SINE_BUFFER[2 + data->phase];
    auto remaining = data->phase - floor(data->phase);
    output[i] = ((1 - remaining) * point_1 + remaining * point_2);
  }
}

// Maybe replace with pure data's implementation?
// https://github.com/pure-data/pure-data/blob/master/src/d_osc.c#L457-L503
static void hans_osc_noise_callback(hans_audio_object* self,
                                    hans_object_api* api) {
  auto data = static_cast<hans_osc_data*>(self->data);
  auto output = api->audio_buffers->get(data->buffer, 0);

  for (int i = 0; i < data->buffer.size; ++i) {
    auto noise = rand() / (float)RAND_MAX;
    noise = noise * 2 - 1;
    output[i] = noise;
  }
}

static void hans_osc_callback(hans_audio_object* self, hans_object_api* api) {
  hans_osc_data* data = static_cast<hans_osc_data*>(self->data);

  if (api->parameters->get(data->waveform, 0) > 0.5) {
    hans_osc_noise_callback(self, api);
  } else {
    hans_osc_sine_callback(self, api);
  }

  auto samples = api->audio_buffers->get(data->buffer, 0);
  for (auto outlet : data->outlets) {
    api->registers->write(outlet, samples);
  }
}

static void hans_osc_setup(hans_audio_object* self, hans_object_api* api) {
  hans_osc_data* data = static_cast<hans_osc_data*>(self->data);
  data->samplerate = api->config->samplerate;
  data->waveform = api->parameters->make(self->id, LIBOSC_PARAM_WAVEFORM);
  data->frequency = api->parameters->make(self->id, LIBOSC_PARAM_FREQUENCY);
  data->buffer = api->audio_buffers->make(self->id, LIBOSC_AUDIO_BUFFER);

  for (auto i = 0; i < data->channels; ++i) {
    data->outlets[i] = api->registers->make(self->id, HANS_OUTLET, i);
  }
}

static void hans_osc_new(hans_constructor_api* api, void* buffer, size_t size) {
  hans_osc_data* data = static_cast<hans_osc_data*>(buffer);
  data->phase = 0;
  data->channels = 1;
  hans_osc_parse_args(api, data);
  api->request_resource(api, HANS_OUTLET, data->channels);
}

void hans_osc_init(void* instance) {
  auto object = static_cast<hans_audio_object*>(instance);
  object->setup = hans_osc_setup;
  object->callback = hans_osc_callback;
}

extern "C" {
void setup(hans_library_api* api) {
  size_t size = sizeof(hans_osc_data);
  api->register_object(api, "snd-oscillator", size, hans_osc_new, hans_osc_init,
                       nullptr);
}
}
