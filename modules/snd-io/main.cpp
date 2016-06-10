#include <stdexcept>
#include "./snd.io_generated.h"
#include "hans/engine/object.hpp"

#define SND_IO_MAX_CHANNELS 8

typedef struct {
  // Audio bus ID
  uint16_t bus;
  uint8_t channels_len;
  // Channel ID's from which to data from/to
  uint8_t channels[SND_IO_MAX_CHANNELS];
  hans_register registers[SND_IO_MAX_CHANNELS];
} hans_io_data;

static void hans_io_parse_args(hans_constructor_api* api, hans_io_data* data) {
  auto args = api->get_arguments(api);

  data->channels_len = 0;

  for (int i = 0; i < args.length; ++i) {
    switch (args.data[i].name) {
    case LIBHANS_SND_IO_ARG_BUS:
      if (args.data[i].type != HANS_NUMBER) {
        throw std::runtime_error("snd.io bus ID must be a number");
      }
      data->bus = args.data[i].number;
      break;

    case LIBHANS_SND_IO_ARG_CHANNEL:
      if (data->channels_len == SND_IO_MAX_CHANNELS) {
        throw std::runtime_error("snd.io has to channels");
      }
      if (args.data[i].type != HANS_NUMBER) {
        throw std::runtime_error("snd.io channel must be a number");
      }
      data->channels[data->channels_len] = args.data[i].number;
      data->channels_len++;
      break;
    }
  }
}

static void hans_io_setup_in(hans_audio_object* self, hans_object_api* api) {
  auto data = static_cast<hans_io_data*>(self->data);
  for (auto i = 0; i < data->channels_len; ++i) {
    data->registers[i] = api->registers->make(self->id, HANS_OUTLET, i);
  }
}

static void hans_io_setup_out(hans_audio_object* self, hans_object_api* api) {
  auto data = static_cast<hans_io_data*>(self->data);
  for (auto i = 0; i < data->channels_len; ++i) {
    data->registers[i] = api->registers->make(self->id, HANS_INLET, i);
  }
}

static void hans_io_callback_in(hans_audio_object* self, hans_object_api* api) {
  auto data = static_cast<hans_io_data*>(self->data);

  for (auto i = 0; i < data->channels_len; ++i) {
    // Read data from the audio bus and write to the outlets
    auto samples = api->audio_buses->read(data->bus, data->channels[i]);
    api->registers->write(data->registers[i], samples);
  }
}

static void hans_io_callback_out(hans_audio_object* self,
                                 hans_object_api* api) {
  auto data = static_cast<hans_io_data*>(self->data);

  for (auto i = 0; i < data->channels_len; ++i) {
    // Read data from the inlets and write to the audio bus
    auto inlet = data->registers[i];
    auto samples = static_cast<hans_audio_sample*>(api->registers->read(inlet));
    api->audio_buses->write(data->bus, data->channels[i], samples);
  }
}

static void hans_io_new_in(hans_constructor_api* api, void* buffer,
                           size_t size) {
  auto data = static_cast<hans_io_data*>(buffer);
  hans_io_parse_args(api, data);
  api->request_resource(api, HANS_OUTLET, data->channels_len);
}

static void hans_io_new_out(hans_constructor_api* api, void* buffer,
                            size_t size) {
  auto data = static_cast<hans_io_data*>(buffer);
  hans_io_parse_args(api, data);
  api->request_resource(api, HANS_INLET, data->channels_len);
}

void hans_io_init_in(void* instance) {
  auto object = static_cast<hans_audio_object*>(instance);
  object->setup = hans_io_setup_in;
  object->callback = hans_io_callback_in;
}

void hans_io_init_out(void* instance) {
  auto object = static_cast<hans_audio_object*>(instance);
  object->setup = hans_io_setup_out;
  object->callback = hans_io_callback_out;
}

extern "C" {
void setup(hans_library_api* api) {
  size_t size = sizeof(hans_io_data);
  api->register_object(api, "snd-in", size, hans_io_new_in, hans_io_init_in,
                       nullptr);
  api->register_object(api, "snd-out", size, hans_io_new_out, hans_io_init_out,
                       nullptr);
}
}
