#include "hans/engine/object.hpp"
#include "./snd.io_generated.h"
#include <cassert>
#include <iostream>

typedef struct {
  // Audio bus to read/write data from
  uint16_t bus;
  // Channels to copy to outlets/inlets
  std::vector<uint8_t> channels;
  // Handles to outlets/inlets
  std::vector<hans_register_handle> pads;
  // Signal buffers
  std::vector<hans_audio_buffer*> buffers;
} hans_io_data;

static void hans_io_get_resources(hans_io_data* data,
                                  hans_object_resource* resources, int len) {
  for (int i = 0; i < len; ++i) {
    auto resource = &resources[i];
    switch (resource->type) {
    case HANS_INLET:
      data->pads.push_back(resource->inlet);
      break;

    case HANS_OUTLET:
      data->pads.push_back(resource->outlet);
      break;

    case HANS_AUDIO_BUFFER:
      data->buffers.push_back(resource->audio_buffer);
      break;

    default:
      assert(false && "Unhandled resource");
      break;
    }
  }
}

static void hans_io_parse_args(hans_constructor_api* api, hans_io_data* data) {
  auto args = api->get_args(api);
  for (int i = 0; i < args.length; ++i) {
    switch (args.data[i].name) {
    case LIBHANS_SND_IO_ARG_BUS:
      assert(args.data[i].type == HANS_NUMBER);
      data->bus = args.data[i].number;
      break;

    case LIBHANS_SND_IO_ARG_CHANNEL:
      assert(args.data[i].type == HANS_NUMBER);
      data->channels.push_back(args.data[i].number);
      break;
    }
  }
}

static hans_io_data* hans_io_patch_object(hans_audio_object* object,
                                          void* buff) {
  void* offset = static_cast<char*>(buff) + sizeof(hans_audio_object);
  hans_io_data* data = static_cast<hans_io_data*>(offset);
  object->data = data;
  return data;
}

static void hans_io_callback_in(hans_audio_object* self, hans_object_api* api) {
  hans_io_data* data = static_cast<hans_io_data*>(self->data);

  for (int i = 0, size = data->channels.size(); i < size; ++i) {
    // Read data from the audio bus and write to the outlets
    // bus -> outlets
    auto channel = data->channels.at(i);
    hans_audio_sample* samples = api->audio_buses->read(data->bus, channel);

    hans_register_handle outlet = data->pads.at(i);
    bool empty = api->registers->is_empty(outlet);
    if (empty) {
      continue;
    }

    // Copy data into object owned buffer
    auto signal = data->buffers.at(i);
    for (int s = 0; s < signal->samples_len; ++s) {
      signal->samples[0][s] = samples[s];
    }

    api->registers->set_write_reg(outlet, signal);
  }
}

static void hans_io_callback_out(hans_audio_object* self,
                                 hans_object_api* api) {
  hans_io_data* data = static_cast<hans_io_data*>(self->data);

  for (int i = 0, size = data->channels.size(); i < size; ++i) {
    // Read data from the inlets and write to the audio bus
    // inlets -> bus
    auto channel = data->channels.at(i);

    hans_register_handle inlet = data->pads.at(i);
    bool empty = api->registers->is_empty(inlet);
    if (empty) {
      continue;
    }

    void* indata = api->registers->get_read_reg(inlet);
    assert(indata != nullptr);

    auto signal = static_cast<hans_audio_buffer*>(indata);
    bool res = api->audio_buses->write(data->bus, channel, signal->samples[0]);
    assert(res == true);
  }
}

static void hans_io_setup(hans_audio_object* self, hans_object_api* api) {
  hans_io_data* data = static_cast<hans_io_data*>(self->data);
  hans_io_get_resources(data, self->resources, self->num_resources);
}

static void hans_io_new_in(hans_constructor_api* api, void* buffer,
                           size_t size) {
  hans_audio_object* object = static_cast<hans_audio_object*>(buffer);
  object->setup = hans_io_setup;
  object->callback = hans_io_callback_in;

  hans_io_data* data = hans_io_patch_object(object, buffer);
  hans_io_parse_args(api, data);

  auto num_signals = data->channels.size();
  api->request_resource(api, HANS_OUTLET, num_signals);
  api->request_resource(api, HANS_AUDIO_BUFFER, num_signals);
}

static void hans_io_new_out(hans_constructor_api* api, void* buffer,
                            size_t size) {
  hans_audio_object* object = static_cast<hans_audio_object*>(buffer);
  object->setup = hans_io_setup;
  object->callback = hans_io_callback_out;

  hans_io_data* data = hans_io_patch_object(object, buffer);
  hans_io_parse_args(api, data);

  auto num_signals = data->channels.size();
  api->request_resource(api, HANS_INLET, num_signals);
  api->request_resource(api, HANS_AUDIO_BUFFER, num_signals);
}

extern "C" {
void setup(hans_library_api* api) {
  size_t size = sizeof(hans_audio_object) + sizeof(hans_io_data);
  bool success;

  success = api->register_object(api, "snd.in", size, hans_io_new_in, nullptr);
  assert(success == true);

  success =
      api->register_object(api, "snd.out", size, hans_io_new_out, nullptr);
  assert(success == true);
}
}
