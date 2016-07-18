#include <cassert>
#include <cstdlib>
#include "hans/engine/object.hpp"

// "name"
#define SND_RB_ARG_NAME 0xd4c943cba60c270b

typedef struct {
  hans_hash name;
  hans_register inlet;
  hans_register outlet;
  hans_ring_buffer ringbuffer;
} hans_rb_data;

static void hans_rb_parse_args(hans_constructor_api* api, hans_rb_data* data) {
  auto args = api->get_arguments(api);
  for (int i = 0; i < args.length; ++i) {
    switch (args.data[i].name) {
    case SND_RB_ARG_NAME:
      assert(args.data[i].type == HANS_STRING);
      data->name = args.data[i].string;
      break;
    }
  }
}

static void hans_rb_new(hans_constructor_api* api, void* buffer, size_t size) {
  hans_rb_data* data = static_cast<hans_rb_data*>(buffer);
  hans_rb_parse_args(api, data);

  uint8_t num_inlets = 1;
  uint8_t num_outlets = 1;

  api->request_resource(api, HANS_INLET, &num_inlets);
  api->request_resource(api, HANS_OUTLET, &num_outlets);
  api->request_resource(api, HANS_RING_BUFFER, &data->name);
}

static void hans_rb_setup(hans_audio_object* self, hans_object_api* api) {
  hans_rb_data* data = static_cast<hans_rb_data*>(self->data);
  data->inlet = api->registers->make(self->id, HANS_INLET, 0);
  data->outlet = api->registers->make(self->id, HANS_OUTLET, 0);
  data->ringbuffer = api->ring_buffers->make(self->id, data->name);
}

static void hans_rb_callback(hans_audio_object* self, hans_object_api* api) {
  hans_rb_data* data = static_cast<hans_rb_data*>(self->data);

  auto& inlet = data->inlet;
  auto& outlet = data->outlet;
  auto samples = static_cast<hans_audio_sample*>(api->registers->read(inlet));

  api->ring_buffers->write(data->ringbuffer, samples);
  api->registers->write(outlet, samples);
}

void hans_rb_init(void* instance) {
  auto object = static_cast<hans_audio_object*>(instance);
  object->setup = hans_rb_setup;
  object->callback = hans_rb_callback;
}

extern "C" {
void setup(hans_library_api* api) {
  size_t size = sizeof(hans_rb_data);
  api->register_object(api, "snd-ringbuffer", size, hans_rb_new, hans_rb_init,
                       nullptr);
}
}
