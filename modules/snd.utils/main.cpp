#include "hans/engine/object.hpp"
#include "./snd.utils_generated.h"
#include <cassert>

typedef struct { float gain; } hans_gain_data;

void hans_gain_callback(hans_audio_object* self, hans_object_api* api) {
  hans_gain_data* data = static_cast<hans_gain_data*>(self->data);
}

void hans_gain_setup(hans_audio_object* self, hans_object_api* api) {
  hans_gain_data* data = static_cast<hans_gain_data*>(self->data);
  data->gain = 1;
}

void hans_gain_new(hans_constructor_api* api, void* buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 1);
  api->request_resource(api, HANS_OUTLET, 1);

  hans_audio_object* object = static_cast<hans_audio_object*>(buffer);
  object->setup = hans_gain_setup;
  object->callback = hans_gain_callback;

  void* offset = static_cast<char*>(buffer) + sizeof(hans_audio_object);
  hans_gain_data* data = static_cast<hans_gain_data*>(offset);
  object->data = data;
}

/*
typedef struct {
  float direction;
} hans_pan_data;

void hans_pan_callback(hans_audio_object* self, hans_audio_sample* input,
                      hans_audio_sample* output) {
  hans_pan_data* data = static_cast<hans_pan_data*>(self->data);
}

void hans_pan_setup(hans_audio_object* self, hans_object_api* api) {
  hans_pan_data* data = static_cast<hans_pan_data*>(self->data);
  data->direction = 0;
}

void hans_pan_new(hans_constructor_api* api, void* buffer, size_t size) {
  user_object->inlets_len = 1;
  user_object->outlets_len = 1;

  hans_audio_object* object = static_cast<hans_audio_object*>(buffer);
  object->setup = hans_pan_setup;
  object->callback = hans_pan_callback;

  void* offset = static_cast<char*>(buffer) + sizeof(hans_audio_object);
  hans_pan_data* data = static_cast<hans_pan_data*>(offset);
  object->data = data;
}
*/

extern "C" {
void setup(hans_library_api* api) {
  bool success;
  size_t size;

  size = sizeof(hans_audio_object) + sizeof(hans_gain_data);
  success = api->register_object(api, "snd.gain", size, hans_gain_new, nullptr);
  assert(success == true);

  /*
  size = sizeof(hans_audio_object) + sizeof(hans_pan_data);
  success = api->register_object(api, "snd.pan", size, hans_gain_new, nullptr);
  assert(success == true);
  */
}
}
