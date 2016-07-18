#include <cassert>
#include <cstring>
#include <iostream>
#include "hans/engine/object.hpp"

using namespace hans;

// "name"
#define GFX_RB_ARG_NAME 0xd4c943cba60c270b
#define GFX_RB_MAX_FRAMES 30

typedef struct {
  GLuint texture;
  hans_hash name;
  hans_register outlet;
  uint32_t texture_value;
  hans_audio_sample* samples;
} sndtex_data;

static void sndtex_parse_args(hans_constructor_api* api, sndtex_data* data) {
  auto args = api->get_arguments(api);
  for (int i = 0; i < args.length; ++i) {
    switch (args.data[i].name) {
    case GFX_RB_ARG_NAME:
      assert(args.data[i].type == HANS_STRING);
      data->name = args.data[i].string;
      break;
    }
  }
}

static void sndtex_new(hans_constructor_api* api, void* buffer, size_t size) {
  auto data = static_cast<sndtex_data*>(buffer);
  sndtex_parse_args(api, data);
  uint8_t num_outlets = 1;
  api->request_resource(api, HANS_OUTLET, &num_outlets);
}

static void sndtex_setup(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<sndtex_data*>(self->data);
  auto blocksize = api->config->blocksize;

  data->outlet = api->registers->make(self->id, HANS_OUTLET, 0);
  data->samples = new hans_audio_sample[blocksize * GFX_RB_MAX_FRAMES];

  glGenTextures(1, &data->texture);
  api->registers->write(data->outlet, &data->texture);
}

static void sndtex_update(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<sndtex_data*>(self->data);
  auto blocksize = api->config->blocksize;
  auto framesize = blocksize * sizeof(hans_audio_sample);
  auto available = api->ring_buffers->available(data->name);

  if (available >= GFX_RB_MAX_FRAMES) {
    available = GFX_RB_MAX_FRAMES - 1;
  }

  for (auto i = 0; i < available; ++i) {
    auto buffer = api->ring_buffers->read(data->name, i);
    auto dest = &data->samples[i * blocksize];
    std::memcpy(dest, buffer, framesize);
  }

  glBindTexture(GL_TEXTURE_2D, data->texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, 1, blocksize * available, 0, GL_RED,
               GL_FLOAT, data->samples);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}

static void sndtex_init(void* instance) {
  hans_graphics_object* object = static_cast<hans_graphics_object*>(instance);
  object->setup = sndtex_setup;
  object->update = sndtex_update;
  object->draw = nullptr;
}

extern "C" {
void setup(hans_library_api* api) {
  auto size = sizeof(sndtex_data);
  api->register_object(api, "gfx-sndtex", size, sndtex_new, sndtex_init,
                       nullptr);
}
}
