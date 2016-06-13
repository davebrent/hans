#include <cassert>
#include <iostream>
#include "hans/engine/object.hpp"

using namespace hans;

#define MAX_FRAMES 30
// "left"
#define ARG_LEFT 0xe7add5f891f566b
// "right"
#define ARG_RIGHT 0xc6ef9eb8bc8afb11
// "scopes/shaders/oscilloscope"
#define GFX_SHDRS_OSCILLOSCOPE 0x84f56ca0bb2363c8
// "scopes/shaders/phasescope"
#define GFX_SHDRS_PHASESCOPE 0xe1a6fd5f260d183c
// "scopes/shaders/fragment"
#define GFX_SHDRS_FRAGMENT 0x6f9314e2cc69a7e6

typedef struct {
  GLuint vao;
  hans_fbo fbo;

  hans_hash vertex_shader_name;

  hans_hash left;
  hans_hash right;
  hans_register outlet;
  hans_shader_program_instance program;
  hans_audio_sample* samples;
  float buffer_length;
  GLuint audio_buffer_object;
  GLuint buffer_length_loc;
} scope_data;

static void scope_parse_args(hans_constructor_api* api, scope_data* data) {
  auto args = api->get_arguments(api);
  for (int i = 0; i < args.length; ++i) {
    switch (args.data[i].name) {
    case ARG_LEFT:
      assert(args.data[i].type == HANS_STRING);
      data->left = args.data[i].string;
      break;
    case ARG_RIGHT:
      assert(args.data[i].type == HANS_STRING);
      data->right = args.data[i].string;
      break;
    }
  }
}

static void osc_scope_new(hans_constructor_api* api, void* buffer,
                          size_t size) {
  auto data = static_cast<scope_data*>(buffer);

  data->left = 0;
  data->right = 0;
  data->buffer_length = 0;
  data->vertex_shader_name = GFX_SHDRS_OSCILLOSCOPE;

  scope_parse_args(api, data);

  uint8_t num_outlets = 1;
  api->request_resource(api, HANS_OUTLET, &num_outlets);
}

static void osc_scope_setup(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<scope_data*>(self->data);
  auto blocksize = api->config->blocksize;
  auto channels = 2;
  auto max_channel_samples = blocksize * MAX_FRAMES;
  auto max_points = max_channel_samples * channels;

  data->outlet = api->registers->make(self->id, HANS_OUTLET, 0);
  data->fbo = api->fbos->make(self->id);
  data->samples = new hans_audio_sample[max_points];
  data->buffer_length = max_points;

  auto texture = api->fbos->get_color_attachment(data->fbo, 0);
  api->registers->write(data->outlet, &texture);

  glGenVertexArrays(1, &data->vao);
  glBindVertexArray(data->vao);

  auto sample_index = new float[max_points];
  for (auto t = 0; t < max_points; ++t) {
    sample_index[t] = t % max_channel_samples;
  }

  auto size = sizeof(float) * max_points;

  GLuint vbo;
  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, size, sample_index, GL_STATIC_DRAW);
  delete[] sample_index;

  auto channel_index = new float[max_points];
  for (auto c = 0; c < channels; ++c) {
    auto offset = c * max_channel_samples;
    for (auto t = 0; t < max_channel_samples; ++t) {
      channel_index[offset + t] = t;
    }
  }

  GLuint channel_buffer_object;
  glGenBuffers(1, &channel_buffer_object);
  glBindBuffer(GL_ARRAY_BUFFER, channel_buffer_object);
  glBufferData(GL_ARRAY_BUFFER, size, channel_index, GL_STATIC_DRAW);
  delete[] channel_index;

  glGenBuffers(1, &data->audio_buffer_object);
  glBindBuffer(GL_ARRAY_BUFFER, data->audio_buffer_object);
  glBufferData(GL_ARRAY_BUFFER, size, nullptr, GL_STREAM_DRAW);

  auto vert_shdr = api->shaders->create_shader(data->vertex_shader_name);
  auto frag_shdr = api->shaders->create_shader(GFX_SHDRS_FRAGMENT);
  data->program = api->shaders->create_program(vert_shdr, frag_shdr);
  glUseProgram(data->program.handle);

  auto pos = 0;
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  pos = glGetAttribLocation(data->program.handle, "index");
  glVertexAttribPointer(pos, 1, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  glBindBuffer(GL_ARRAY_BUFFER, channel_buffer_object);
  pos = glGetAttribLocation(data->program.handle, "channel");
  glVertexAttribPointer(pos, 1, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  glBindBuffer(GL_ARRAY_BUFFER, data->audio_buffer_object);
  pos = glGetAttribLocation(data->program.handle, "sample");
  glVertexAttribPointer(pos, 1, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  data->buffer_length_loc =
      glGetUniformLocation(data->program.handle, "audio_buffer_length");
}

static uint8_t osc_read_ring_buffer(scope_data* data, hans_object_api* api,
                                    hans_hash rb_name, size_t offset) {
  auto blocksize = api->config->blocksize;
  auto framesize = blocksize * sizeof(hans_audio_sample);
  auto available = api->ring_buffers->available(rb_name);

  if (available >= MAX_FRAMES) {
    available = MAX_FRAMES - 1;
  }

  for (auto i = 0; i < available; ++i) {
    auto buffer = api->ring_buffers->read(rb_name, i);
    auto dest = &data->samples[(i * blocksize) + offset];
    std::memcpy(dest, buffer, framesize);
  }

  return available;
}

static void osc_scope_update(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<scope_data*>(self->data);
  auto blocksize = api->config->blocksize;

  auto read = 0;

  read += osc_read_ring_buffer(data, api, data->right, 0);
  read += osc_read_ring_buffer(data, api, data->left, read * blocksize);

  data->buffer_length = read * blocksize;

  auto size = sizeof(hans_audio_sample) * data->buffer_length;
  glBindBuffer(GL_ARRAY_BUFFER, data->audio_buffer_object);
  glBufferSubData(GL_ARRAY_BUFFER, 0, size, data->samples);
}

static void osc_scope_draw(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<scope_data*>(self->data);

  glUseProgram(data->program.handle);
  glUniform1f(data->buffer_length_loc, data->buffer_length);

  api->fbos->bind_fbo(data->fbo);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(data->vao);
  glPointSize(3);
  glDrawArrays(GL_POINTS, 0, data->buffer_length);
}

static void osc_scope_init(void* instance) {
  hans_graphics_object* object = static_cast<hans_graphics_object*>(instance);
  object->setup = osc_scope_setup;
  object->update = osc_scope_update;
  object->draw = osc_scope_draw;
}

// -----------------------------------------------------------------------------

static void phase_scope_new(hans_constructor_api* api, void* buffer,
                            size_t size) {
  auto data = static_cast<scope_data*>(buffer);

  data->left = 0;
  data->right = 0;
  data->buffer_length = 0;
  data->vertex_shader_name = GFX_SHDRS_PHASESCOPE;

  scope_parse_args(api, data);

  uint8_t num_outlets = 1;
  api->request_resource(api, HANS_OUTLET, &num_outlets);
}

static void phase_scope_setup(hans_graphics_object* self,
                              hans_object_api* api) {
  auto data = static_cast<scope_data*>(self->data);
  auto blocksize = api->config->blocksize;
  auto channels = 2;

  auto max_points = blocksize * MAX_FRAMES * channels;

  data->outlet = api->registers->make(self->id, HANS_OUTLET, 0);
  data->fbo = api->fbos->make(self->id);
  data->samples = new hans_audio_sample[max_points];
  data->buffer_length = max_points / channels;

  auto texture = api->fbos->get_color_attachment(data->fbo, 0);
  api->registers->write(data->outlet, &texture);

  glGenVertexArrays(1, &data->vao);
  glBindVertexArray(data->vao);

  auto size = sizeof(float) * max_points;
  glGenBuffers(1, &data->audio_buffer_object);
  glBindBuffer(GL_ARRAY_BUFFER, data->audio_buffer_object);
  glBufferData(GL_ARRAY_BUFFER, size, nullptr, GL_STREAM_DRAW);

  auto vert_shdr = api->shaders->create_shader(data->vertex_shader_name);
  auto frag_shdr = api->shaders->create_shader(GFX_SHDRS_FRAGMENT);
  data->program = api->shaders->create_program(vert_shdr, frag_shdr);
  glUseProgram(data->program.handle);

  auto pos = 0;
  glBindBuffer(GL_ARRAY_BUFFER, data->audio_buffer_object);
  pos = glGetAttribLocation(data->program.handle, "sample");
  glVertexAttribPointer(pos, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  data->buffer_length_loc =
      glGetUniformLocation(data->program.handle, "audio_buffer_length");
}

static void phase_scope_update(hans_graphics_object* self,
                               hans_object_api* api) {
  auto data = static_cast<scope_data*>(self->data);
  auto blocksize = api->config->blocksize;
  auto available = api->ring_buffers->available(data->left);

  if (available >= MAX_FRAMES) {
    available = MAX_FRAMES - 1;
  }

  for (auto i = 0; i < available; ++i) {
    auto left = api->ring_buffers->read(data->left, i);
    auto right = api->ring_buffers->read(data->right, i);
    auto block = (i * blocksize);
    auto j = 0;

    for (auto t = 0; t < blocksize; ++t) {
      data->samples[block + j] = left[t];
      data->samples[block + j + 1] = right[t + 1];
      j += 2;
    }
  }

  data->buffer_length = available * blocksize;

  auto size = sizeof(hans_audio_sample) * (available + available) * blocksize;
  glBindBuffer(GL_ARRAY_BUFFER, data->audio_buffer_object);
  glBufferSubData(GL_ARRAY_BUFFER, 0, size, data->samples);
}

static void phase_scope_draw(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<scope_data*>(self->data);

  glUseProgram(data->program.handle);
  glUniform1f(data->buffer_length_loc, data->buffer_length);

  api->fbos->bind_fbo(data->fbo);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(data->vao);
  glPointSize(3);
  glDrawArrays(GL_POINTS, 0, data->buffer_length);
}

static void phase_scope_init(void* instance) {
  hans_graphics_object* object = static_cast<hans_graphics_object*>(instance);
  object->setup = phase_scope_setup;
  object->update = phase_scope_update;
  object->draw = phase_scope_draw;
}

extern "C" {
void setup(hans_library_api* api) {
  api->register_object(api, "gfx-oscilloscope", sizeof(scope_data),
                       osc_scope_new, osc_scope_init, nullptr);
  api->register_object(api, "gfx-phasescope", sizeof(scope_data),
                       phase_scope_new, phase_scope_init, nullptr);
}
}
