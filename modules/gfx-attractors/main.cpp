#include <cassert>
#include <cmath>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include "hans/engine/object.hpp"

using namespace hans;

#define NUM_PARTICLES 100000
// "attractors/shaders/vert"
#define GFX_SHDRS_VERTEX 0x2852785caca700a4
// "attractors/shaders/frag"
#define GFX_SHDRS_FRAGMENT 0x956f8b548265e345

typedef struct {
  GLuint vao;
  hans_fbo fbo;

  float a;
  float b;
  float c;
  float d;
  float e;
  float f;

  hans_register outlet;
  hans_shader_program_instance program;

  float* positions;
  size_t buffer_length;
  GLuint position_buffer;
  GLuint buffer_length_loc;
} attractors_data;

static void attractors_new(hans_constructor_api* api, void* buffer,
                           size_t size) {
  auto data = static_cast<attractors_data*>(buffer);
  uint8_t num_outlets = 1;
  api->request_resource(api, HANS_OUTLET, &num_outlets);
}

static void attractors_setup(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<attractors_data*>(self->data);

  data->a = 0.08;
  data->b = 1.39;
  data->c = 0.08;
  data->d = 1.03;
  data->e = 1.37;
  data->f = 0.43;

  data->outlet = api->registers->make(self->id, HANS_OUTLET, 0);
  data->fbo = api->fbos->make(self->id);
  data->buffer_length = NUM_PARTICLES * 2;
  data->positions = new float[data->buffer_length];

  auto texture = api->fbos->get_color_attachment(data->fbo, 0);
  api->registers->write(data->outlet, &texture);

  glGenVertexArrays(1, &data->vao);
  glBindVertexArray(data->vao);

  auto ids = new float[NUM_PARTICLES];
  for (auto c = 0; c < NUM_PARTICLES; ++c) {
    ids[c] = c;
  }

  GLuint id_buffer;
  glGenBuffers(1, &id_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, id_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * NUM_PARTICLES, ids,
               GL_STATIC_DRAW);
  delete[] ids;

  auto size = sizeof(float) * data->buffer_length;

  glGenBuffers(1, &data->position_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, data->position_buffer);
  glBufferData(GL_ARRAY_BUFFER, size, nullptr, GL_STREAM_DRAW);

  auto vert_shdr = api->shaders->create_shader(GFX_SHDRS_VERTEX);
  auto frag_shdr = api->shaders->create_shader(GFX_SHDRS_FRAGMENT);
  data->program = api->shaders->create_program(vert_shdr, frag_shdr);
  glUseProgram(data->program.handle);

  auto pos = 0;
  glBindBuffer(GL_ARRAY_BUFFER, id_buffer);
  pos = glGetAttribLocation(data->program.handle, "id");
  glVertexAttribPointer(pos, 1, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  glBindBuffer(GL_ARRAY_BUFFER, data->position_buffer);
  pos = glGetAttribLocation(data->program.handle, "position");
  glVertexAttribPointer(pos, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);
}

static float rand_fract(float scale) {
  auto ran = ((double)rand() / RAND_MAX);
  auto r = (((ran)*2.f) - 1.0) * scale;
  return r;
}

static void attractors_update(hans_graphics_object* self,
                              hans_object_api* api) {
  auto data = static_cast<attractors_data*>(self->data);

  float xs = 0, ys = 0, zs = 0;

  // Based on http://koaning.io/fluctuating-repetition.html
  for (int i = 0; i < data->buffer_length; i += 2) {
    data->positions[i + 0] = xs;
    data->positions[i + 1] = ys;
    xs = (sin(data->a * xs) + sin(data->b * ys)) - cos(data->c * zs);
    ys = (sin(data->d * xs) + sin(data->e * ys)) - cos(data->f * zs);
    zs = zs + 0.1;
  }

  data->a = data->a + rand_fract(0.05);
  data->b = data->b + rand_fract(0.05);
  data->c = data->c + rand_fract(0.05);
  data->d = data->d + rand_fract(0.05);
  data->e = data->e + rand_fract(0.05);
  data->f = data->f + rand_fract(0.05);

  glBindBuffer(GL_ARRAY_BUFFER, data->position_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(float) * data->buffer_length,
                  data->positions);
}

static void attractors_draw(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<attractors_data*>(self->data);

  glUseProgram(data->program.handle);

  api->fbos->bind_fbo(data->fbo);

  glEnable(GL_ALPHA_TEST);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_POINT_SMOOTH);
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(data->vao);
  glPointSize(1);
  glDrawArrays(GL_POINTS, 0, data->buffer_length);
}

static void attractors_init(void* instance) {
  hans_graphics_object* object = static_cast<hans_graphics_object*>(instance);
  object->setup = attractors_setup;
  object->update = attractors_update;
  object->draw = attractors_draw;
}

extern "C" {
void setup(hans_library_api* api) {
  api->register_object(api, "gfx-attractors", sizeof(attractors_data),
                       attractors_new, attractors_init, nullptr);
}
}
