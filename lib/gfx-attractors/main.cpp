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
// "a", "b", "c", "d", "e", "f"
#define PARAM_NAME_A 0x71717d2d36b6b11
#define PARAM_NAME_B 0xea8bfc7d922a2a37
#define PARAM_NAME_C 0xd8788d18ba82e0b5
#define PARAM_NAME_D 0x17dffbc5a8f17839
#define PARAM_NAME_E 0x7115e430772d6a11
#define PARAM_NAME_F 0x5647bce94f90d9aa

typedef struct {
  GLuint vao;
  hans_fbo fbo;

  hans_parameter a;
  hans_parameter b;
  hans_parameter c;
  hans_parameter d;
  hans_parameter e;
  hans_parameter f;

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

  data->a = api->parameters->make(self->id, PARAM_NAME_A);
  data->b = api->parameters->make(self->id, PARAM_NAME_B);
  data->c = api->parameters->make(self->id, PARAM_NAME_C);
  data->d = api->parameters->make(self->id, PARAM_NAME_D);
  data->e = api->parameters->make(self->id, PARAM_NAME_E);
  data->f = api->parameters->make(self->id, PARAM_NAME_F);

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

static void attractors_update(hans_graphics_object* self,
                              hans_object_api* api) {
  auto data = static_cast<attractors_data*>(self->data);

  float a = api->parameters->get(data->a, 0);
  float b = api->parameters->get(data->b, 0);
  float c = api->parameters->get(data->c, 0);
  float d = api->parameters->get(data->d, 0);
  float e = api->parameters->get(data->e, 0);
  float f = api->parameters->get(data->f, 0);

  float xs = 0, ys = 0, zs = 0;

  // Based on http://koaning.io/fluctuating-repetition.html
  for (int i = 0; i < data->buffer_length; i += 2) {
    data->positions[i + 0] = xs;
    data->positions[i + 1] = ys;
    xs = (sin(a * xs) + sin(b * ys)) - cos(c * zs);
    ys = (sin(d * xs) + sin(e * ys)) - cos(f * zs);
    zs = zs + 0.1;
  }

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
