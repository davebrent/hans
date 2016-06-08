#include <cassert>
#include <iostream>
#include "./gfx.quad_generated.h"
#include "hans/engine/object.hpp"

using namespace hans;

static const float VERTICES[] = {-1, 1, -1, -1, 1, -1, 1, 1};
static const int INDEX[] = {0, 1, 2, 2, 3, 0};

typedef struct {
  hans_fbo fbo;
  GLuint vao;
  GLuint texture;
  hans_shader_instance v_shader;
  hans_shader_instance f_shader;
  hans_shader_program_instance program;
  hans_register inlet;
  uint32_t texture_value;
} QuadData;

void quad_setup(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<QuadData*>(self->data);

  data->fbo = api->fbos->make(self->id);
  data->inlet = api->registers->make(self->id, HANS_INLET, 0);

  GLuint vbo;
  GLuint ebo;

  glGenVertexArrays(1, &data->vao);
  glBindVertexArray(data->vao);

  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(VERTICES), VERTICES, GL_STATIC_DRAW);

  glGenBuffers(1, &ebo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(INDEX), INDEX, GL_STATIC_DRAW);

  glBindVertexArray(data->vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);

  data->v_shader = api->shaders->create_shader(LIBQUAD_VERT_SHADER);
  data->f_shader = api->shaders->create_shader(LIBQUAD_FRAG_SHADER);
  data->program = api->shaders->create_program(data->v_shader, data->f_shader);
  glUseProgram(data->program.handle);

  data->texture = glGetUniformLocation(data->program.handle, "u_texture");

  GLint pos_attrib = glGetAttribLocation(data->program.handle, "position");
  glVertexAttribPointer(pos_attrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);

  auto register_value = api->registers->read(data->inlet);
  data->texture_value = *static_cast<uint32_t*>(register_value);
}

void quad_draw(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<QuadData*>(self->data);

  glUseProgram(data->program.handle);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, data->texture_value);
  glUniform1i(data->texture, 0);

  api->fbos->release_fbo();
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(data->vao);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

void quad_new(hans_constructor_api* api, void* buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 1);
  api->request_resource(api, HANS_SHADER, 2);
}

void quad_init(void* instance) {
  hans_graphics_object* object = static_cast<hans_graphics_object*>(instance);
  object->setup = quad_setup;
  object->update = nullptr;
  object->draw = quad_draw;
}

extern "C" {
void setup(hans_library_api* api) {
  auto size = sizeof(QuadData);
  api->register_object(api, "gfx-quad", size, quad_new, quad_init, nullptr);
}
}
