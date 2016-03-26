#include "hans/engine/object.hpp"
#include "./gfx.quad_generated.h"
#include <iostream>
#include <cassert>

using namespace hans;

static const float VERTICES[] = {-1, 1, -1, -1, 1, -1, 1, 1};
static const int INDEX[] = {0, 1, 2, 2, 3, 0};

typedef struct {
  hans_fbo_handle fbo;
  GLuint vao;
  GLuint texture;

  hans_shader_instance v_shader;
  hans_shader_instance f_shader;
  hans_shader_program_instance program;

  hans_register_handle inlet_texture;
  hans_register_handle outlet_frame;

  uint32_t texture_value;
  bool is_empty;
} QuadData;

void get_resources(QuadData* data, hans_object_resource* resources, int len) {
  int seen_shaders = 0;

  for (int i = 0; i < len; ++i) {
    auto resource = &resources[i];
    switch (resource->type) {
    case HANS_INLET:
      data->inlet_texture = resource->inlet;
      break;

    // TODO: pass the texture to outlet
    case HANS_OUTLET:
      break;

    case HANS_FRAME_BUFFER:
      data->fbo = resource->frame_buffer;
      break;

    case HANS_SHADER:
      switch (seen_shaders) {
      case 0:
        data->v_shader = resource->shader;
        break;
      case 1:
        data->f_shader = resource->shader;
        break;
      }
      seen_shaders++;
      break;

    default:
      assert(false && "Unhandled resource");
      break;
    }
  }
}

void quad_setup(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<QuadData*>(self->data);
  get_resources(data, self->resources, self->num_resources);

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

  api->shaders->create_shader(data->v_shader, LIBQUAD_VERT_SHADER);
  api->shaders->create_shader(data->f_shader, LIBQUAD_FRAG_SHADER);

  data->program = api->shaders->create_program(data->v_shader, data->f_shader);
  glUseProgram(data->program.handle);
  data->texture = glGetUniformLocation(data->program.handle, "u_texture");

  GLint pos_attrib = glGetAttribLocation(data->program.handle, "position");
  glVertexAttribPointer(pos_attrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);

  data->is_empty = api->registers->is_empty(data->inlet_texture);

  if (data->is_empty == false) {
    auto register_value = api->registers->get_read_reg(data->inlet_texture);
    assert(register_value != nullptr);
    data->texture_value = *static_cast<uint32_t*>(register_value);
  }
}

void quad_draw(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<QuadData*>(self->data);

  glUseProgram(data->program.handle);

  if (data->is_empty == false) {
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, data->texture_value);
    glUniform1i(data->texture, 0);
  }

  // XXX: Make this optional
  api->frame_buffers->release_frame_buffer();
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glBindVertexArray(data->vao);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

void quad_new(hans_constructor_api* api, void* buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 1);
  api->request_resource(api, HANS_OUTLET, 1);
  api->request_resource(api, HANS_SHADER, 2);

  hans_graphics_object* object = static_cast<hans_graphics_object*>(buffer);
  object->setup = quad_setup;
  object->update = nullptr;
  object->draw = quad_draw;

  void* offset = static_cast<char*>(buffer) + sizeof(hans_graphics_object);
  object->data = static_cast<QuadData*>(offset);
}

extern "C" {
void setup(hans_library_api* api) {
  auto size = sizeof(hans_graphics_object) + sizeof(QuadData);
  api->register_object(api, "gfx.quad", size, quad_new, nullptr);
}
}
