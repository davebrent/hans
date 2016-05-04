#include <cassert>
#include <iostream>
#include "./gfx.filter_generated.h"
#include "hans/engine/object.hpp"

using namespace hans;

static const float VERTICES[] = {-1, 1, -1, -1, 1, -1, 1, 1};
static const int INDEX[] = {0, 1, 2, 2, 3, 0};

typedef struct {
  hans_fbo_handle fbo;
  GLuint vao;
  GLuint texture;

  hans_hash shader;
  hans_shader_instance v_shader;
  hans_shader_instance f_shader;
  hans_shader_program_instance program;

  hans_register_handle inlet_texture;
  hans_register_handle outlet_texture;
  hans_parameter_handle amount;

  GLuint u_center_loc;
  GLuint u_resolution_loc;
  GLuint u_amount_loc;

  uint32_t texture_value;
  bool is_empty;
} FilterData;

void get_resources(FilterData* data, hans_object_resource* resources, int len) {
  int seen_shaders = 0;

  for (int i = 0; i < len; ++i) {
    auto resource = &resources[i];
    switch (resource->type) {
    case HANS_INLET:
      data->inlet_texture = resource->inlet;
      break;

    case HANS_OUTLET:
      data->outlet_texture = resource->outlet;
      break;

    case HANS_FRAME_BUFFER:
      data->fbo = resource->frame_buffer;
      break;

    case HANS_PARAMETER:
      data->amount = resource->parameter;
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

void filter_setup(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<FilterData*>(self->data);
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

  api->shaders->create_shader(data->v_shader, LIBHANS_FILTER_VERT_SHADER);
  api->shaders->create_shader(data->f_shader, data->shader);

  data->program = api->shaders->create_program(data->v_shader, data->f_shader);
  glUseProgram(data->program.handle);
  data->u_resolution_loc =
      glGetUniformLocation(data->program.handle, "u_resolution");
  data->u_center_loc = glGetUniformLocation(data->program.handle, "u_center");

  data->texture = glGetUniformLocation(data->program.handle, "u_texture");
  data->u_amount_loc = glGetUniformLocation(data->program.handle, "u_amount");

  GLint pos_attrib = glGetAttribLocation(data->program.handle, "position");
  glVertexAttribPointer(pos_attrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);

  // Read in an input texture
  data->is_empty = api->registers->is_empty(data->inlet_texture);
  if (!data->is_empty) {
    auto register_value = api->registers->get_read_reg(data->inlet_texture);
    assert(register_value != nullptr);
    data->texture_value = *static_cast<uint32_t*>(register_value);
  }

  // Send the textures we will be writing to to the outlet
  auto out_tex = api->frame_buffers->get_color_attachment(data->fbo, 0);
  api->registers->set_write_reg(data->outlet_texture, &out_tex);
}

void filter_draw(hans_graphics_object* self, hans_object_api* api) {
  auto data = static_cast<FilterData*>(self->data);
  glUseProgram(data->program.handle);

  // Bind input texture
  if (data->is_empty == false) {
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, data->texture_value);
    glUniform1i(data->texture, 0);
  }

  int input_width;
  int input_height;

  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &input_width);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &input_height);

  glUniform2f(data->u_resolution_loc, input_width, input_height);
  glUniform2f(data->u_center_loc, input_width / 2.f, input_height / 2.f);
  glUniform1f(data->u_amount_loc, api->parameters->get(data->amount, 0));

  api->frame_buffers->bind_frame_buffer(data->fbo);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glBindVertexArray(data->vao);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

void filter_new(hans_constructor_api* api, void* buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 1);
  api->request_resource(api, HANS_OUTLET, 1);
  api->request_resource(api, HANS_SHADER, 2);

  hans_graphics_object* object = static_cast<hans_graphics_object*>(buffer);
  object->setup = filter_setup;
  object->update = nullptr;
  object->draw = filter_draw;

  void* offset = static_cast<char*>(buffer) + sizeof(hans_graphics_object);
  object->data = static_cast<FilterData*>(offset);

  auto data = static_cast<FilterData*>(object->data);
  // Parse arguments
  bool found = false;
  auto args = api->get_args(api);
  for (int i = 0; i < args.length; ++i) {
    if (args.data[i].type == HANS_STRING &&
        args.data[i].name == LIBHANS_FILTER_ARG_NAME) {
      data->shader = args.data[i].string;
      found = true;
    }
  }

  if (!found) {
    data->shader = LIBHANS_FILTER_PASSTHROUGH_SHADER;
  }
}

extern "C" {
void setup(hans_library_api* api) {
  auto size = sizeof(hans_graphics_object) + sizeof(FilterData);
  api->register_object(api, "gfx.filter", size, filter_new, nullptr);
}
}
