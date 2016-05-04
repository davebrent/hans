#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/mat4x4.hpp>
#include <glm/vec3.hpp>
#include <iostream>
#include "./gfx.superformula_generated.h"
#include "hans/engine/object.hpp"

using namespace hans;

class SuperFormulaGeometry {
 public:
  int vertices_bytes;
  int indices_bytes;
  int *indices;
  float *vertices;

  SuperFormulaGeometry(int segments) {
    int num_vertices = ((segments + 1) * (segments + 1)) * 3;
    int num_indices = (segments * segments) * 6;

    vertices_bytes = num_vertices * sizeof(float);
    indices_bytes = num_indices * sizeof(int);

    vertices = new float[num_vertices];
    indices = new int[num_indices];

    int v1, v2, v3, v4, y, x, i = 0;

    for (y = 0; y <= segments; y++) {
      for (x = 0; x <= segments; x++) {
        vertices[i] = x;
        vertices[i + 1] = y;
        vertices[i + 2] = 0;
        i += 3;
      }
    }

    i = 0;

    for (y = 0; y < segments; y++) {
      for (x = 0; x < segments; x++) {
        v1 = (y * (segments + 1)) + x;
        v2 = v1 + segments + 1;
        v3 = v1 + 1;
        v4 = v2 + 1;

        indices[i] = v1;
        indices[i + 1] = v2;
        indices[i + 2] = v3;

        indices[i + 3] = v2;
        indices[i + 4] = v4;
        indices[i + 5] = v3;
        i += 6;
      }
    }
  }

  ~SuperFormulaGeometry() {
    delete[] vertices;
    delete[] indices;
  }
};

typedef struct {
  int size;
  hans_hash name;
  hans_parameter_handle parameter;
  GLuint uniform;
} UniformParameter;

typedef struct {
  int segments;
  float rotation;

  hans_fbo_handle fbo;

  GLuint shape_vao;
  GLuint model_view_matrix;
  GLuint proj_matrix;

  hans_shader_program_instance program;

  hans_register_handle outlet_colors;
  hans_register_handle outlet_normals;
  hans_register_handle outlet_depth;

  hans_parameter_handle translation;
  hans_parameter_handle rotation_axis;
  hans_parameter_handle rotation_speed;
  hans_parameter_handle draw_mode;

  std::vector<UniformParameter> uniforms;
} SuperFormula;

static void default_gl_state() {
  glPointSize(0.8);
  glLineWidth(0.5);

  glEnable(GL_CULL_FACE);
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);

  glEnable(GL_ALPHA_TEST);

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

  glEnable(GL_LINE_SMOOTH);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);

  glEnable(GL_POINT_SMOOTH);
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);

  glHint(GL_POLYGON_SMOOTH, GL_NICEST);
  glEnable(GL_POLYGON_SMOOTH);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

static GLenum to_draw_mode(int mode) {
  switch (mode) {
  case 1:
    return GL_POINTS;
  case 2:
    return GL_LINE_STRIP;
  case 3:
    return GL_LINE_LOOP;
  case 4:
    return GL_LINES;
  case 5:
    return GL_LINE_STRIP_ADJACENCY;
  case 6:
    return GL_LINES_ADJACENCY;
  case 7:
    return GL_TRIANGLE_STRIP;
  case 8:
    return GL_TRIANGLE_FAN;
  default:
    return GL_TRIANGLES;
  }
}

static void update_uniforms(hans_object_api *api,
                            const std::vector<UniformParameter> &uniforms) {
  for (const UniformParameter &p : uniforms) {
    switch (p.size) {
    case 1:
      glUniform1f(p.uniform, api->parameters->get(p.parameter, 0));
      break;
    case 2:
      glUniform2f(p.uniform, api->parameters->get(p.parameter, 0),
                  api->parameters->get(p.parameter, 1));
      break;
    case 3:
      glUniform3f(p.uniform, api->parameters->get(p.parameter, 0),
                  api->parameters->get(p.parameter, 1),
                  api->parameters->get(p.parameter, 2));
      break;
    }
  }
}

void superformula_setup(hans_graphics_object *self, hans_object_api *api) {
  auto data = static_cast<SuperFormula *>(self->data);

  data->segments = 90;
  data->rotation = 0;

#define MAKE_UNIFORM_PARAM(SIZE, NAME) \
  data->uniforms.push_back({.size = SIZE, .name = api->strings->intern(NAME)});

  data->uniforms.reserve(13);
  MAKE_UNIFORM_PARAM(2, "superformula_m");
  MAKE_UNIFORM_PARAM(2, "superformula_n1");
  MAKE_UNIFORM_PARAM(2, "superformula_n2");
  MAKE_UNIFORM_PARAM(2, "superformula_n3");
  MAKE_UNIFORM_PARAM(2, "superformula_a");
  MAKE_UNIFORM_PARAM(2, "superformula_b");
  MAKE_UNIFORM_PARAM(2, "superformula_u");
  MAKE_UNIFORM_PARAM(2, "superformula_v");
  MAKE_UNIFORM_PARAM(1, "superformula_scale");
  MAKE_UNIFORM_PARAM(1, "superformula_segments");
  MAKE_UNIFORM_PARAM(1, "superformula_seed");
  MAKE_UNIFORM_PARAM(1, "superformula_deform");
  MAKE_UNIFORM_PARAM(3, "superformula_color");

#undef MAKE_UNIFORM_PARAM

  int seen_shaders = 0;
  int seen_outlets = 0;

  hans_shader_instance vert_shader;
  hans_shader_instance frag_shader;

  for (int i = 0; i < self->num_resources; ++i) {
    auto resource = &self->resources[i];

    switch (resource->type) {
    case HANS_OUTLET:
      switch (seen_outlets) {
      case 0:
        data->outlet_colors = resource->outlet;
        break;
      case 1:
        data->outlet_normals = resource->outlet;
        break;
      case 2:
        data->outlet_depth = resource->outlet;
        break;
      }
      seen_outlets++;
      break;

    case HANS_FRAME_BUFFER:
      data->fbo = resource->frame_buffer;
      break;

    case HANS_SHADER:
      switch (seen_shaders) {
      case 0:
        vert_shader = resource->shader;
        break;
      case 1:
        frag_shader = resource->shader;
        break;
      }
      seen_shaders++;
      break;

    case HANS_PARAMETER: {
      switch (resource->name) {
      case LIBSUPERFORUMLA_PARAM_ROTATION_SPEED:
        data->rotation_speed = resource->parameter;
        break;
      case LIBSUPERFORUMLA_PARAM_ROTATION_AXIS:
        data->rotation_axis = resource->parameter;
        break;
      case LIBSUPERFORUMLA_PARAM_TRANSLATE:
        data->translation = resource->parameter;
        break;
      case LIBSUPERFORUMLA_PARAM_DRAW_MODE:
        data->draw_mode = resource->parameter;
        break;
      default:
        for (auto &uniform : data->uniforms) {
          if (uniform.name == resource->name) {
            uniform.parameter = resource->parameter;
            break;
          }
        }
        break;
      }
      break;
    }

    default:
      assert(false && "Resource not handled");
      break;
    }
  }

  // Setup buffers
  SuperFormulaGeometry geometry(data->segments);

  GLuint shape_vbo;
  GLuint shape_ebo;

  glGenVertexArrays(1, &data->shape_vao);
  glBindVertexArray(data->shape_vao);

  glGenBuffers(1, &shape_vbo);
  glBindBuffer(GL_ARRAY_BUFFER, shape_vbo);
  glBufferData(GL_ARRAY_BUFFER, geometry.vertices_bytes, geometry.vertices,
               GL_STATIC_DRAW);

  glGenBuffers(1, &shape_ebo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, shape_ebo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, geometry.indices_bytes,
               geometry.indices, GL_STATIC_DRAW);

  // Setup shaders
  api->shaders->create_shader(vert_shader, LIBSUPERFORUMLA_VERT_SHADER);
  api->shaders->create_shader(frag_shader, LIBSUPERFORUMLA_FRAG_SHADER);
  data->program = api->shaders->create_program(vert_shader, frag_shader);
  glUseProgram(data->program.handle);

  // Setup attributes and frag locations
  GLint pos_attrib = glGetAttribLocation(data->program.handle, "position");
  glVertexAttribPointer(pos_attrib, 3, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);

  // Uniforms
  data->model_view_matrix =
      glGetUniformLocation(data->program.handle, "model_view_matrix");
  data->proj_matrix =
      glGetUniformLocation(data->program.handle, "projection_matrix");
  for (UniformParameter &uparam : data->uniforms) {
    const char *name = api->strings->lookup(uparam.name);
    uparam.uniform = glGetUniformLocation(data->program.handle, name);
  }

  default_gl_state();

  // Send the textures we will be writing to to the output registers
  auto color_tex = api->frame_buffers->get_color_attachment(data->fbo, 0);
  auto normal_tex = api->frame_buffers->get_color_attachment(data->fbo, 1);
  auto depth_tex = api->frame_buffers->get_depth_attachment(data->fbo);

  api->registers->set_write_reg(data->outlet_colors, &color_tex);
  api->registers->set_write_reg(data->outlet_normals, &normal_tex);
  api->registers->set_write_reg(data->outlet_depth, &depth_tex);
}

void superformula_draw(hans_graphics_object *self, hans_object_api *api) {
  auto data = static_cast<SuperFormula *>(self->data);
  glm::vec3 translation = glm::vec3(api->parameters->get(data->translation, 0),
                                    api->parameters->get(data->translation, 1),
                                    api->parameters->get(data->translation, 2));

  glm::vec3 axis = glm::vec3(api->parameters->get(data->rotation_axis, 0),
                             api->parameters->get(data->rotation_axis, 1),
                             api->parameters->get(data->rotation_axis, 2));

  glm::mat4 model_view_matrix = glm::mat4();
  glm::mat4 projection_matrix =
      glm::perspective(45.0f, 1184.0f / 640.0f, 0.1f, 100.f);
  model_view_matrix = glm::translate(model_view_matrix, translation);
  model_view_matrix = glm::rotate(model_view_matrix, data->rotation, axis);

  data->rotation += api->parameters->get(data->rotation_speed, 0);

  glUseProgram(data->program.handle);
  glUniformMatrix4fv(data->model_view_matrix, 1, 0,
                     glm::value_ptr(model_view_matrix));
  glUniformMatrix4fv(data->proj_matrix, 1, 0,
                     glm::value_ptr(projection_matrix));
  update_uniforms(api, data->uniforms);

  GLenum mode = to_draw_mode(std::min<float>(
      std::max<float>(api->parameters->get(data->draw_mode, 0), 0), 10));

  api->frame_buffers->bind_frame_buffer(data->fbo);
  glClearColor(0.2, 0.2, 0.2, 1);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glBindVertexArray(data->shape_vao);
  glDrawElements(mode, (data->segments + 1) * (data->segments + 1) * 6,
                 GL_UNSIGNED_INT, 0);
}

void superformula_new(hans_constructor_api *api, void *buffer, size_t size) {
  api->request_resource(api, HANS_INLET, 0);
  api->request_resource(api, HANS_OUTLET, 3);
  api->request_resource(api, HANS_SHADER, 4);

  hans_graphics_object *object = static_cast<hans_graphics_object *>(buffer);
  object->setup = superformula_setup;
  object->update = nullptr;
  object->draw = superformula_draw;

  void *offset = static_cast<char *>(buffer) + sizeof(hans_graphics_object);
  object->data = static_cast<SuperFormula *>(offset);
}

extern "C" {
void setup(hans_library_api *api) {
  api->register_object(api, "gfx.superformula",
                       sizeof(hans_graphics_object) + sizeof(SuperFormula),
                       superformula_new, nullptr);
}
}
