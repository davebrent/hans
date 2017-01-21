#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/mat4x4.hpp>
#include <glm/vec3.hpp>
#include "hans/engine/object.hpp"

#define PARAM_ROTATION 0x2060566242789baa      /* rotation */
#define PARAM_ROTATION_AXIS 0x975878516616e3a4 /* rotation_axis */
#define PARAM_TRANSLATE 0xff8b6e0cf15e6b25     /* translate */
#define PARAM_DRAW_MODE 0x25c9c1a36cc45020     /* draw_mode */
#define FRAG_SHADER 0x79b67a11a5e1e481 /* superformula/shader/fragment */
#define VERT_SHADER 0x62fd67147cf1e377 /* superformula/shader/vertex */

using namespace hans;
using namespace hans::engine;

class SuperFormulaGeometry {
 public:
  int vertices_bytes;
  int indices_bytes;
  int* indices;
  float* vertices;

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

struct UniformParameter {
  int size;
  hash name;
  Parameter parameter;
  GLuint uniform;
};

static void update_uniforms(Engine& engine,
                            const std::vector<UniformParameter>& uniforms) {
  for (const UniformParameter& p : uniforms) {
    switch (p.size) {
    case 1:
      glUniform1f(p.uniform, engine.parameters.get(p.parameter, 0));
      break;
    case 2:
      glUniform2f(p.uniform, engine.parameters.get(p.parameter, 0),
                  engine.parameters.get(p.parameter, 1));
      break;
    case 3:
      glUniform3f(p.uniform, engine.parameters.get(p.parameter, 0),
                  engine.parameters.get(p.parameter, 1),
                  engine.parameters.get(p.parameter, 2));
      break;
    }
  }
}

struct FormulaState {
  int segments;
  graphics::FBO fbo;
  GLuint shape_vao;
  GLuint model_view_matrix;
  GLuint proj_matrix;
  graphics::ShaderProgram program;
  Register outlet_colors;
  Register outlet_normals;
  Register outlet_depth;
  Parameter translation;
  Parameter rotation_axis;
  Parameter rotation;
  Parameter draw_mode;
  std::vector<UniformParameter> uniforms;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(segments);
  }
};

class FormulaObject : protected GraphicsObject {
  friend class hans::engine::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void update(Engine& engine) override {
  }
  virtual void draw(Engine& engine) const override;

 private:
  FormulaState state;
};

void FormulaObject::create(IPatcher& patcher) {
  patcher.request(IPatcher::Resources::OUTLET, 3);
}

void FormulaObject::setup(Engine& engine) {
  state.segments = 90;

#define MAKE_UNIFORM_PARAM(SIZE, NAME) \
  state.uniforms.push_back({.size = SIZE, .name = engine.strings.intern(NAME)});

  state.uniforms.reserve(13);
  MAKE_UNIFORM_PARAM(2, "m");
  MAKE_UNIFORM_PARAM(2, "n1");
  MAKE_UNIFORM_PARAM(2, "n2");
  MAKE_UNIFORM_PARAM(2, "n3");
  MAKE_UNIFORM_PARAM(2, "a");
  MAKE_UNIFORM_PARAM(2, "b");
  MAKE_UNIFORM_PARAM(2, "u");
  MAKE_UNIFORM_PARAM(2, "v");
  MAKE_UNIFORM_PARAM(1, "scale");
  MAKE_UNIFORM_PARAM(1, "segments");
  MAKE_UNIFORM_PARAM(1, "seed");
  MAKE_UNIFORM_PARAM(1, "deform");
  MAKE_UNIFORM_PARAM(3, "u_color");

#undef MAKE_UNIFORM_PARAM

  for (auto& uniform : state.uniforms) {
    uniform.parameter = engine.parameters.make(id, uniform.name);
  }

  state.rotation = engine.parameters.make(id, PARAM_ROTATION);
  state.rotation_axis = engine.parameters.make(id, PARAM_ROTATION_AXIS);
  state.translation = engine.parameters.make(id, PARAM_TRANSLATE);
  state.draw_mode = engine.parameters.make(id, PARAM_DRAW_MODE);

  state.outlet_colors = engine.registers.make(id, Register::Types::OUTLET, 0);
  state.outlet_normals = engine.registers.make(id, Register::Types::OUTLET, 1);
  state.outlet_depth = engine.registers.make(id, Register::Types::OUTLET, 2);
  state.fbo = engine.fbos.make(id);

  // Setup buffers
  SuperFormulaGeometry geometry(state.segments);

  GLuint shape_vbo;
  GLuint shape_ebo;

  glGenVertexArrays(1, &state.shape_vao);
  glBindVertexArray(state.shape_vao);

  glGenBuffers(1, &shape_vbo);
  glBindBuffer(GL_ARRAY_BUFFER, shape_vbo);
  glBufferData(GL_ARRAY_BUFFER, geometry.vertices_bytes, geometry.vertices,
               GL_STATIC_DRAW);

  glGenBuffers(1, &shape_ebo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, shape_ebo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, geometry.indices_bytes,
               geometry.indices, GL_STATIC_DRAW);

  // Setup shaders
  auto vert_shader = engine.shaders.create(VERT_SHADER);
  auto frag_shader = engine.shaders.create(FRAG_SHADER);
  state.program = engine.shaders.create(vert_shader, frag_shader);
  glUseProgram(state.program.handle);

  // Setup attributes and frag locations
  GLint pos_attrib = glGetAttribLocation(state.program.handle, "position");
  glVertexAttribPointer(pos_attrib, 3, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);

  // Uniforms
  state.model_view_matrix =
      glGetUniformLocation(state.program.handle, "model_view_matrix");
  state.proj_matrix =
      glGetUniformLocation(state.program.handle, "projection_matrix");
  for (UniformParameter& uparam : state.uniforms) {
    const char* name = engine.strings.lookup(uparam.name);
    uparam.uniform = glGetUniformLocation(state.program.handle, name);
  }

  default_gl_state();

  // Send the textures we will be writing to to the output registers
  auto color_tex = engine.fbos.get_color_attachment(state.fbo, 0);
  auto normal_tex = engine.fbos.get_color_attachment(state.fbo, 1);
  auto depth_tex = engine.fbos.get_depth_attachment(state.fbo);

  engine.registers.write(state.outlet_colors, &color_tex);
  engine.registers.write(state.outlet_normals, &normal_tex);
  engine.registers.write(state.outlet_depth, &depth_tex);
}

void FormulaObject::draw(Engine& engine) const {
  auto translation = glm::vec3(engine.parameters.get(state.translation, 0),
                               engine.parameters.get(state.translation, 1),
                               engine.parameters.get(state.translation, 2));

  auto axis = glm::vec3(engine.parameters.get(state.rotation_axis, 0),
                        engine.parameters.get(state.rotation_axis, 1),
                        engine.parameters.get(state.rotation_axis, 2));

  auto model_view_matrix = glm::mat4();

  auto aspect = (float)engine.settings.width / (float)engine.settings.height;
  auto projection_matrix = glm::perspective(45.0f, aspect, 0.1f, 100.f);
  model_view_matrix = glm::translate(model_view_matrix, translation);
  model_view_matrix = glm::rotate(
      model_view_matrix, engine.parameters.get(state.rotation, 0), axis);

  glUseProgram(state.program.handle);
  glUniformMatrix4fv(state.model_view_matrix, 1, 0,
                     glm::value_ptr(model_view_matrix));
  glUniformMatrix4fv(state.proj_matrix, 1, 0,
                     glm::value_ptr(projection_matrix));
  update_uniforms(engine, state.uniforms);

  GLenum mode = to_draw_mode(std::min<float>(
      std::max<float>(engine.parameters.get(state.draw_mode, 0), 0), 10));

  engine.fbos.bind_fbo(state.fbo);
  glClearColor(0.2, 0.2, 0.2, 1);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glBindVertexArray(state.shape_vao);
  glDrawElements(mode, (state.segments + 1) * (state.segments + 1) * 6,
                 GL_UNSIGNED_INT, 0);
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<FormulaState, FormulaObject>("gfx-superformula");
}
