#define GLM_FORCE_RADIANS
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/mat4x4.hpp>
#include <glm/vec3.hpp>

#include "hans/hasher.hpp"
#include "hans/object.hpp"

#define SHADER_VERT 0xa5734f97f7b8fcfd /* planes/shader/vert */
#define SHADER_FRAG 0x9e9fae9aa05c4b94 /* planes/shader/frag */

#define DEG_90 1.5708f

using namespace hans;

class Gate {
 public:
  void set(uint64_t over, uint64_t multiples) {
    _over = over;
    _multiples = multiples;
  }

  void set(uint64_t over) {
    set(over, 1);
  }

  int open() {
    if (_updates % _over == 0) {
      if (!_paused) {
        return _multiple;
      }
      return -1;
    } else if (!_paused) {
      return _multiple;
    }
    return -1;
  }

  void update() {
    if (_updates % _over == 0) {
      _paused = !_paused;
      _multiple += 1;
      _multiple %= _multiples;
    }

    _updates++;
  }

 private:
  uint64_t _over = 1;
  uint64_t _multiples = 1;
  uint64_t _multiple = 0;
  uint64_t _updates = 0;
  bool _paused = false;
};

struct Rotation {
  float amount;
  glm::vec3 axis;
};

struct Plane {
  glm::vec3 normal;
  glm::mat4 matrix;
  glm::vec3 color;
  Rotation rotation;
};

// Whats the difference between a modelMatrix & modelViewMatrix?

struct PlanesState {
  Register outlet;
  graphics::FBO fbo;
  graphics::ShaderProgram program;
  GLuint texture;
  GLuint vao;

  GLuint mvm;
  GLuint pjm;
  GLuint color;
  GLuint scale_loc;

  float scale_big;
  float theta;
  int frameno;
  bool paused;

  glm::vec3 rotation_axis;
  glm::mat4 proj_matrix;
  std::vector<Plane> planes;
  std::vector<Plane> background;

  Gate rotation_gate;
  Gate rotation_axis_gate;

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

static std::vector<Plane> make_cube() {
  Plane a;
  a.rotation.amount = 0;
  a.rotation.axis = glm::vec3(1, 1, 1);
  a.normal = glm::vec3(0, 0, 1);
  a.color = glm::vec3(0.9);

  Plane b;
  b.rotation.amount = DEG_90 * 2;
  b.rotation.axis = glm::vec3(1, 0, 0);
  b.normal = glm::vec3(0, 0, -1);
  b.color = glm::vec3(0.9);

  Plane c;
  c.rotation.amount = DEG_90;
  c.rotation.axis = glm::vec3(0, 1, 0);
  c.normal = glm::vec3(1, 0, 0);
  c.color = glm::vec3(0.9);

  Plane d;
  d.rotation.amount = DEG_90 * 3.f;
  d.rotation.axis = glm::vec3(0, 1, 0);
  d.normal = glm::vec3(-1, 0, 0);
  d.color = glm::vec3(0.9);

  Plane e;
  e.rotation.amount = DEG_90;
  e.rotation.axis = glm::vec3(1, 0, 0);
  e.normal = glm::vec3(0, -1, 0);
  e.color = glm::vec3(0.9);

  Plane f;
  f.rotation.amount = DEG_90 * 3.f;
  f.rotation.axis = glm::vec3(1, 0, 0);
  f.normal = glm::vec3(0, 1, 0);
  f.color = glm::vec3(0.9);
  return {b, c, e, d, a, f};
}

static glm::mat4 to_world_space(const Plane& plane, float scale) {
  auto matrix = glm::translate(plane.matrix, plane.normal * (scale * 0.5f));
  matrix = glm::rotate(matrix, plane.rotation.amount, plane.rotation.axis);
  return matrix;
}

static void hexify(std::string str) {
  std::stringstream stream;
  stream << std::hex << hasher(str.c_str());
  std::string result(stream.str());
  std::cout << result << std::endl;
}

static void default_gl_state() {
  glEnable(GL_CULL_FACE);
  glFrontFace(GL_CCW);
  glCullFace(GL_BACK);

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

class PlanesObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;

  virtual void create(IConfigurator& configurator) override {
    configurator.request(IConfigurator::Resources::OUTLET, 1);
  }

  virtual void setup(context& ctx) override {
    state.fbo = ctx.fbos.make(id);
    state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
    state.program = ctx.shaders.create(ctx.shaders.create(SHADER_VERT),
                                       ctx.shaders.create(SHADER_FRAG));

    float vertices[] = {-0.5, 0.5, -0.5, -0.5, 0.5, -0.5, 0.5, 0.5};
    int indices[] = {0, 1, 2, 2, 3, 0};

    glGenVertexArrays(1, &state.vao);
    glBindVertexArray(state.vao);

    GLuint vbo;
    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    GLuint ebo;
    glGenBuffers(1, &ebo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices,
                 GL_STATIC_DRAW);

    glBindVertexArray(state.vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);

    glUseProgram(state.program.handle);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
    glEnableVertexAttribArray(0);

    state.mvm = glGetUniformLocation(state.program.handle, "model_view_matrix");
    state.pjm = glGetUniformLocation(state.program.handle, "projection_matrix");
    state.color = glGetUniformLocation(state.program.handle, "u_color");
    state.scale_loc = glGetUniformLocation(state.program.handle, "u_scale");

    auto width = ctx.settings.graphics.width;
    auto height = ctx.settings.graphics.height;
    auto hw = (float)width / 2;
    auto hh = (float)height / 2;
    auto near = 0.1f;
    auto far = 1000.f;

    float s = 250;    // size of cube
    float hs = s / 2; // half size of cube

    state.scale_big = s;
    state.theta = 0;
    state.frameno = 0;
    state.paused = 0;
    state.rotation_gate.set(120);
    state.rotation_axis_gate.set(60, 5);
    state.rotation_axis = glm::vec3(1, 0, 0);

    // Setup the orthographic projection
    // clang-format off
    state.proj_matrix = glm::ortho(-hw, hw, -hh, hh, near, far);
    state.proj_matrix = glm::translate(state.proj_matrix, glm::vec3(0, 0, -500));
    // rotated vertically (around the horizontal axis) by about 35.264°
    state.proj_matrix = glm::rotate(state.proj_matrix, 0.616101f, glm::vec3(1, 0, 0));
    // then ±45° around the vertical axis.
    state.proj_matrix = glm::rotate(state.proj_matrix, 0.785398f, glm::vec3(0, 1, 0));
    // clang-format on

    state.planes = make_cube();
    state.background = make_cube();
  }

  virtual void update(context& ctx) override {
    if (state.rotation_gate.open() == -1) {
      auto t = 0.785398f / 30.0f;
      state.proj_matrix = glm::rotate(state.proj_matrix, t, state.rotation_axis);
    }

    switch (state.rotation_axis_gate.open()) {
    case 0:
      state.rotation_axis = glm::vec3(0, 1, 0);
      break;
    case 1:
      state.rotation_axis = glm::vec3(1, 0, 0);
      break;
    case 2:
      state.rotation_axis = glm::vec3(1, 0, 1);
      break;
    case 3:
      state.rotation_axis = glm::vec3(1, 1, 0);
      break;
    case 4:
      state.rotation_axis = glm::vec3(0, 1, 1);
      break;
    case -1:
      state.rotation_axis = glm::vec3(1, 1, 1);

    }

    state.rotation_axis_gate.update();
    state.rotation_gate.update();
  }

  virtual void draw(context& ctx) const override {
    default_gl_state();

    ctx.fbos.bind_fbo(state.fbo);
    glClearColor(0.129, 0.129, 0.129, 1);
    glClear(GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glDisable(GL_CULL_FACE);
    glUseProgram(state.program.handle);

    glUniformMatrix4fv(state.pjm, 1, 0, glm::value_ptr(state.proj_matrix));
    glBindVertexArray(state.vao);

    auto scale = state.scale_big;
    for (const auto& plane : state.planes) {
      auto model = to_world_space(plane, scale * 1.3);

      auto mini_scale = scale * 0.5f;
      glUniform1f(state.scale_loc, mini_scale * 0.5f);
      for (const auto& plane : state.background) {
        auto mini_model = model * to_world_space(plane, mini_scale * 1.3);
        glUniformMatrix4fv(state.mvm, 1, 0, glm::value_ptr(mini_model));
        glUniform3fv(state.color, 1, glm::value_ptr(plane.color));
        glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
      }

      glUniform1f(state.scale_loc, scale * 0.5f);
      glUniformMatrix4fv(state.mvm, 1, 0, glm::value_ptr(model));
      glUniform3fv(state.color, 1, glm::value_ptr(plane.color));
      glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
    }

    auto texture = ctx.fbos.get_color_attachment(state.fbo, 0);
    ctx.registers.write(state.outlet, texture);
  }

 private:
  PlanesState state;
};

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<PlanesState, PlanesObject>("gfx-planes");
}
