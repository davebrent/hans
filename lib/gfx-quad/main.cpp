#include "hans/engine/object.hpp"

#define LIBQUAD_VERT_SHADER 0xb3e177b2033e92a3
#define LIBQUAD_FRAG_SHADER 0x14702fc4633a84b9

using namespace hans;
using namespace hans::engine;

static const float VERTICES[] = {-1, 1, -1, -1, 1, -1, 1, 1};
static const int INDEX[] = {0, 1, 2, 2, 3, 0};

struct QuadState {
  graphics::FBO fbo;
  GLuint vao;
  GLuint texture;
  graphics::Shader::Instance v_shader;
  graphics::Shader::Instance f_shader;
  graphics::ShaderProgram program;
  Register inlet;
  uint32_t texture_value;
};

class QuadObject : protected GraphicsObject {
  friend class LibraryManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void update(Engine& engine) override {
  }
  virtual void draw(Engine& engine) const override;

 private:
  QuadState state;
};

void QuadObject::create(IPatcher& patcher) {
  patcher.request(IPatcher::Resources::INLET, 1);
}

void QuadObject::setup(Engine& engine) {
  state.fbo = engine.fbos->make(id);
  state.inlet = engine.registers->make(id, Register::Types::INLET, 0);

  GLuint vbo;
  GLuint ebo;

  glGenVertexArrays(1, &state.vao);
  glBindVertexArray(state.vao);

  glGenBuffers(1, &vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBufferData(GL_ARRAY_BUFFER, sizeof(VERTICES), VERTICES, GL_STATIC_DRAW);

  glGenBuffers(1, &ebo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(INDEX), INDEX, GL_STATIC_DRAW);

  glBindVertexArray(state.vao);
  glBindBuffer(GL_ARRAY_BUFFER, vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);

  state.v_shader = engine.shaders->create(LIBQUAD_VERT_SHADER);
  state.f_shader = engine.shaders->create(LIBQUAD_FRAG_SHADER);
  state.program = engine.shaders->create(state.v_shader, state.f_shader);
  glUseProgram(state.program.handle);

  state.texture = glGetUniformLocation(state.program.handle, "u_texture");

  GLint pos_attrib = glGetAttribLocation(state.program.handle, "position");
  glVertexAttribPointer(pos_attrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);

  auto register_value = engine.registers->read(state.inlet);
  state.texture_value = *static_cast<uint32_t*>(register_value);
}

void QuadObject::draw(Engine& engine) const {
  glUseProgram(state.program.handle);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, state.texture_value);
  glUniform1i(state.texture, 0);

  engine.fbos->release_fbo();
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(state.vao);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

HANS_PLUGIN_INIT(LibraryManager* library) {
  library->add_object<QuadState, QuadObject>("gfx-quad");
}
