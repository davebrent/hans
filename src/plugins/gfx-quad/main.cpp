#include "hans/object.hpp"

#define LIBQUAD_VERT_SHADER 0xb3e177b2033e92a3
#define LIBQUAD_FRAG_SHADER 0x14702fc4633a84b9

using namespace hans;

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

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

class QuadObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(IConfigurator& configurator) override;
  virtual void setup(context& ctx) override;
  virtual void update(context& ctx) override {
  }
  virtual void draw(context& ctx) const override;

 private:
  QuadState state;
};

void QuadObject::create(IConfigurator& configurator) {
  configurator.request(IConfigurator::Resources::INLET, 1);
}

void QuadObject::setup(context& ctx) {
  state.fbo = ctx.fbos.make(id);
  state.inlet = ctx.registers.make(id, Register::Types::INLET, 0);

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

  state.v_shader = ctx.shaders.create(LIBQUAD_VERT_SHADER);
  state.f_shader = ctx.shaders.create(LIBQUAD_FRAG_SHADER);
  state.program = ctx.shaders.create(state.v_shader, state.f_shader);
  glUseProgram(state.program.handle);

  state.texture = glGetUniformLocation(state.program.handle, "u_texture");

  GLint pos_attrib = glGetAttribLocation(state.program.handle, "position");
  glVertexAttribPointer(pos_attrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);
}

void QuadObject::draw(context& ctx) const {
  if (!ctx.registers.has_data(state.inlet)) {
    return;
  }

  auto x = (ctx.window.width * 0.5) - (ctx.settings.graphics.width * 0.5);
  auto y = (ctx.window.height * 0.5) - (ctx.settings.graphics.height * 0.5);

  glUseProgram(state.program.handle);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, ctx.registers.read(state.inlet));
  glUniform1i(state.texture, 0);
  glBindVertexArray(state.vao);

  glBindFramebuffer(GL_FRAMEBUFFER, ctx.window.fbo);
  glEnable(GL_SCISSOR_TEST);
  glScissor(x, y, ctx.settings.graphics.width, ctx.settings.graphics.height);
  glViewport(x, y, ctx.settings.graphics.width, ctx.settings.graphics.height);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<QuadState, QuadObject>("gfx-quad");
}
