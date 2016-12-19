#include "hans/engine/object.hpp"

#define FILTER_AMOUNT 0x11ed734ddeee006c
#define FILTER_HALFTONE_SHADER 0x5b44d05452629d5a
#define FILTER_SOBEL_SHADER 0x52938ee09e9f277a
#define FILTER_INVERT_SHADER 0xb7c4e0bff104a4bf
#define FILTER_ZOOMBLUR_SHADER 0x5ef3fc01a1325fe9
#define FILTER_ARG_NAME 0xd4c943cba60c270b
#define FILTER_RGBSPLIT_SHADER 0xaf8de3558a198d7a
#define FILTER_PIXELATE_SHADER 0x41eb10bf4db253f7
#define FILTER_ARG_SHADER 0xcce8d5b5f5ae333f
#define FILTER_PASSTHROUGH_SHADER 0x56b3ebbf4277fe64
#define FILTER_VERT_SHADER 0x349e8c3fc580b1cc
#define FILTER_DOTSCREEN_SHADER 0x897d66aa1c0e88ae
#define FILTER_CGADISPLAY_SHADER 0x261d3c19febd7f25
#define FILTER_GAMMA_SHADER 0xf1eedb418e8f6683
#define FILTER_SEPIA_SHADER 0x104a242028d65dc6
#define FILTER_GREYSCALE_SHADER 0x18ae8a6b858ebee

using namespace hans;
using namespace hans::engine;

static const float VERTICES[] = {-1, 1, -1, -1, 1, -1, 1, 1};
static const int INDEX[] = {0, 1, 2, 2, 3, 0};

struct FilterState {
  graphics::FBO fbo;
  GLuint vao;
  GLuint texture;
  hash shader;
  graphics::ShaderProgram program;
  Register inlet_texture;
  Register outlet_texture;
  Parameter amount;
  GLuint u_center_loc;
  GLuint u_resolution_loc;
  GLuint u_amount_loc;
  uint32_t texture_value;
};

class FilterObject : protected GraphicsObject {
  friend class hans::engine::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void update(Engine& engine) override {
  }
  virtual void draw(Engine& engine) const override;

 private:
  FilterState state;
};

void FilterObject::create(IPatcher& patcher) {
  patcher.request(IPatcher::Resources::INLET, 1);
  patcher.request(IPatcher::Resources::OUTLET, 1);

  state.shader = FILTER_PASSTHROUGH_SHADER;
  for (const auto& arg : patcher.arguments()) {
    if (arg.type == Argument::Types::STRING && arg.name == FILTER_ARG_NAME) {
      state.shader = arg.string;
    }
  }
}

void FilterObject::setup(Engine& engine) {
  state.amount = engine.parameters.make(id, FILTER_AMOUNT);
  state.inlet_texture = engine.registers.make(id, Register::Types::INLET, 0);
  state.outlet_texture = engine.registers.make(id, Register::Types::OUTLET, 0);
  state.fbo = engine.fbos.make(id);

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

  auto v_shader = engine.shaders.create(FILTER_VERT_SHADER);
  auto f_shader = engine.shaders.create(state.shader);
  state.program = engine.shaders.create(v_shader, f_shader);
  glUseProgram(state.program.handle);

  state.u_resolution_loc =
      glGetUniformLocation(state.program.handle, "u_resolution");
  state.u_center_loc = glGetUniformLocation(state.program.handle, "u_center");
  state.texture = glGetUniformLocation(state.program.handle, "u_texture");
  state.u_amount_loc = glGetUniformLocation(state.program.handle, "u_amount");

  GLint pos_attrib = glGetAttribLocation(state.program.handle, "position");
  glVertexAttribPointer(pos_attrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);

  auto register_value = engine.registers.read(state.inlet_texture);
  state.texture_value = *static_cast<uint32_t*>(register_value);

  // Send the textures we will be writing to to the outlet
  auto out_tex = engine.fbos.get_color_attachment(state.fbo, 0);
  engine.registers.write(state.outlet_texture, &out_tex);
}

void FilterObject::draw(Engine& engine) const {
  glUseProgram(state.program.handle);

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, state.texture_value);
  glUniform1i(state.texture, 0);

  int input_width;
  int input_height;

  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &input_width);
  glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, &input_height);

  glUniform2f(state.u_resolution_loc, input_width, input_height);
  glUniform2f(state.u_center_loc, input_width / 2.f, input_height / 2.f);
  glUniform1f(state.u_amount_loc, engine.parameters.get(state.amount, 0));

  engine.fbos.bind_fbo(state.fbo);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glBindVertexArray(state.vao);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<FilterState, FilterObject>("gfx-filter");
}