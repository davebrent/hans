#include "hans/engine/object.hpp"

#define LIBQUAD_VERT_SHADER 0xb3e177b2033e92a3
#define LIBQUAD_FRAG_SHADER 0x14702fc4633a84b9

using namespace hans;

static const float VERTICES[] = {-1, 1, -1, -1, 1, -1, 1, 1};
static const int INDEX[] = {0, 1, 2, 2, 3, 0};

struct QuadState {
  hans_fbo fbo;
  GLuint vao;
  GLuint texture;
  hans_shader_instance v_shader;
  hans_shader_instance f_shader;
  hans_shader_program_instance program;
  hans_register inlet;
  uint32_t texture_value;
};

class QuadObject : protected GraphicsObject {
  friend class hans::engine::LibraryManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(ObjectPatcher& patcher) override;
  virtual void setup(hans_object_api& api) override;
  virtual void update(hans_object_api& api) override {
  }
  virtual void draw(hans_object_api& api) override;

 private:
  QuadState state;
};

void QuadObject::create(ObjectPatcher& patcher) {
  patcher.request(HANS_INLET, 1);
}

void QuadObject::setup(hans_object_api& api) {
  state.fbo = api.fbos->make(id);
  state.inlet = api.registers->make(id, HANS_INLET, 0);

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

  state.v_shader = api.shaders->create_shader(LIBQUAD_VERT_SHADER);
  state.f_shader = api.shaders->create_shader(LIBQUAD_FRAG_SHADER);
  state.program = api.shaders->create_program(state.v_shader, state.f_shader);
  glUseProgram(state.program.handle);

  state.texture = glGetUniformLocation(state.program.handle, "u_texture");

  GLint pos_attrib = glGetAttribLocation(state.program.handle, "position");
  glVertexAttribPointer(pos_attrib, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos_attrib);

  auto register_value = api.registers->read(state.inlet);
  state.texture_value = *static_cast<uint32_t*>(register_value);
}

void QuadObject::draw(hans_object_api& api) {
  glUseProgram(state.program.handle);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, state.texture_value);
  glUniform1i(state.texture, 0);

  api.fbos->release_fbo();
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(state.vao);
  glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);
}

extern "C" {
void setup(hans::engine::LibraryManager* library) {
  library->add_object<QuadState, QuadObject>("gfx-quad");
}
}
