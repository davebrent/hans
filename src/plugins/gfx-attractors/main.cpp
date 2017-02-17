#include <cmath>
#include "hans/object.hpp"

#define NUM_PARTICLES 100000
#define GFX_SHDRS_VERTEX 0x2852785caca700a4   /* attractors/shaders/vert */
#define GFX_SHDRS_FRAGMENT 0x956f8b548265e345 /* attractors/shaders/frag */
#define PARAM_NAME_A 0x71717d2d36b6b11        /* a */
#define PARAM_NAME_B 0xea8bfc7d922a2a37       /* b */
#define PARAM_NAME_C 0xd8788d18ba82e0b5       /* c */
#define PARAM_NAME_D 0x17dffbc5a8f17839       /* d */
#define PARAM_NAME_E 0x7115e430772d6a11       /* e */
#define PARAM_NAME_F 0x5647bce94f90d9aa       /* f */

using namespace hans;

struct AttractorsState {
  GLuint vao;
  graphics::FBO fbo;
  Parameter a;
  Parameter b;
  Parameter c;
  Parameter d;
  Parameter e;
  Parameter f;
  Register outlet;
  graphics::ShaderProgram program;
  float* positions;
  size_t buffer_length;
  GLuint position_buffer;
  GLuint buffer_length_loc;

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

class AttractorsObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(IConfigurator& configurator) override;
  virtual void setup(context& ctx) override;
  virtual void update(context& ctx) override;
  virtual void draw(context& ctx) const override;

 private:
  AttractorsState state;
};

void AttractorsObject::create(IConfigurator& configurator) {
  configurator.request(IConfigurator::Resources::OUTLET, 1);
}

void AttractorsObject::setup(context& ctx) {
  state.a = ctx.parameters.make(id, PARAM_NAME_A);
  state.b = ctx.parameters.make(id, PARAM_NAME_B);
  state.c = ctx.parameters.make(id, PARAM_NAME_C);
  state.d = ctx.parameters.make(id, PARAM_NAME_D);
  state.e = ctx.parameters.make(id, PARAM_NAME_E);
  state.f = ctx.parameters.make(id, PARAM_NAME_F);

  state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
  state.fbo = ctx.fbos.make(id);
  state.buffer_length = NUM_PARTICLES * 2;
  state.positions = new float[state.buffer_length];

  glGenVertexArrays(1, &state.vao);
  glBindVertexArray(state.vao);

  auto ids = new float[NUM_PARTICLES];
  for (auto c = 0; c < NUM_PARTICLES; ++c) {
    ids[c] = c;
  }

  GLuint id_buffer;
  glGenBuffers(1, &id_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, id_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(float) * NUM_PARTICLES, ids,
               GL_STATIC_DRAW);
  delete[] ids;

  auto size = sizeof(float) * state.buffer_length;

  glGenBuffers(1, &state.position_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, state.position_buffer);
  glBufferData(GL_ARRAY_BUFFER, size, nullptr, GL_STREAM_DRAW);

  auto vert_shdr = ctx.shaders.create(GFX_SHDRS_VERTEX);
  auto frag_shdr = ctx.shaders.create(GFX_SHDRS_FRAGMENT);
  state.program = ctx.shaders.create(vert_shdr, frag_shdr);
  glUseProgram(state.program.handle);

  auto pos = 0;
  glBindBuffer(GL_ARRAY_BUFFER, id_buffer);
  pos = glGetAttribLocation(state.program.handle, "id");
  glVertexAttribPointer(pos, 1, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);

  glBindBuffer(GL_ARRAY_BUFFER, state.position_buffer);
  pos = glGetAttribLocation(state.program.handle, "position");
  glVertexAttribPointer(pos, 2, GL_FLOAT, GL_FALSE, 0, 0);
  glEnableVertexAttribArray(pos);
}

void AttractorsObject::update(context& ctx) {
  float a = ctx.parameters.get(state.a, 0);
  float b = ctx.parameters.get(state.b, 0);
  float c = ctx.parameters.get(state.c, 0);
  float d = ctx.parameters.get(state.d, 0);
  float e = ctx.parameters.get(state.e, 0);
  float f = ctx.parameters.get(state.f, 0);

  float xs = 0, ys = 0, zs = 0;

  // Based on http://koaning.io/fluctuating-repetition.html
  for (int i = 0; i < state.buffer_length; i += 2) {
    state.positions[i + 0] = xs;
    state.positions[i + 1] = ys;
    xs = (sin(a * xs) + sin(b * ys)) - cos(c * zs);
    ys = (sin(d * xs) + sin(e * ys)) - cos(f * zs);
    zs = zs + 0.1;
  }

  glBindBuffer(GL_ARRAY_BUFFER, state.position_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(float) * state.buffer_length,
                  state.positions);
}

void AttractorsObject::draw(context& ctx) const {
  glUseProgram(state.program.handle);

  ctx.fbos.bind_fbo(state.fbo);

  glEnable(GL_ALPHA_TEST);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  glEnable(GL_POINT_SMOOTH);
  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glBindVertexArray(state.vao);
  glPointSize(1);
  glDrawArrays(GL_POINTS, 0, state.buffer_length);

  auto texture = ctx.fbos.get_color_attachment(state.fbo, 0);
  ctx.registers.write(state.outlet, texture);
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<AttractorsState, AttractorsObject>("gfx-attractors");
}
