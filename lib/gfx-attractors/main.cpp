#include <cmath>
#include "hans/engine/object.hpp"

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
  hans_fbo fbo;
  hans_parameter a;
  hans_parameter b;
  hans_parameter c;
  hans_parameter d;
  hans_parameter e;
  hans_parameter f;
  hans_register outlet;
  hans_shader_program_instance program;
  float* positions;
  size_t buffer_length;
  GLuint position_buffer;
  GLuint buffer_length_loc;
};

class AttractorsObject : protected GraphicsObject {
  friend class engine::LibraryManager;

 public:
  using GraphicsObject::GraphicsObject;
  virtual void create(ObjectPatcher& patcher) override;
  virtual void setup(hans_object_api& api) override;
  virtual void update(hans_object_api& api) override;
  virtual void draw(hans_object_api& api) override;

 private:
  AttractorsState state;
};

void AttractorsObject::create(ObjectPatcher& patcher) {
  patcher.request(HANS_OUTLET, 1);
}

void AttractorsObject::setup(hans_object_api& api) {
  state.a = api.parameters->make(id, PARAM_NAME_A);
  state.b = api.parameters->make(id, PARAM_NAME_B);
  state.c = api.parameters->make(id, PARAM_NAME_C);
  state.d = api.parameters->make(id, PARAM_NAME_D);
  state.e = api.parameters->make(id, PARAM_NAME_E);
  state.f = api.parameters->make(id, PARAM_NAME_F);

  state.outlet = api.registers->make(id, HANS_OUTLET, 0);
  state.fbo = api.fbos->make(id);
  state.buffer_length = NUM_PARTICLES * 2;
  state.positions = new float[state.buffer_length];

  auto texture = api.fbos->get_color_attachment(state.fbo, 0);
  api.registers->write(state.outlet, &texture);

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

  auto vert_shdr = api.shaders->create_shader(GFX_SHDRS_VERTEX);
  auto frag_shdr = api.shaders->create_shader(GFX_SHDRS_FRAGMENT);
  state.program = api.shaders->create_program(vert_shdr, frag_shdr);
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

void AttractorsObject::update(hans_object_api& api) {
  float a = api.parameters->get(state.a, 0);
  float b = api.parameters->get(state.b, 0);
  float c = api.parameters->get(state.c, 0);
  float d = api.parameters->get(state.d, 0);
  float e = api.parameters->get(state.e, 0);
  float f = api.parameters->get(state.f, 0);

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

void AttractorsObject::draw(hans_object_api& api) {
  glUseProgram(state.program.handle);

  api.fbos->bind_fbo(state.fbo);

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
}

extern "C" {
void setup(engine::LibraryManager* library) {
  library->add_object<AttractorsState, AttractorsObject>("gfx-attractors");
}
}
