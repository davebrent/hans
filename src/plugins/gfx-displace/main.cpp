#include <random>
#include "hans/object.hpp"
#define NANOVG_GL3_IMPLEMENTATION 1
#include <nanovg.h>
#include <nanovg_gl.h>

#define SHADER_VERT 0x1a388558a7a03b80    /* displace/shader/vert */
#define SHADER_FRAG 0xae926490b2177fd3    /* displace/shader/frag */
#define PARAM_X 0x8295151af8224269        /* x */
#define PARAM_Y 0x9a5db2cd2c1fd6ce        /* y */
#define PARAM_W 0x22727cb14c3bb41d        /* w */
#define PARAM_H 0x46be922470df1f1f        /* h */
#define PARAM_STEPS 0x12de0a79e23531aa    /* steps */
#define PARAM_RESPAWN 0x7836898f9c61aedf  /* respawn */
#define PARAM_DISPLACE 0x7ebc26f842b1771a /* displace */

using namespace hans;

struct Rect {
  float x0 = 0;
  float y0 = 0;
  float w0 = 0;
  float h0 = 0;
  float x1 = 0;
  float y1 = 0;
  float w1 = 0;
  float h1 = 0;
  float displace[2];
  uint32_t steps = 0;
  uint32_t taken = 0;
};

struct DisplaceState {
  Register inlet0;
  Register inlet1;
  Register outlet;
  Parameter x;
  Parameter y;
  Parameter w;
  Parameter h;
  Parameter steps;
  Parameter respawn;
  Parameter displacement;
  std::vector<Rect> rects;
  graphics::FBO fbo;
  graphics::ShaderProgram program;
  GLuint displace;
  GLuint texture;
  GLuint vao;
  NVGcontext* vg;

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

float randomize(Parameter& p, context& ctx) {
  float min = ctx.parameters.get(p, 0);
  float max = ctx.parameters.get(p, 1);
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_real_distribution<> dist(min, max);
  return dist(gen);
}

float lerp(float v0, float v1, float t) {
  return v0 + t * (v1 - v0);
}

class DisplaceObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;

  virtual void create(IConfigurator& configurator) override {
    configurator.request(IConfigurator::Resources::INLET, 2);
    configurator.request(IConfigurator::Resources::OUTLET, 1);
  }

  virtual void setup(context& ctx) override {
    state.fbo = ctx.fbos.make(id);
    state.inlet0 = ctx.registers.make(id, Register::Types::INLET, 0);
    state.inlet1 = ctx.registers.make(id, Register::Types::INLET, 1);
    state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
    state.x = ctx.parameters.make(id, PARAM_X);
    state.y = ctx.parameters.make(id, PARAM_Y);
    state.w = ctx.parameters.make(id, PARAM_W);
    state.h = ctx.parameters.make(id, PARAM_H);
    state.steps = ctx.parameters.make(id, PARAM_STEPS);
    state.respawn = ctx.parameters.make(id, PARAM_RESPAWN);
    state.displacement = ctx.parameters.make(id, PARAM_DISPLACE);
    state.program = ctx.shaders.create(ctx.shaders.create(SHADER_VERT),
                                       ctx.shaders.create(SHADER_FRAG));
    for (auto i = 0; i < 12; ++i) {
      Rect r;
      state.rects.push_back(r);
    }

    state.vg = nvgCreateGL3(NVG_ANTIALIAS | NVG_STENCIL_STROKES | NVG_DEBUG);

    float vertices[] = {-1, 1, -1, -1, 1, -1, 1, 1};
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

    auto pgm = state.program.handle;
    glUseProgram(pgm);
    state.texture = glGetUniformLocation(pgm, "image");
    state.displace = glGetUniformLocation(pgm, "displace");

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
    glEnableVertexAttribArray(0);
  }

  virtual void update(context& ctx) override {
    auto respawn = ctx.parameters.get(state.respawn, 0);
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<> dist(0, 1);

    for (auto& rect : state.rects) {
      if (rect.taken == rect.steps) {
        if (dist(gen) > respawn) {
          continue;
        }

        rect.x0 = randomize(state.x, ctx);
        rect.y0 = randomize(state.y, ctx);
        rect.w0 = randomize(state.w, ctx);
        rect.h0 = randomize(state.h, ctx);
        rect.x1 = randomize(state.x, ctx);
        rect.y1 = randomize(state.y, ctx);
        rect.w1 = randomize(state.w, ctx);
        rect.h1 = randomize(state.h, ctx);
        rect.steps = randomize(state.steps, ctx);
        rect.taken = 0;
        rect.displace[0] = randomize(state.displacement, ctx);
        rect.displace[1] = randomize(state.displacement, ctx);
      } else {
        float t = (float)rect.taken / (float)rect.steps;
        rect.x0 = lerp(rect.x0, rect.x1, t);
        rect.y0 = lerp(rect.y0, rect.y1, t);
        rect.w0 = lerp(rect.w0, rect.w1, t);
        rect.h0 = lerp(rect.h0, rect.h1, t);
        rect.taken++;
      }
    }
  }

  virtual void draw(context& ctx) const override {
    if (!ctx.registers.has_data(state.inlet0)) {
      ctx.fbos.bind_fbo(state.fbo);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
      auto texture = ctx.fbos.get_color_attachment(state.fbo, 0);
      ctx.registers.write(state.outlet, texture);
      return;
    }

    auto displace = ctx.fbos.get_color_attachment(state.fbo, 0);
    if (!ctx.registers.has_data(state.inlet1)) {
      auto width = 480;
      auto height = 270;
      ctx.fbos.bind_fbo(state.fbo);
      glClearColor(0.5, 0.5, 0.5, 1.0);
      glClear(GL_COLOR_BUFFER_BIT | GL_STENCIL_BUFFER_BIT |
              GL_DEPTH_BUFFER_BIT);
      nvgBeginFrame(state.vg, width, height, width / height);

      for (const auto& rect : state.rects) {
        nvgSave(state.vg);
        nvgBeginPath(state.vg);
        nvgStrokeColor(state.vg, nvgRGBA(0, 0, 0, 0));
        nvgFillColor(state.vg, nvgRGB(127.5 + (rect.displace[0] * 127.5),
                                      127.5 + (rect.displace[1] * 127.5), 0));

        auto x = rect.x0 - (rect.w0 / 2.f);
        auto y = rect.y0 - (rect.h0 / 2.f);
        nvgRect(state.vg, x, y, rect.w0, rect.h0);
        nvgFill(state.vg);
        nvgRestore(state.vg);
      }

      nvgEndFrame(state.vg);
    } else {
      displace = ctx.registers.read(state.inlet1);
    }

    glUseProgram(state.program.handle);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, ctx.registers.read(state.inlet0));
    glUniform1i(state.texture, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, displace);
    glUniform1i(state.displace, 1);

    glBindVertexArray(state.vao);
    glClear(GL_STENCIL_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

    auto texture = ctx.fbos.get_color_attachment(state.fbo, 0);
    ctx.registers.write(state.outlet, displace);
  }

 private:
  DisplaceState state;
};

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<DisplaceState, DisplaceObject>("gfx-displace");
}
