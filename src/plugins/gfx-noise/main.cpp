#include <noise/noise.h>
#include "hans/object.hpp"

#define FREQUENCY 0xce6a758b9a7e1778   /* frequency */
#define OCTAVECOUNT 0x7c767977096112b2 /* octavecount */
#define LACUNARITY 0xb237ff7f8b30410f  /* lacunarity */

using namespace hans;

struct PerlinState {
  Parameter frequency;
  Parameter lacunarity;
  Parameter octavecount;
  Register outlet;
  uint32_t texture;
  noise::module::Perlin noise;
  uint8_t* data;
  size_t width;
  size_t height;
  size_t frameno;
  size_t channels;

  template <class Archive>
  void serialize(Archive& ar) {
  }
};

class PerlinObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;

  virtual void create(IConfigurator& configurator) override {
    configurator.request(IConfigurator::Resources::OUTLET, 1);
  }

  virtual void setup(context& ctx) override {
    state.frequency = ctx.parameters.make(id, FREQUENCY);
    state.octavecount = ctx.parameters.make(id, OCTAVECOUNT);
    state.lacunarity = ctx.parameters.make(id, LACUNARITY);
    state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
    state.channels = 1;
    state.width = 256;
    state.height = 128;
    state.frameno = 0;
    state.data = new uint8_t[state.width * state.height * state.channels]();

    glGenTextures(1, &state.texture);
    glBindTexture(GL_TEXTURE_2D, state.texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, state.width, state.height, 0, GL_RED,
                 GL_UNSIGNED_BYTE, state.data);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  }

  virtual void update(context& ctx) override {
    auto frequency = ctx.parameters.get(state.frequency, 0);
    auto lacunarity = ctx.parameters.get(state.lacunarity, 0);
    auto octavecount = ctx.parameters.get(state.octavecount, 0);

    state.noise.SetSeed(state.frameno);
    state.noise.SetFrequency(frequency);
    state.noise.SetOctaveCount(octavecount);
    state.noise.SetLacunarity(lacunarity);

    for (auto i = 0; i < state.width * state.height * state.channels; ++i) {
      float x = (i % state.width);
      float y = (i / state.width);
      auto value = state.noise.GetValue(x / state.width, y / state.height, 0.5);
      state.data[i] = ((value + 1) * 0.5) * 255.f;
    }

    state.frameno++;
  }

  virtual void draw(context& ctx) const override {
    glBindTexture(GL_TEXTURE_2D, state.texture);
    glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, state.width, state.height, GL_RED,
                    GL_UNSIGNED_BYTE, state.data);
    ctx.registers.write(state.outlet, state.texture);
  }

 private:
  PerlinState state;
};

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<PerlinState, PerlinObject>("gfx-perlin");
}
