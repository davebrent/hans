#include <cstring>
#include "hans/object.hpp"

using namespace hans;

#define MAX_FRAMES 30
#define ARG_NAME 0xd4c943cba60c270b /* name */

struct SndTexState {
  GLuint texture;
  hash name;
  Register outlet;
  uint32_t texture_value;
  audio::sample* samples = nullptr;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(name);
  }
};

class SndTexObject : protected GraphicsObject {
  friend class hans::PluginManager;

 public:
  using GraphicsObject::GraphicsObject;
  ~SndTexObject();
  virtual void create(IConfigurator& configurator) override;
  virtual void setup(context& ctx) override;
  virtual void update(context& ctx) override;
  virtual void draw(context& ctx) const override;

 private:
  SndTexState state;
};

SndTexObject::~SndTexObject() {
  delete[] state.samples;
}

void SndTexObject::create(IConfigurator& configurator) {
  for (const auto& arg : configurator.arguments()) {
    if (arg.name == ARG_NAME && arg.type == Argument::Types::STRING) {
      state.name = arg.string;
    }
  }

  configurator.request(IConfigurator::Resources::OUTLET, 1);
}

void SndTexObject::setup(context& ctx) {
  auto blocksize = ctx.settings.blocksize;

  state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
  state.samples = new audio::sample[blocksize * MAX_FRAMES];

  glGenTextures(1, &state.texture);
}

void SndTexObject::update(context& ctx) {
  auto blocksize = ctx.settings.blocksize;
  auto framesize = blocksize * sizeof(audio::sample);
  auto available = ctx.ring_buffers.available(state.name);

  if (available >= MAX_FRAMES) {
    available = MAX_FRAMES - 1;
  }

  for (auto i = 0; i < available; ++i) {
    auto buffer = ctx.ring_buffers.read(state.name, i);
    auto dest = &state.samples[i * blocksize];
    std::memcpy(dest, buffer, framesize);
  }

  glBindTexture(GL_TEXTURE_2D, state.texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, 1, blocksize * available, 0, GL_RED,
               GL_FLOAT, state.samples);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}

void SndTexObject::draw(context& ctx) const {
  ctx.registers.write(state.outlet, state.texture);
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<SndTexState, SndTexObject>("gfx-sndtex");
}
