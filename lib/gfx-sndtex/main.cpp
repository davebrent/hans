#include <cstring>
#include "hans/engine/object.hpp"

using namespace hans;
using namespace hans::engine;

#define MAX_FRAMES 30
#define ARG_NAME 0xd4c943cba60c270b /* name */

struct SndTexState {
  GLuint texture;
  hash name;
  Register outlet;
  uint32_t texture_value;
  audio::sample* samples = nullptr;
};

class SndTexObject : protected GraphicsObject {
  friend class hans::engine::LibraryManager;

 public:
  using GraphicsObject::GraphicsObject;
  ~SndTexObject();
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void update(Engine& engine) override;
  virtual void draw(Engine& engine) const override {
  }

 private:
  SndTexState state;
};

SndTexObject::~SndTexObject() {
  delete[] state.samples;
}

void SndTexObject::create(IPatcher& patcher) {
  for (const auto& arg : patcher.arguments()) {
    if (arg.name == ARG_NAME && arg.type == Argument::Types::STRING) {
      state.name = arg.string;
    }
  }

  patcher.request(IPatcher::Resources::OUTLET, 1);
}

void SndTexObject::setup(Engine& engine) {
  auto blocksize = engine.config->blocksize;

  state.outlet = engine.registers->make(id, Register::Types::OUTLET, 0);
  state.samples = new audio::sample[blocksize * MAX_FRAMES];

  glGenTextures(1, &state.texture);
  engine.registers->write(state.outlet, &state.texture);
}

void SndTexObject::update(Engine& engine) {
  auto blocksize = engine.config->blocksize;
  auto framesize = blocksize * sizeof(audio::sample);
  auto available = engine.ring_buffers->available(state.name);

  if (available >= MAX_FRAMES) {
    available = MAX_FRAMES - 1;
  }

  for (auto i = 0; i < available; ++i) {
    auto buffer = engine.ring_buffers->read(state.name, i);
    auto dest = &state.samples[i * blocksize];
    std::memcpy(dest, buffer, framesize);
  }

  glBindTexture(GL_TEXTURE_2D, state.texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, 1, blocksize * available, 0, GL_RED,
               GL_FLOAT, state.samples);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}

HANS_PLUGIN_INIT(LibraryManager* library) {
  library->add_object<SndTexState, SndTexObject>("gfx-sndtex");
}
