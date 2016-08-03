#include <cstring>
#include "hans/engine/object.hpp"

using namespace hans;

#define GFX_RB_MAX_FRAMES 30
#define GFX_RB_ARG_NAME 0xd4c943cba60c270b /* name */

struct SndTexState {
  GLuint texture;
  hans_hash name;
  hans_register outlet;
  uint32_t texture_value;
  hans_audio_sample* samples = nullptr;
};

class SndTexObject : protected GraphicsObject {
  friend class engine::LibraryManager;

 public:
  using GraphicsObject::GraphicsObject;
  ~SndTexObject();
  virtual void create(ObjectPatcher& patcher) override;
  virtual void setup(hans_object_api& api) override;
  virtual void update(hans_object_api& api) override;
  virtual void draw(hans_object_api& api) override {
  }

 private:
  SndTexState state;
};

SndTexObject::~SndTexObject() {
  delete[] state.samples;
}

void SndTexObject::create(ObjectPatcher& patcher) {
  auto args = patcher.get_args();
  for (auto i = 0; i < args.length; ++i) {
    const auto& arg = args.data[i];
    if (arg.name == GFX_RB_ARG_NAME && arg.type == HANS_STRING) {
      state.name = arg.string;
    }
  }

  patcher.request(HANS_OUTLET, 1);
}

void SndTexObject::setup(hans_object_api& api) {
  auto blocksize = api.config->blocksize;

  state.outlet = api.registers->make(id, HANS_OUTLET, 0);
  state.samples = new hans_audio_sample[blocksize * GFX_RB_MAX_FRAMES];

  glGenTextures(1, &state.texture);
  api.registers->write(state.outlet, &state.texture);
}

void SndTexObject::update(hans_object_api& api) {
  auto blocksize = api.config->blocksize;
  auto framesize = blocksize * sizeof(hans_audio_sample);
  auto available = api.ring_buffers->available(state.name);

  if (available >= GFX_RB_MAX_FRAMES) {
    available = GFX_RB_MAX_FRAMES - 1;
  }

  for (auto i = 0; i < available; ++i) {
    auto buffer = api.ring_buffers->read(state.name, i);
    auto dest = &state.samples[i * blocksize];
    std::memcpy(dest, buffer, framesize);
  }

  glBindTexture(GL_TEXTURE_2D, state.texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, 1, blocksize * available, 0, GL_RED,
               GL_FLOAT, state.samples);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
}

extern "C" {
void setup(engine::LibraryManager* library) {
  library->add_object<SndTexState, SndTexObject>("gfx-sndtex");
}
}
