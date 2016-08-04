#include "hans/engine/object.hpp"

#define UTILS_ARG_PAN 0x3d9fff7097514989
#define UTILS_ARG_GAIN 0xf98f299598722871

using namespace hans;
using namespace hans::engine;

struct GainState {
  float gain;
};

class GainObject : protected AudioObject {
  friend class LibraryManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void callback(Engine& engine) override;

 private:
  GainState state;
};

void GainObject::create(IPatcher& patcher) {
  patcher.request(IPatcher::Resources::INLET, 1);
  patcher.request(IPatcher::Resources::OUTLET, 1);

  for (const auto& arg : patcher.arguments()) {
    if (arg.name == UTILS_ARG_GAIN && arg.type == Argument::Types::NUMBER) {
      state.gain = arg.number;
    }
  }
}

void GainObject::setup(Engine& engine) {
  state.gain = 1;
}

void GainObject::callback(Engine& engine) {
  // TODO: Implement
}

extern "C" {
void setup(LibraryManager* library) {
  library->add_object<GainState, GainObject>("snd-gain");
}
}
