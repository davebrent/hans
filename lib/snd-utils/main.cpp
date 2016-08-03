#include "hans/engine/object.hpp"

#define UTILS_ARG_PAN 0x3d9fff7097514989
#define UTILS_ARG_GAIN 0xf98f299598722871

using namespace hans;

struct GainState {
  float gain;
};

class GainObject : protected AudioObject {
  friend class hans::engine::LibraryManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(ObjectPatcher& patcher) override;
  virtual void setup(hans_object_api& api) override;
  virtual void callback(hans_object_api& api) override;

 private:
  GainState state;
};

void GainObject::create(ObjectPatcher& patcher) {
  patcher.request(HANS_INLET, 1);
  patcher.request(HANS_OUTLET, 1);

  auto args = patcher.get_args();
  for (auto i = 0; i < args.length; ++i) {
    const auto& arg = args.data[i];
    if (arg.name == UTILS_ARG_GAIN && arg.type == HANS_NUMBER) {
      state.gain = arg.number;
    }
  }
}

void GainObject::setup(hans_object_api& api) {
  state.gain = 1;
}

void GainObject::callback(hans_object_api& api) {
  // TODO: Implement
}

extern "C" {
void setup(engine::LibraryManager* library) {
  library->add_object<GainState, GainObject>("snd-gain");
}
}
