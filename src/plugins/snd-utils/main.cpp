#include "hans/engine/object.hpp"

#define UTILS_ARG_PAN 0x3d9fff7097514989
#define UTILS_ARG_GAIN 0xf98f299598722871

using namespace hans;
using namespace hans::engine;

struct GainState {
  float gain;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(gain);
  }
};

class GainObject : protected AudioObject {
  friend class hans::engine::PluginManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(Configurator& patcher) override;
  virtual void setup(context& ctx) override;
  virtual void update(context& ctx) override {
  }
  virtual void callback(context& ctx) override;

 private:
  GainState state;
};

void GainObject::create(Configurator& patcher) {
  patcher.request(Configurator::Resources::INLET, 1);
  patcher.request(Configurator::Resources::OUTLET, 1);

  for (const auto& arg : patcher.arguments()) {
    if (arg.name == UTILS_ARG_GAIN && arg.type == Argument::Types::NUMBER) {
      state.gain = arg.number;
    }
  }
}

void GainObject::setup(context& ctx) {
  state.gain = 1;
}

void GainObject::callback(context& ctx) {
  // TODO: Implement
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<GainState, GainObject>("snd-gain");
}
