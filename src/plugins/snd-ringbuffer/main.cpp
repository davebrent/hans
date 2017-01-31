#include "hans/engine/object.hpp"

#define RB_ARG_NAME 0xd4c943cba60c270b /* name */

using namespace hans;
using namespace hans::engine;

struct RingBufferState {
  hash name;
  Register inlet;
  Register outlet;
  RingBuffer ringbuffer;

  template <class Archive>
  void serialize(Archive& ar) {
    ar(name);
  }
};

class RingBufferObject : protected AudioObject {
  friend class hans::engine::PluginManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(Configurator& patcher) override;
  virtual void setup(context& ctx) override;
  virtual void update(context& ctx) override {
  }
  virtual void callback(context& ctx) override;

 private:
  RingBufferState state;
};

void RingBufferObject::create(Configurator& patcher) {
  for (const auto& arg : patcher.arguments()) {
    if (arg.name == RB_ARG_NAME && arg.type == Argument::Types::STRING) {
      state.name = arg.string;
    }
  }

  if (!state.name) {
    patcher.missing("name");
  } else {
    patcher.request(Configurator::Resources::INLET, 1);
    patcher.request(Configurator::Resources::OUTLET, 1);
    patcher.request(Configurator::Resources::RING_BUFFER, state.name);
  }
}

void RingBufferObject::setup(context& ctx) {
  state.inlet = ctx.registers.make(id, Register::Types::INLET, 0);
  state.outlet = ctx.registers.make(id, Register::Types::OUTLET, 0);
  state.ringbuffer = ctx.ring_buffers.make(id, state.name);
}

void RingBufferObject::callback(context& ctx) {
  auto& inlet = state.inlet;
  auto& outlet = state.outlet;
  auto samples = static_cast<audio::sample*>(ctx.registers.read(inlet));

  ctx.ring_buffers.write(state.ringbuffer, samples);
  ctx.registers.write(outlet, samples);
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<RingBufferState, RingBufferObject>("snd-ringbuffer");
}
