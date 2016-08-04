#include "hans/engine/object.hpp"

#define RB_ARG_NAME 0xd4c943cba60c270b /* name */

using namespace hans;
using namespace hans::audio;
using namespace hans::engine;

struct RingBufferState {
  hash name;
  Register inlet;
  Register outlet;
  RingBuffer ringbuffer;
};

class RingBufferObject : protected AudioObject {
  friend class LibraryManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void callback(Engine& engine) override;

 private:
  RingBufferState state;
};

void RingBufferObject::create(IPatcher& patcher) {
  for (const auto& arg : patcher.arguments()) {
    if (arg.name == RB_ARG_NAME && arg.type == Argument::Types::STRING) {
      state.name = arg.string;
    }
  }

  patcher.request(IPatcher::Resources::INLET, 1);
  patcher.request(IPatcher::Resources::OUTLET, 1);
  patcher.request(IPatcher::Resources::RING_BUFFER, state.name);
}

void RingBufferObject::setup(Engine& engine) {
  state.inlet = engine.registers->make(id, Register::Types::INLET, 0);
  state.outlet = engine.registers->make(id, Register::Types::OUTLET, 0);
  state.ringbuffer = engine.ring_buffers->make(id, state.name);
}

void RingBufferObject::callback(Engine& engine) {
  auto& inlet = state.inlet;
  auto& outlet = state.outlet;
  auto samples = static_cast<audio::sample*>(engine.registers->read(inlet));

  engine.ring_buffers->write(state.ringbuffer, samples);
  engine.registers->write(outlet, samples);
}

extern "C" {
void setup(LibraryManager* library) {
  library->add_object<RingBufferState, RingBufferObject>("snd-ringbuffer");
}
}
