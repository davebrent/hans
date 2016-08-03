#include "hans/engine/object.hpp"

#define RB_ARG_NAME 0xd4c943cba60c270b /* name */

using namespace hans;

struct RingBufferState {
  hans_hash name;
  hans_register inlet;
  hans_register outlet;
  hans_ring_buffer ringbuffer;
};

class RingBufferObject : protected AudioObject {
  friend class hans::engine::LibraryManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(ObjectPatcher& patcher) override;
  virtual void setup(hans_object_api& api) override;
  virtual void callback(hans_object_api& api) override;

 private:
  RingBufferState state;
};

void RingBufferObject::create(ObjectPatcher& patcher) {
  auto args = patcher.get_args();
  for (auto i = 0; i < args.length; ++i) {
    const auto& arg = args.data[i];
    if (arg.name == RB_ARG_NAME && arg.type == HANS_STRING) {
      state.name = arg.string;
    }
  }

  patcher.request(HANS_INLET, 1);
  patcher.request(HANS_OUTLET, 1);
  patcher.request(HANS_RING_BUFFER, state.name);
}

void RingBufferObject::setup(hans_object_api& api) {
  state.inlet = api.registers->make(id, HANS_INLET, 0);
  state.outlet = api.registers->make(id, HANS_OUTLET, 0);
  state.ringbuffer = api.ring_buffers->make(id, state.name);
}

void RingBufferObject::callback(hans_object_api& api) {
  auto& inlet = state.inlet;
  auto& outlet = state.outlet;
  auto samples = static_cast<hans_audio_sample*>(api.registers->read(inlet));

  api.ring_buffers->write(state.ringbuffer, samples);
  api.registers->write(outlet, samples);
}

extern "C" {
void setup(engine::LibraryManager* library) {
  library->add_object<RingBufferState, RingBufferObject>("snd-ringbuffer");
}
}
