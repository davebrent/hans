#include "hans/engine/object.hpp"

#define IO_MAX_CHANNELS 8
#define IO_ARG_CHANNEL 0x69de5123fa4a72cb /* channel */
#define IO_ARG_BUS 0xe327f17e3594b6fd     /* bus */

using namespace hans;

struct IOState {
  uint16_t bus;
  uint8_t channels_len;
  uint8_t channels[IO_MAX_CHANNELS];
  hans_register registers[IO_MAX_CHANNELS];
};

class InObject : protected AudioObject {
  friend class hans::engine::LibraryManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(ObjectPatcher& patcher) override;
  virtual void setup(hans_object_api& api) override;
  virtual void callback(hans_object_api& api) override;

 private:
  IOState state;
};

class OutObject : protected AudioObject {
  friend class hans::engine::LibraryManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(ObjectPatcher& patcher) override;
  virtual void setup(hans_object_api& api) override;
  virtual void callback(hans_object_api& api) override;

 private:
  IOState state;
};

static void parse_args(ObjectPatcher& patcher, IOState& state) {
  auto args = patcher.get_args();

  state.channels_len = 0;

  for (int i = 0; i < args.length; ++i) {
    const auto& arg = args.data[i];
    if (arg.name == IO_ARG_BUS && arg.type == HANS_NUMBER) {
      state.bus = arg.number;
    } else if (arg.name == IO_ARG_CHANNEL && arg.type == HANS_NUMBER &&
               state.channels_len != IO_MAX_CHANNELS) {
      state.channels[state.channels_len] = args.data[i].number;
      state.channels_len++;
    }
  }
}

void InObject::create(ObjectPatcher& patcher) {
  parse_args(patcher, state);
  patcher.request(HANS_OUTLET, state.channels_len);
}

void InObject::setup(hans_object_api& api) {
  for (auto i = 0; i < state.channels_len; ++i) {
    state.registers[i] = api.registers->make(id, HANS_OUTLET, i);
  }
}

void InObject::callback(hans_object_api& api) {
  // Read from audio bus and write to outlets
  for (auto i = 0; i < state.channels_len; ++i) {
    auto samples = api.audio_buses->read(state.bus, state.channels[i]);
    api.registers->write(state.registers[i], samples);
  }
}

void OutObject::create(ObjectPatcher& patcher) {
  parse_args(patcher, state);
  patcher.request(HANS_INLET, state.channels_len);
}

void OutObject::setup(hans_object_api& api) {
  for (auto i = 0; i < state.channels_len; ++i) {
    state.registers[i] = api.registers->make(id, HANS_INLET, i);
  }
}

void OutObject::callback(hans_object_api& api) {
  // Read from inlets and write to audio bus
  for (auto i = 0; i < state.channels_len; ++i) {
    const auto& inlet = state.registers[i];
    auto samples = static_cast<hans_audio_sample*>(api.registers->read(inlet));
    api.audio_buses->write(state.bus, state.channels[i], samples);
  }
}

extern "C" {
void setup(engine::LibraryManager* library) {
  library->add_object<IOState, InObject>("snd-in");
  library->add_object<IOState, OutObject>("snd-out");
}
}
