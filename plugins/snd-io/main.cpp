#include "hans/engine/object.hpp"

#define IO_MAX_CHANNELS 8
#define IO_ARG_CHANNEL 0x69de5123fa4a72cb /* channel */
#define IO_ARG_BUS 0xe327f17e3594b6fd     /* bus */

using namespace hans;
using namespace hans::engine;

struct IOState {
  uint16_t bus;
  uint8_t channels_len;
  uint8_t channels[IO_MAX_CHANNELS];
  Register registers[IO_MAX_CHANNELS];

  template <class Archive>
  void serialize(Archive& ar) {
    ar(bus, channels_len, channels);
  }
};

class InObject : protected AudioObject {
  friend class hans::engine::PluginManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void update(Engine& engine) override {
  }
  virtual void callback(Engine& engine) override;

 private:
  IOState state;
};

class OutObject : protected AudioObject {
  friend class hans::engine::PluginManager;

 public:
  using AudioObject::AudioObject;
  virtual void create(IPatcher& patcher) override;
  virtual void setup(Engine& engine) override;
  virtual void update(Engine& engine) override {
  }
  virtual void callback(Engine& engine) override;

 private:
  IOState state;
};

static void parse_args(IPatcher& patcher, IOState& state) {
  state.channels_len = 0;

  for (const auto& arg : patcher.arguments()) {
    if (arg.name == IO_ARG_BUS && arg.type == Argument::Types::NUMBER) {
      state.bus = arg.number;
    } else if (arg.name == IO_ARG_CHANNEL &&
               arg.type == Argument::Types::NUMBER &&
               state.channels_len != IO_MAX_CHANNELS) {
      state.channels[state.channels_len] = arg.number;
      state.channels_len++;
    }
  }
}

void InObject::create(IPatcher& patcher) {
  parse_args(patcher, state);
  patcher.request(IPatcher::Resources::OUTLET, state.channels_len);
}

void InObject::setup(Engine& engine) {
  for (auto i = 0; i < state.channels_len; ++i) {
    state.registers[i] = engine.registers.make(id, Register::Types::OUTLET, i);
  }
}

void InObject::callback(Engine& engine) {
  // Read from audio bus and write to outlets
  for (auto i = 0; i < state.channels_len; ++i) {
    auto samples = engine.audio_buses.read(state.bus, state.channels[i]);
    engine.registers.write(state.registers[i], samples);
  }
}

void OutObject::create(IPatcher& patcher) {
  parse_args(patcher, state);
  patcher.request(IPatcher::Resources::INLET, state.channels_len);
}

void OutObject::setup(Engine& engine) {
  for (auto i = 0; i < state.channels_len; ++i) {
    state.registers[i] = engine.registers.make(id, Register::Types::INLET, i);
  }
}

void OutObject::callback(Engine& engine) {
  // Read from inlets and write to audio bus
  for (auto i = 0; i < state.channels_len; ++i) {
    const auto& inlet = state.registers[i];
    auto samples = static_cast<audio::sample*>(engine.registers.read(inlet));
    engine.audio_buses.write(state.bus, state.channels[i], samples);
  }
}

HANS_PLUGIN_INIT(PluginManager* manager) {
  manager->add_object<IOState, InObject>("snd-in");
  manager->add_object<IOState, OutObject>("snd-out");
}
