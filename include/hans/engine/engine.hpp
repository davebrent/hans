#ifndef HANS_ENGINE_ENGINE_H
#define HANS_ENGINE_ENGINE_H

#include <functional>
#include "hans/engine/audio_backend_base.hpp"
#include "hans/engine/context.hpp"
#include "hans/engine/modulation.hpp"
#include "hans/engine/object.hpp"
#include "hans/engine/primitives.hpp"
#include "hans/engine/replay.hpp"
#include "hans/engine/window.hpp"

namespace hans {
namespace engine {

class Engine {
 public:
  Engine(const Engine& other) = delete;
  Engine(EngineData data);

  bool set_program(size_t index);
  bool set_parameter(ObjectDef::ID object, const hash name,
                     const Parameter::Length component,
                     const Parameter::Value value);
  bool setup();
  void run_forever();
  void run_forever(std::function<bool()> callback);
  void tick_graphics();
  void tick_audio();
  bool destroy();
  bool capture(Frame& frame);
  bool record_start();
  bool record_stop();
  bool player_start();
  bool player_set(size_t frameno);
  bool player_stop();

 private:
  EngineData m_data;
  context m_ctx;
  Window m_window;
  PluginManager m_plugins;
  ModulationManager m_modulators;
  ReplayRecorder m_recorder;
  ReplayPlayer m_player;
  AudioBackendBase* m_stream;
  std::vector<GraphicsObject*> m_graphics_objects;
  std::vector<AudioObject*> m_audio_objects;

  size_t m_selected_program;

  template <typename T>
  void construct(std::vector<T>& output, Graphs& input) {
    std::vector<T> table;
    table.reserve(input.objects.size());
    output.reserve(input.indices.size());

    for (auto i = 0; i < input.objects.size(); ++i) {
      auto& object = input.objects.at(i);
      auto& state = input.states.at(i);
      auto instance = m_plugins.construct(object.name, object.id, state);
      instance->setup(m_ctx);
      table.push_back(static_cast<T>(instance));
    }

    for (const auto index : input.indices) {
      output.push_back(table.at(index));
    }
  }

  template <typename T>
  void destruct(std::vector<T>& instances, Graphs& graph) {
    std::vector<T> table;
    table.reserve(graph.objects.size());

    for (const auto instance : instances) {
      if (std::find(table.begin(), table.end(), instance) == table.end()) {
        table.push_back(instance);
      }
    }

    for (auto i = 0; i < graph.objects.size(); ++i) {
      auto& object = graph.objects.at(i);
      m_plugins.destruct(object.name, table.at(i));
    }

    instances.clear();
  }
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_ENGINE_H
