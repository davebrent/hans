#ifndef HANS_ENGINE_H
#define HANS_ENGINE_H

#include <atomic>
#include <functional>
#include "hans/audio_backend_base.hpp"
#include "hans/context.hpp"
#include "hans/graphics_debug.hpp"
#include "hans/modulation.hpp"
#include "hans/object.hpp"
#include "hans/primitives.hpp"
#include "hans/replay.hpp"

namespace hans {

class Engine {
 public:
  Engine(const Engine& other) = delete;
  Engine(EngineData data, AudioBuses& buses);
  ~Engine();
  const EngineData& data();
  void set_display(const Display& window);
  bool set_program(size_t index);
  void set_parameter(ObjectDef::ID object, const hash name,
                     const Parameter::Length component,
                     const Parameter::Value value);
  void tick_graphics();
  void tick_audio();
  bool record_start();
  bool record_stop();
  bool player_start();
  bool player_set(size_t frameno);
  bool player_stop();

 private:
  EngineData m_data;
  context m_ctx;
  PluginManager m_plugins;
  ModulationManager m_modulators;
  ReplayRecorder m_recorder;
  ReplayPlayer m_player;
  GraphicsDebug m_debug;
  std::atomic<bool> m_should_stop;
  std::vector<GraphicsObject*> m_graphics_objects;
  std::vector<AudioObject*> m_audio_objects;

  size_t m_selected_program;

  template <typename T>
  void construct(std::vector<T>& output, Graphs& input) {
    output.reserve(input.objects.size());

    for (auto i = 0; i < input.objects.size(); ++i) {
      auto& object = input.objects.at(i);
      auto& state = input.states.at(i);
      auto instance = m_plugins.construct(object.name, object.id, state);
      instance->setup(m_ctx);
      output.push_back(static_cast<T>(instance));
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

} // namespace hans

#endif // HANS_ENGINE_H
