#include "hans/engine.hpp"
#include <string>
#include "./audio_backend_jack.hpp"
#include "./audio_backend_portaudio.hpp"
#include "hans/audio_buffers.hpp"
#include "hans/audio_buses.hpp"
#include "hans/modulation.hpp"
#include "hans/object.hpp"
#include "hans/parameters.hpp"
#include "hans/plugins.hpp"
#include "hans/registers.hpp"
#include "hans/replay.hpp"
#include "hans/ring_buffers.hpp"
#include "hans/strings.hpp"

using namespace hans;

Engine::Engine(EngineData ng, AudioBuses& audio_buses)
    : m_data(ng),
      m_ctx(m_data, audio_buses),
      m_plugins(m_ctx.strings, m_data.plugins),
      m_modulators(m_data.modulators, m_data.parameters),
      m_recorder(m_data.parameters.buffer, m_data.recordings),
      m_player(m_data.parameters.buffer, m_data.recordings),
      m_debug(m_ctx.strings, true),
      m_should_stop(false) {
  m_selected_program = 0;
  m_debug.push("setup");
  m_ctx.fbos.setup();
  construct<AudioObject*>(m_audio_objects, m_data.programs.audio);
  construct<GraphicsObject*>(m_graphics_objects, m_data.programs.graphics);
  m_debug.pop();
};

Engine::~Engine() {
  destruct<AudioObject*>(m_audio_objects, m_data.programs.audio);
  destruct<GraphicsObject*>(m_graphics_objects, m_data.programs.graphics);
  m_ctx.fbos.destroy();
  m_ctx.shaders.destroy();
}

const EngineData& Engine::data() {
  return m_data;
}

bool Engine::set_program(size_t index) {
  m_selected_program = index;
  return true;
}

void Engine::set_parameter(ObjectDef::ID object, const hash name,
                           const Parameter::Length component,
                           const Parameter::Value value) {
  m_ctx.parameters.set(object, name, component, value);
}

void Engine::tick_audio() {
  auto range = m_data.programs.audio.ranges.at(m_selected_program);

  for (auto i = range.start; i < range.end; ++i) {
    m_audio_objects.at(i)->update(m_ctx);
  }

  m_modulators.snd_modulate();

  for (auto i = range.start; i < range.end; ++i) {
    m_audio_objects.at(i)->callback(m_ctx);
  }

  m_modulators.snd_restore();
}

void Engine::tick_graphics() {
  m_ctx.parameters.update();
  auto range = m_data.programs.graphics.ranges.at(m_selected_program);

  m_debug.push("update");
  for (auto i = range.start; i < range.end; ++i) {
    auto& object = m_graphics_objects.at(i);
    m_debug.push(std::to_string(i).c_str());
    object->update(m_ctx);
    m_debug.pop();
  }
  m_debug.pop();

  // m_modulators.gfx_modulate();

  m_player.tick();
  m_recorder.tick();
  m_ctx.ring_buffers.advance_all();

  m_debug.push("draw");
  for (auto i = range.start; i < range.end; ++i) {
    auto& object = m_graphics_objects.at(i);
    m_debug.push(std::to_string(i).c_str());
    object->draw(m_ctx);
    m_debug.pop();
  }
  m_debug.pop();

  // m_modulators.gfx_restore();
}

bool Engine::record_start() {
  m_recorder.start();
  return true;
}

bool Engine::record_stop() {
  m_recorder.stop();
  return true;
}

bool Engine::player_start() {
  m_player.start();
  return true;
}

bool Engine::player_set(size_t frameno) {
  m_player.set(frameno);
  return true;
}

bool Engine::player_stop() {
  m_player.stop();
  return true;
}
