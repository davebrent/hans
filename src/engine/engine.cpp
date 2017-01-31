#include "hans/engine/engine.hpp"
#include <algorithm>
#include "./audio_backend_jack.hpp"
#include "./audio_backend_portaudio.hpp"
#include "hans/engine/audio_buffers.hpp"
#include "hans/engine/audio_buses.hpp"
#include "hans/engine/modulation.hpp"
#include "hans/engine/object.hpp"
#include "hans/engine/parameters.hpp"
#include "hans/engine/plugins.hpp"
#include "hans/engine/registers.hpp"
#include "hans/engine/replay.hpp"
#include "hans/engine/ring_buffers.hpp"
#include "hans/engine/strings.hpp"
#include "hans/engine/window.hpp"

using namespace hans;
using namespace hans::engine;

Engine::Engine(EngineData ng)
    : m_data(ng),
      m_ctx(m_data),
      m_window(),
      m_plugins(m_ctx.strings, m_data.plugins),
      m_modulators(m_data.modulators, m_data.parameters),
      m_recorder(m_data.parameters.buffer, m_data.recordings),
      m_player(m_data.parameters.buffer, m_data.recordings) {
  m_stream = nullptr;
  m_selected_program = 0;
};

bool Engine::set_program(size_t index) {
  m_selected_program = index;
  return true;
}

bool Engine::set_parameter(ObjectDef::ID object, const hash name,
                           const Parameter::Length component,
                           const Parameter::Value value) {
  // FIXME: This needs to be double buffered, then swappend at begin frame
  return m_ctx.parameters.set(object, name, component, value);
}

bool Engine::setup() {
  if (!m_window.make("Hans", m_ctx.settings.width, m_ctx.settings.height)) {
    std::cerr << "[HANS] Unable to open window" << std::endl;
    return false;
  }

  m_ctx.fbos.setup();

  construct<AudioObject*>(m_audio_objects, m_data.programs.audio);
  construct<GraphicsObject*>(m_graphics_objects, m_data.programs.graphics);

  m_stream = new AudioBackendPortAudio(m_ctx.settings, m_ctx.audio_buses,
                                       [&]() { tick_audio(); });
  if (!m_stream->open()) {
    std::cerr << "[HANS] Unable to open audio stream" << std::endl;
    return false;
  }

  m_stream->start();
  return true;
}

void Engine::run_forever() {
  while (!m_window.should_close()) {
    tick_graphics();
  }
}

void Engine::run_forever(std::function<bool()> callback) {
  while (!m_window.should_close()) {
    if (!callback()) {
      break;
    }
    tick_graphics();
  }
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
  auto range = m_data.programs.graphics.ranges.at(m_selected_program);

  for (auto i = range.start; i < range.end; ++i) {
    m_graphics_objects.at(i)->update(m_ctx);
  }

  m_modulators.gfx_modulate();

  m_player.tick();
  m_recorder.tick();
  m_ctx.ring_buffers.advance_all();

  for (auto i = range.start; i < range.end; ++i) {
    m_graphics_objects.at(i)->draw(m_ctx);
  }

  m_modulators.gfx_restore();
  m_window.update();
}

bool Engine::destroy() {
  if (m_stream != nullptr) {
    m_stream->close();
    delete m_stream;
    m_stream = nullptr;
  }

  destruct<AudioObject*>(m_audio_objects, m_data.programs.audio);
  destruct<GraphicsObject*>(m_graphics_objects, m_data.programs.graphics);

  m_ctx.fbos.destroy();
  m_ctx.shaders.destroy();
  return true;
}

bool Engine::capture(Frame& frame) {
  m_window.capture(frame);
  return true;
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
