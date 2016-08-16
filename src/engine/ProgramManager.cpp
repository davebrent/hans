#include "hans/engine/ProgramManager.hpp"
#include <cassert>
#include <cstring>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

ProgramManager::ProgramManager(Engine& engine) : m_engine(engine) {
}

void ProgramManager::use(ListView<ObjectDef> objects,
                         ListView<Program> programs, ListView<size_t> chains,
                         DataFile::Blob state) {
  m_objects = objects;
  m_chains = chains;
  m_programs = programs;
  m_default_state = state;

  size_t state_size = 0;
  for (const auto& object : m_objects) {
    state_size += object.size;
  }
  assert(m_default_state.size == state_size);
}

void ProgramManager::switch_to(hash name) {
  m_active = 0;

  if (name == 0) {
    return;
  }

  while (m_programs[m_active].name != name) {
    m_active++;
  }
}

void ProgramManager::setup_all() {
  auto state = m_default_state.data;
  for (auto& object : m_objects) {
    object.instance = object.create(object.id, state);
    state = static_cast<void*>(static_cast<char*>(state) + object.size);
    static_cast<Object*>(object.instance)->setup(m_engine);
  }
}

void ProgramManager::close_all() {
  for (const auto& object : m_objects) {
    object.destroy(object.instance);
  }
}

void ProgramManager::tick_graphics(float delta) {
  const auto& program = m_programs[m_active];
  const auto& chain = program.graphics;

  m_engine.modulators->begin();

  for (auto i = chain.start; i < chain.end; ++i) {
    auto instance_id = m_chains[i];
    for (const auto& object : m_objects) {
      if (object.id == instance_id) {
        auto instance = static_cast<GraphicsObject*>(object.instance);
        instance->update(m_engine);
        break;
      }
    }
  }

  for (auto i = chain.start; i < chain.end; ++i) {
    auto instance_id = m_chains[i];
    for (const auto& object : m_objects) {
      if (object.id == instance_id) {
        auto instance = static_cast<GraphicsObject*>(object.instance);
        instance->draw(m_engine);
        break;
      }
    }
  }

  m_engine.modulators->end();
}

void ProgramManager::tick_audio() {
  const auto& program = m_programs[m_active];
  const auto& chain = program.audio;

  for (auto i = chain.start; i < chain.end; ++i) {
    auto instance_id = m_chains[i];
    for (const auto& object : m_objects) {
      if (object.id == instance_id) {
        auto instance = static_cast<AudioObject*>(object.instance);
        instance->callback(m_engine);
        break;
      }
    }
  }
}
