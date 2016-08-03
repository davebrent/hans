#include "hans/engine/ProgramManager.hpp"
#include <cassert>
#include <cstring>

using namespace hans;

engine::ProgramManager::ProgramManager(hans_object_api& api)
    : m_api(api), m_allocator(0) {
}

void engine::ProgramManager::use(common::ListView<hans_object>& objects,
                                 common::ListView<hans_program>& programs,
                                 common::ListView<size_t>& chains,
                                 hans_blob init_object_data) {
  m_objects = &objects[0];
  m_chains = &chains[0];
  m_num_objects = objects.size();
  m_programs = &programs[0];
  m_init_object_data = init_object_data;
}

void engine::ProgramManager::switch_to(hans_hash name) {
  m_active = 0;

  if (name == 0) {
    return;
  }

  while (m_programs[m_active].name != name) {
    m_active++;
  }
}

void engine::ProgramManager::setup_all() {
  auto objects = m_objects;
  size_t internal_data_size = 0;

  for (auto i = 0; i < m_num_objects; ++i) {
    auto& object = objects[i];
    internal_data_size += object.size;
  }

  m_allocator.reset(internal_data_size);

  // Copy over inital state
  assert(m_init_object_data.size == internal_data_size);
  auto internal_data = m_allocator.allocate(internal_data_size);
  std::memcpy(internal_data, m_init_object_data.data, internal_data_size);

  // Create data for each object instance
  auto internal_base = static_cast<char*>(m_allocator.start());
  for (auto i = 0; i < m_num_objects; ++i) {
    auto& object = objects[i];
    auto instance = static_cast<Object*>(object.make(object.id, internal_base));
    instance->setup(m_api);

    object.instance = instance;
    internal_base += object.size;
  }
}

void engine::ProgramManager::close_all() {
  auto objects = m_objects;
  for (auto i = 0; i < m_num_objects; ++i) {
    auto& object = objects[i];
    object.destroy(object.instance);
  }
}

void engine::ProgramManager::tick_graphics(float delta) {
  auto program = m_programs[m_active];
  auto chain = program.graphics;
  auto objects = m_objects;
  auto chains = m_chains;
  auto num_objects = m_num_objects;

  m_api.modulators->begin();

  for (auto i = chain.start; i < chain.end; ++i) {
    auto instance_id = chains[i];
    for (auto k = 0; k < num_objects; ++k) {
      auto& object = objects[k];
      if (object.id == instance_id) {
        auto instance = static_cast<GraphicsObject*>(object.instance);
        instance->update(m_api);
        break;
      }
    }
  }

  for (auto i = chain.start; i < chain.end; ++i) {
    auto instance_id = chains[i];
    for (auto k = 0; k < num_objects; ++k) {
      auto& object = objects[k];
      if (object.id == instance_id) {
        auto instance = static_cast<GraphicsObject*>(object.instance);
        instance->draw(m_api);
        break;
      }
    }
  }

  m_api.modulators->end();
}

void engine::ProgramManager::tick_audio() {
  auto program = m_programs[m_active];
  auto chain = program.audio;
  auto objects = m_objects;
  auto chains = m_chains;
  auto num_objects = m_num_objects;

  for (auto i = chain.start; i < chain.end; ++i) {
    auto instance_id = chains[i];
    for (auto k = 0; k < num_objects; ++k) {
      auto& object = objects[k];
      if (object.id == instance_id) {
        auto instance = static_cast<AudioObject*>(object.instance);
        instance->callback(m_api);
        break;
      }
    }
  }
}
