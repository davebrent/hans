#include "hans/engine/ProgramManager.hpp"
#include <cassert>
#include <cstring>

using namespace hans;

engine::ProgramManager::ProgramManager() : m_allocator(0) {
}

void engine::ProgramManager::use(hans_object_api& api,
                                 common::ListView<hans_object>& objects,
                                 common::ListView<hans_program>& programs,
                                 common::ListView<size_t>& chains,
                                 hans_blob init_object_data) {
  m_api = &api;
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

template <typename T>
static void call_setup(const hans_object& object, hans_object_api* api) {
  T* instance = static_cast<T*>(object.instance);
  instance->id = object.id;
  if (instance->setup != nullptr) {
    instance->setup(instance, api);
  }
}

void engine::ProgramManager::setup_all() {
  auto objects = m_objects;
  size_t internal_data_size = 0;
  size_t external_data_size = 0;

  for (auto i = 0; i < m_num_objects; ++i) {
    auto& object = objects[i];
    internal_data_size += object.size;
    switch (object.type) {
    case HANS_OBJECT_AUDIO:
      external_data_size += sizeof(hans_audio_object);
      break;
    case HANS_OBJECT_GRAPHICS:
      external_data_size += sizeof(hans_graphics_object);
      break;
    }
  }

  m_allocator.reset(internal_data_size + external_data_size);

  // Copy over inital state
  assert(m_init_object_data.size == internal_data_size);
  auto internal_data = m_allocator.allocate(internal_data_size);
  std::memcpy(internal_data, m_init_object_data.data, internal_data_size);

  // Create data for each object instance
  auto internal_base = static_cast<char*>(m_allocator.start());
  for (auto i = 0; i < m_num_objects; ++i) {
    auto& object = objects[i];

    switch (object.type) {
    case HANS_OBJECT_AUDIO: {
      object.instance = m_allocator.allocate(sizeof(hans_audio_object));
      auto instance = static_cast<hans_audio_object*>(object.instance);
      instance->data = internal_base;
      break;
    }
    case HANS_OBJECT_GRAPHICS: {
      object.instance = m_allocator.allocate(sizeof(hans_graphics_object));
      auto instance = static_cast<hans_graphics_object*>(object.instance);
      instance->data = internal_base;
      break;
    }
    }

    internal_base += object.size;

    // Patches the instance's function pointers
    if (object.init != nullptr) {
      object.init(object.instance);
    }

    switch (object.type) {
    case HANS_OBJECT_AUDIO:
      call_setup<hans_audio_object>(object, m_api);
      break;
    case HANS_OBJECT_GRAPHICS:
      call_setup<hans_graphics_object>(object, m_api);
      break;
    }
  }
}

void engine::ProgramManager::close_all() {
  auto objects = m_objects;
  for (auto i = 0; i < m_num_objects; ++i) {
    auto& object = objects[i];

    if (object.destroy == nullptr) {
      continue;
    }

    switch (object.type) {
    case HANS_OBJECT_AUDIO:
      object.destroy(static_cast<hans_audio_object*>(object.instance));
      break;
    case HANS_OBJECT_GRAPHICS:
      object.destroy(static_cast<hans_graphics_object*>(object.instance));
      break;
    }
  }
}

void engine::ProgramManager::tick_graphics(float delta) {
  auto program = m_programs[m_active];
  auto chain = program.graphics;
  auto objects = m_objects;
  auto chains = m_chains;
  auto num_objects = m_num_objects;

  for (auto i = chain.start; i < chain.end; ++i) {
    auto instance_id = chains[i];
    for (auto k = 0; k < num_objects; ++k) {
      auto& object = objects[k];
      if (object.id == instance_id) {
        auto instance = static_cast<hans_graphics_object*>(object.instance);
        if (instance->update != nullptr) {
          instance->update(instance, m_api);
        }
        break;
      }
    }
  }

  for (auto i = chain.start; i < chain.end; ++i) {
    auto instance_id = chains[i];
    for (auto k = 0; k < num_objects; ++k) {
      auto& object = objects[k];
      if (object.id == instance_id) {
        auto instance = static_cast<hans_graphics_object*>(object.instance);
        if (instance->draw != nullptr) {
          instance->draw(instance, m_api);
        }
        break;
      }
    }
  }
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
        auto instance = static_cast<hans_audio_object*>(object.instance);
        instance->callback(instance, m_api);
        break;
      }
    }
  }
}
