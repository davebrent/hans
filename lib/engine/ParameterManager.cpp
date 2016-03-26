#include "hans/engine/ParameterManager.hpp"
#include <cassert>
#include <cstdlib>
#include <cstring>

using namespace hans;

struct container_size {
  size_t container;
  size_t values;
  size_t names;
  size_t instances;
  size_t sizes;
  size_t total;
};

static void calc_size(struct container_size& s, int capacity) {
  s.container = sizeof(engine::ParameterManager::data);
  s.values = sizeof(hans_parameter_value) * capacity;
  s.names = sizeof(hans_hash) * capacity;
  s.instances = sizeof(hans_instance_id) * capacity;
  s.sizes = sizeof(hans_parameter_size) * capacity;
  s.total = s.container + s.names + s.instances + s.sizes + s.values +
            1 /* padding */;
}

static void patch(engine::ParameterManager::data* data, void* buffer) {
  struct container_size size;
  calc_size(size, data->capacity);

  char* bytes = reinterpret_cast<char*>(buffer);
  char* o1 = bytes + size.container;
  char* o2 = bytes + size.container + size.names;
  char* o3 = bytes + size.container + size.names + size.instances;
  char* o4 = bytes + size.container + size.names + size.instances + size.sizes;

  data->names = reinterpret_cast<hans_hash*>(o1);
  data->instances = reinterpret_cast<hans_instance_id*>(o2);
  data->sizes = reinterpret_cast<hans_parameter_size*>(o3);
  data->values = reinterpret_cast<hans_parameter_value*>(o4 + 1 /* padding */);
}

engine::ParameterManager::ParameterManager(std::vector<hans_parameter> defaults)
    : m_parameters(defaults) {
  m_blob.bytes = 0;
  m_blob.data = nullptr;
  m_data = nullptr;
}

engine::ParameterManager::~ParameterManager() {
  std::free(m_blob.data);
}

void engine::ParameterManager::set_capacity(unsigned capacity) {
  std::free(m_blob.data);

  struct container_size size;
  calc_size(size, capacity);

  m_blob.bytes = size.total;
  m_blob.data = std::calloc(1, m_blob.bytes);

  m_data = reinterpret_cast<engine::ParameterManager::data*>(m_blob.data);
  m_data->capacity = capacity;

  patch(m_data, m_blob.data);
}

uint16_t engine::ParameterManager::set_objects(
    const std::vector<hans_object_id>& object_ids) {
  int size = 0;
  uint16_t num_handles = 0;

  for (const auto& parameter : m_parameters) {
    for (const auto& id : object_ids) {
      if (parameter.object_id == id) {
        size += parameter.size;
        num_handles++;
      }
    }
  }

  set_capacity(size);
  return num_handles;
}

void engine::ParameterManager::clear() {
  auto capacity = m_data->capacity;
  std::memset(m_blob.data, 0, m_blob.bytes);
  m_data->capacity = capacity;
  patch(m_data, m_blob.data);
}

int engine::ParameterManager::make(hans_object_resource* resources,
                                   const hans_object_id object_id,
                                   const hans_instance_id instance_id) {
  int created = 0;

  for (const auto& parameter : m_parameters) {
    if (parameter.object_id != object_id) {
      continue;
    }

    resources->type = HANS_PARAMETER;
    resources->name = parameter.name;

    hans_parameter_handle handle = {.value = m_data->size,
                                    .meta = m_data->count};

    m_data->names[m_data->count] = parameter.name;
    m_data->sizes[m_data->count] = parameter.size;
    m_data->instances[m_data->count] = instance_id;
    m_data->count++;
    m_data->size += parameter.size;
    assert(m_data->size <= m_data->capacity);

    if (parameter.values != nullptr) {
      for (int i = 0; i < parameter.size; ++i) {
        set(handle, i, parameter.values[i]);
      }
    }

    resources->parameter = handle;
    resources++;
    created++;
  }

  return created;
}

void engine::ParameterManager::set(const hans_parameter_handle& parameter,
                                   const hans_parameter_size& component,
                                   const hans_parameter_value& value) {
  assert(component < m_data->sizes[parameter.meta]);
  m_data->values[parameter.value + component] = value;
}

hans_parameter_value engine::ParameterManager::get(
    const hans_parameter_handle& parameter,
    const hans_parameter_size& component) const {
  assert(component < m_data->sizes[parameter.meta]);
  return m_data->values[parameter.value + component];
}

void engine::ParameterManager::copy(const engine::ParameterManager& src) {
  assert(m_blob.bytes == src.m_blob.bytes);
  std::memcpy(m_blob.data, src.m_blob.data, m_blob.bytes);
  patch(m_data, m_blob.data);
}
