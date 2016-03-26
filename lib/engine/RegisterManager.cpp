#include "hans/engine/RegisterManager.hpp"
#include <cstring>
#include <cassert>

using namespace hans;

engine::RegisterManager::RegisterManager(size_t register_size)
    : m_register_size(register_size) {
  m_register_base = nullptr;
  m_connections = nullptr;
  m_num_connections = 0;
}

bool engine::RegisterManager::is_empty(
    const hans_register_handle& handle) const {
  return handle.bin > m_num_connections;
}

void* engine::RegisterManager::get_read_reg(
    const hans_register_handle& handle) const {
  if (handle.bin < (m_num_connections + 2)) {
    return m_register_base + (m_register_size * handle.bin);
  }
  return nullptr;
}

bool engine::RegisterManager::set_write_reg(const hans_register_handle& handle,
                                            const void* src) const {
  if (handle.bin < (m_num_connections + 2)) {
    char* dest = m_register_base + (m_register_size * handle.bin);
    std::memcpy(dest, src, m_register_size);
    return true;
  }
  return false;
}

bool engine::RegisterManager::assign_write_reg(hans_register_handle& handle,
                                               const uint32_t object_index,
                                               unsigned outlet) {
  for (int i = 0; i < m_num_connections; ++i) {
    if (m_connections[i].source == object_index &&
        m_connections[i].outlet == outlet) {
      handle.bin = i;
      return true;
    }
  }
  return false;
}

bool engine::RegisterManager::assign_read_reg(hans_register_handle& handle,
                                              const uint32_t object_index,
                                              unsigned inlet) {
  for (int i = 0; i < m_num_connections; ++i) {
    if (m_connections[i].sink == object_index &&
        m_connections[i].inlet == inlet) {
      handle.bin = i;
      return true;
    }
  }
  return false;
}

int engine::RegisterManager::set_interference_graph(
    hans_object_connection* connections, unsigned num_connections) {
  m_connections = connections;
  m_num_connections = num_connections;
  // FIXME: Graph coloring and calculate the minimum number of registers
  //        needed to execute the graph
  m_allocator.reset(m_register_size * (num_connections + 2));
  m_register_base = static_cast<char*>(m_allocator.start());
  return num_connections;
}

int engine::RegisterManager::make(hans_object_resource* resources,
                                  const uint32_t object_index, int num_inlets,
                                  int num_outlets) {
  int created = 0;

  for (int l = 0; l < num_inlets; ++l) {
    resources->type = HANS_INLET;
    if (!assign_read_reg(resources->inlet, object_index, l)) {
      resources->inlet.bin = m_num_connections + 1;
    }

    created++;
    resources++;
  }

  for (int o = 0; o < num_outlets; ++o) {
    resources->type = HANS_OUTLET;
    if (!assign_write_reg(resources->outlet, object_index, o)) {
      resources->outlet.bin = m_num_connections + 2;
    }

    created++;
    resources++;
  }

  return created;
}
