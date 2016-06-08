#include "hans/engine/RegisterManager.hpp"
#include <cassert>
#include <cstring>
#include <stdexcept>

using namespace hans;

// Bin type sizes
static size_t AUDIO_SIZE = sizeof(hans_audio_buffer);
static size_t GRAPHICS_SIZE = sizeof(uint32_t);
// If an object has empty connection, point all reads & writes to an empty bin
static uint16_t EMPTY_BIN = 65535;

void engine::RegisterManager::use(common::ListView<hans_register>& registers) {
  m_registers = &registers[0];
  m_length = registers.size();

  m_num_audio_bins = -1;
  m_num_graphics_bins = -1;

  for (auto i = 0; i < m_length; ++i) {
    auto& reg = registers[i];
    if (reg.bin == EMPTY_BIN) {
      throw std::runtime_error("Assigned register is the empty register");
    }

    switch (reg.type) {
    case HANS_OBJECT_AUDIO:
      if (reg.bin > m_num_audio_bins) {
        m_num_audio_bins = reg.bin;
      }
      break;
    case HANS_OBJECT_GRAPHICS:
      if (reg.bin > m_num_graphics_bins) {
        m_num_graphics_bins = reg.bin;
      }
      break;
    }
  }

  m_num_graphics_bins += 1;
  m_num_audio_bins += 1;

  // Add one for the "empty" register for both types
  auto a_bins_size = AUDIO_SIZE * (m_num_audio_bins + 1);
  auto g_bins_size = GRAPHICS_SIZE * (m_num_graphics_bins + 1);

  // Single empty bin for both types
  auto e_bin_size = AUDIO_SIZE;
  if (GRAPHICS_SIZE > AUDIO_SIZE) {
    e_bin_size = GRAPHICS_SIZE;
  }

  m_allocator.reset(a_bins_size + g_bins_size + e_bin_size);
  m_audio_bin_base = static_cast<char*>(m_allocator.allocate(a_bins_size));
  m_graphics_bin_base = static_cast<char*>(m_allocator.allocate(g_bins_size));
  m_empty_bin_base = static_cast<char*>(m_allocator.allocate(e_bin_size));
}

hans_register engine::RegisterManager::make(hans_instance_id object,
                                            hans_resource_type type,
                                            uint16_t index) {
  auto readonly = true;
  if (type == HANS_OUTLET) {
    readonly = false;
  }

  auto registers = m_registers;
  for (auto i = 0; i < m_length; ++i) {
    auto& reg = registers[i];
    if (reg.object == object && reg.index == index &&
        reg.readonly == readonly) {
      return reg;
    }
  }

  // Or the empty register
  hans_register reg;
  reg.bin = EMPTY_BIN;
  return reg;
}

void* engine::RegisterManager::read(const hans_register& reg) const {
  if (reg.bin == EMPTY_BIN) {
    return static_cast<void*>(m_empty_bin_base);
  }

  assert(reg.readonly == true);
  char* base = nullptr;
  size_t bin_size = 0;

  switch (reg.type) {
  case HANS_OBJECT_AUDIO:
    base = m_audio_bin_base;
    bin_size = AUDIO_SIZE;
    break;
  case HANS_OBJECT_GRAPHICS:
    base = m_graphics_bin_base;
    bin_size = GRAPHICS_SIZE;
    break;
  }

  return static_cast<void*>(base + (bin_size * reg.bin));
}

bool engine::RegisterManager::write(const hans_register& reg,
                                    const void* data) {
  if (reg.bin == EMPTY_BIN) {
    return false;
  }

  assert(reg.readonly != true);
  char* base = nullptr;
  size_t bin_size = 0;

  switch (reg.type) {
  case HANS_OBJECT_AUDIO:
    base = m_audio_bin_base;
    bin_size = AUDIO_SIZE;
    assert(reg.bin < m_num_audio_bins);
    break;
  case HANS_OBJECT_GRAPHICS:
    base = m_graphics_bin_base;
    bin_size = GRAPHICS_SIZE;
    assert(reg.bin < m_num_graphics_bins);
    break;
  }

  auto dest = static_cast<void*>(base + (bin_size * reg.bin));
  std::memcpy(dest, data, bin_size);
  return true;
}
