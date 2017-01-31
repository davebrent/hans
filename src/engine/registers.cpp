#include "hans/engine/registers.hpp"
#include <cassert>
#include <cstring>
#include <stdexcept>

using namespace hans;
using namespace hans::engine;

// If an object has empty connection, point all reads & writes to an empty bin
static uint16_t EMPTY_BIN = 65535;

RegisterManager::RegisterManager(const Settings& settings,
                                 std::vector<Register>& registers) {
  m_audio_reg_size = sizeof(audio::sample) * settings.blocksize;
  m_graphics_reg_size = sizeof(uint32_t);

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
    case ObjectDef::Types::AUDIO:
      if (reg.bin > m_num_audio_bins) {
        m_num_audio_bins = reg.bin;
      }
      break;
    case ObjectDef::Types::GRAPHICS:
      if (reg.bin > m_num_graphics_bins) {
        m_num_graphics_bins = reg.bin;
      }
      break;
    }
  }

  m_num_graphics_bins += 1;
  m_num_audio_bins += 1;

  // Add one for the "empty" register for both types
  auto a_bins_size = m_audio_reg_size * (m_num_audio_bins + 1);
  auto g_bins_size = m_graphics_reg_size * (m_num_graphics_bins + 1);

  // Single empty bin for both types
  auto e_bin_size = m_audio_reg_size;
  if (m_graphics_reg_size > m_audio_reg_size) {
    e_bin_size = m_graphics_reg_size;
  }

  m_allocator.reset(a_bins_size + g_bins_size + e_bin_size);
  m_audio_bin_base = static_cast<char*>(m_allocator.allocate(a_bins_size));
  m_graphics_bin_base = static_cast<char*>(m_allocator.allocate(g_bins_size));
  m_empty_bin_base = static_cast<char*>(m_allocator.allocate(e_bin_size));
}

Register RegisterManager::make(ObjectDef::ID object, Register::Types type,
                               uint16_t index) {
  auto readonly = type != Register::Types::OUTLET;

  auto registers = m_registers;
  for (auto i = 0; i < m_length; ++i) {
    auto& reg = registers[i];
    if (reg.object == object && reg.index == index &&
        reg.readonly == readonly) {
      return reg;
    }
  }

  // Or the empty register
  Register reg;
  reg.bin = EMPTY_BIN;
  return reg;
}

void* RegisterManager::read(const Register& reg) const {
  if (reg.bin == EMPTY_BIN) {
    return static_cast<void*>(m_empty_bin_base);
  }

  assert(reg.readonly == true);
  char* base = nullptr;
  size_t bin_size = 0;

  switch (reg.type) {
  case ObjectDef::Types::AUDIO:
    base = m_audio_bin_base;
    bin_size = m_audio_reg_size;
    break;
  case ObjectDef::Types::GRAPHICS:
    base = m_graphics_bin_base;
    bin_size = m_graphics_reg_size;
    break;
  }

  return static_cast<void*>(base + (bin_size * reg.bin));
}

bool RegisterManager::write(const Register& reg, const void* data) {
  if (reg.bin == EMPTY_BIN) {
    return false;
  }

  assert(reg.readonly != true);
  char* base = nullptr;
  size_t bin_size = 0;

  switch (reg.type) {
  case ObjectDef::Types::AUDIO:
    base = m_audio_bin_base;
    bin_size = m_audio_reg_size;
    assert(reg.bin < m_num_audio_bins);
    break;
  case ObjectDef::Types::GRAPHICS:
    base = m_graphics_bin_base;
    bin_size = m_graphics_reg_size;
    assert(reg.bin < m_num_graphics_bins);
    break;
  }

  auto dest = static_cast<void*>(base + (bin_size * reg.bin));
  std::memcpy(dest, data, bin_size);
  return true;
}
