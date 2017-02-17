#include "hans/registers.hpp"
#include <cstring>

using namespace hans;

// If an object has empty connection, point all reads & writes to an empty bin
static uint16_t EMPTY_BIN = 65535;

RegisterManager::RegisterManager(const Settings& settings,
                                 const std::vector<Register>& registers)
    : _registers(registers), _blocksize(settings.blocksize) {
  auto num_snd_bins = 0;
  auto num_gfx_bins = 0;

  // XXX: Move to compiler
  for (const auto& reg : _registers) {
    if (reg.type == ObjectDef::Types::GRAPHICS) {
      if (reg.bin > num_gfx_bins) {
        num_gfx_bins = reg.bin;
      }
    } else {
      if (reg.bin > num_snd_bins) {
        num_snd_bins = reg.bin;
      }
    }
  }

  _gfx_bins = new uint32_t[num_gfx_bins + 1]();
  _snd_bins = new audio::sample[(num_snd_bins + 1) * _blocksize]();
}

RegisterManager::~RegisterManager() {
  delete[] _gfx_bins;
  delete[] _snd_bins;
}

Register RegisterManager::make(ObjectDef::ID object, Register::Types type,
                               uint16_t index) {
  auto readonly = type != Register::Types::OUTLET;
  for (auto& reg : _registers) {
    if (reg.object == object && reg.index == index &&
        reg.readonly == readonly) {
      return reg;
    }
  }

  Register reg;
  reg.bin = EMPTY_BIN;
  return reg;
}

bool RegisterManager::has_data(const Register& reg) const {
  return reg.bin != EMPTY_BIN;
}

uint32_t RegisterManager::read(const Register& reg) const {
  return _gfx_bins[reg.bin];
}

void RegisterManager::write(const Register& reg, const uint32_t data) {
  if (reg.bin == EMPTY_BIN) {
    return;
  }
  _gfx_bins[reg.bin] = data;
}

audio::sample* RegisterManager::read_block(const Register& reg) const {
  return &_snd_bins[reg.bin * _blocksize];
}

void RegisterManager::write(const Register& reg, const audio::sample* data) {
  if (reg.bin == EMPTY_BIN) {
    return;
  }
  auto dest = &_snd_bins[reg.bin * _blocksize];
  std::memcpy(dest, data, sizeof(audio::sample) * _blocksize);
}
