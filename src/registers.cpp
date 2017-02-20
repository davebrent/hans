#include "hans/registers.hpp"
#include <cstring>

using namespace hans;

static uint16_t EMPTY_BIN = 65535;

RegisterManager::RegisterManager(const Settings& settings,
                                 const Registers& registers)
    : _registers(registers), _blocksize(settings.audio.blocksize) {
  _gfx_bins = new uint32_t[registers.gfx_registers]();
  _snd_bins = new audio::sample[registers.snd_registers * _blocksize]();
}

RegisterManager::~RegisterManager() {
  delete[] _gfx_bins;
  delete[] _snd_bins;
}

Register RegisterManager::make(ObjectDef::ID object, Register::Types type,
                               uint16_t index) {
  auto readonly = type != Register::Types::OUTLET;
  for (auto& reg : _registers.handles) {
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
