#ifndef HANS_REGISTERS_H_
#define HANS_REGISTERS_H_

#include <vector>
#include "hans/primitives.hpp"

namespace hans {

class RegisterManager {
 public:
  RegisterManager(const RegisterManager& other) = delete;
  RegisterManager(const Settings& settings,
                  const std::vector<Register>& registers);
  ~RegisterManager();

  Register make(ObjectDef::ID object, Register::Types type, uint16_t index);
  bool has_data(const Register& reg) const;

  uint32_t read(const Register& reg) const;
  void write(const Register& reg, const uint32_t data);

  audio::sample* read_block(const Register& reg) const;
  void write(const Register& reg, const audio::sample* data);

 private:
  const std::vector<Register>& _registers;
  size_t _blocksize;
  uint32_t* _gfx_bins;
  audio::sample* _snd_bins;
};

} // namespace hans

#endif // HANS_REGISTERS_H_
