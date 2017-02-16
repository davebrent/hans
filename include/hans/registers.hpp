#ifndef HANS_REGISTERS_H_
#define HANS_REGISTERS_H_

#include <vector>
#include "hans/linear_allocator.hpp"
#include "hans/primitives.hpp"

namespace hans {

class RegisterManager {
 public:
  RegisterManager(const RegisterManager& other) = delete;
  RegisterManager(const Settings& settings, std::vector<Register>& registers);
  /// Return a handle to a register
  Register make(ObjectDef::ID object, Register::Types type, uint16_t index);
  /// Read data from a register
  void* read(const Register& reg) const;
  /// Write data to a register
  bool write(const Register& reg, const void* data);

 private:
  size_t m_audio_reg_size;
  size_t m_graphics_reg_size;

  size_t m_length = 0;
  Register* m_registers = nullptr;

  LinearAllocator m_allocator;
  char* m_audio_bin_base = 0;
  char* m_graphics_bin_base = 0;
  char* m_empty_bin_base = 0;
  int m_num_audio_bins = 0;
  int m_num_graphics_bins = 0;
};

} // namespace hans

#endif // HANS_REGISTERS_H_
