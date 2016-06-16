#ifndef HANS_ENGINE_REGISTERMANAGER_H_
#define HANS_ENGINE_REGISTERMANAGER_H_

#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

class RegisterManager {
 public:
  RegisterManager(const hans_config& config);
  /// Return a handle to a register
  hans_register make(hans_instance_id object, hans_resource_type type,
                     uint16_t index);
  /// Set the manager to read from a given array of values
  void use(hans::common::ListView<hans_register>& registers);
  /// Read data from a register
  void* read(const hans_register& reg) const;
  /// Write data to a register
  bool write(const hans_register& reg, const void* data);

 private:
  size_t m_audio_reg_size;
  size_t m_graphics_reg_size;

  size_t m_length = 0;
  hans_register* m_registers = nullptr;

  hans::common::LinearAllocator m_allocator;
  char* m_audio_bin_base = 0;
  char* m_graphics_bin_base = 0;
  char* m_empty_bin_base = 0;
  int m_num_audio_bins = 0;
  int m_num_graphics_bins = 0;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_REGISTERMANAGER_H_
