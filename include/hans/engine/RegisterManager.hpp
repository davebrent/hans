#ifndef HANS_ENGINE_REGISTERMANAGER_H_
#define HANS_ENGINE_REGISTERMANAGER_H_

#include "hans/common/types.hpp"
#include "hans/memory/LinearAllocator.hpp"

namespace hans {
namespace engine {

class RegisterManager {
 public:
  explicit RegisterManager(size_t register_size);

  bool is_empty(const hans_register_handle& handle) const;

  /// Set the interference graph and return the number of allocated registers
  int set_interference_graph(hans_object_connection* connections,
                             unsigned num_connections);

  /// Used by objects to read and write data to registers
  void* get_read_reg(const hans_register_handle& handle) const;
  bool set_write_reg(const hans_register_handle& handle,
                     const void* data) const;

  /// Return a handle to the register allocated to object with the instance id
  bool assign_write_reg(hans_register_handle& handle,
                        const uint32_t graph_index, unsigned outlet);
  bool assign_read_reg(hans_register_handle& handle, const uint32_t graph_index,
                       unsigned inlet);

  int make(hans_object_resource* resources, const uint32_t graph_index,
           int num_inlets, int num_outlets);

 private:
  size_t m_register_size;
  char* m_register_base;
  unsigned m_num_connections;
  hans::memory::LinearAllocator m_allocator;
  hans_object_connection* m_connections;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_REGISTERMANAGER_H_
