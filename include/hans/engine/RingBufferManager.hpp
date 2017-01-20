#ifndef HANS_ENGINE_RINGBUFFERMANAGER_H_
#define HANS_ENGINE_RINGBUFFERMANAGER_H_

#include <vector>
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/primitives.hpp"

namespace hans {
namespace engine {

class RingBufferManager {
 public:
  RingBufferManager(const RingBufferManager& other) = delete;
  RingBufferManager(uint16_t blocksize, std::vector<RingBuffer>& ring_buffers);
  ~RingBufferManager();

  RingBuffer make(ObjectDef::ID producer, hash name);

  /// Write samples to a ring buffer, samples must be length of blocksize
  bool write(const RingBuffer& ring, const audio::sample* samples);

  /// Advance all ring buffers
  void advance_all();

  /// Get the number of available frames for a named buffer
  uint8_t available(hash name);

  /// Read the Nth available frame for a named buffer
  audio::sample* read(hash name, uint8_t frame);

 private:
  RingBuffer find(hash name);

  common::LinearAllocator m_allocator;
  std::vector<RingBuffer>& m_ring_buffers;
  size_t m_frame_size;
  char* m_base = nullptr;
  uint8_t* m_available;
  uint8_t* m_heads;
  uint8_t* m_tails;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_RINGBUFFERMANAGER_H_
