#ifndef HANS_AUDIO_RINGBUFFERMANAGER_H_
#define HANS_AUDIO_RINGBUFFERMANAGER_H_

#include "hans/audio/RingBufferManager.hpp"
#include "hans/audio/types.hpp"
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/types.hpp"

namespace hans {
namespace audio {

class RingBufferManager {
 public:
  RingBufferManager(const common::Config& config,
                    common::ListView<RingBuffer> ring_buffers);

  RingBuffer make(engine::ObjectDef::ID producer, hash name);

  /// Write samples to a ring buffer, samples must be length of blocksize
  bool write(RingBuffer ring, const sample* samples);

  /// Advance all ring buffers
  void advance_all();

  /// Get the number of available frames for a named buffer
  uint8_t available(hash name);

  /// Read the Nth available frame for a named buffer
  sample* read(hash name, uint8_t frame);

 private:
  RingBuffer find(hash name);

  common::LinearAllocator m_allocator;
  common::ListView<RingBuffer> m_ring_buffers;
  size_t m_frame_size;
  char* m_base = nullptr;
  uint8_t* m_available;
  uint8_t* m_heads;
  uint8_t* m_tails;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_RINGBUFFERMANAGER_H_
