#ifndef HANS_AUDIO_RINGBUFFERMANAGER_H_
#define HANS_AUDIO_RINGBUFFERMANAGER_H_

#include "hans/audio/RingBufferManager.hpp"
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace audio {

class RingBufferManager {
 public:
  RingBufferManager(const hans_config& config,
                    hans::common::ListView<hans_ring_buffer>& ring_buffers);

  hans_ring_buffer make(hans_instance_id producer, hans_hash name);

  /// Write samples to a ring buffer, samples must be length of blocksize
  bool write(hans_ring_buffer ring, const hans_audio_sample* samples);

  /// Advance all ring buffers
  void advance_all();

  /// Get the number of available frames for a named buffer
  uint8_t available(hans_hash name);

  /// Read the Nth available frame for a named buffer
  hans_audio_sample* read(hans_hash name, uint8_t frame);

 private:
  hans_ring_buffer find(hans_hash name);

  hans::common::LinearAllocator m_allocator;
  hans_ring_buffer* m_ring_buffers;
  size_t m_ring_buffers_len;
  size_t m_frame_size;
  char* m_base = nullptr;
  uint8_t* m_available;
  uint8_t* m_heads;
  uint8_t* m_tails;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_RINGBUFFERMANAGER_H_
