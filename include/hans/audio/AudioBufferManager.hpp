#ifndef HANS_AUDIO_AUDIOBUFFERMANAGER_H_
#define HANS_AUDIO_AUDIOBUFFERMANAGER_H_

#include <vector>
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace audio {

class AudioBufferManager {
 public:
  explicit AudioBufferManager(common::ListView<hans_audio_buffer>& buffers);
  hans_audio_buffer make(hans_instance_id id, hans_hash name);
  hans_audio_sample* get(const hans_audio_buffer& buff, uint8_t channel) const;

 private:
  hans_audio_buffer* m_buffers = nullptr;
  size_t m_buffers_len = 0;
  char* m_base = nullptr;
  hans::common::LinearAllocator m_allocator;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIOBUFFERMANAGER_H_
