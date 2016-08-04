#ifndef HANS_AUDIO_AUDIOBUFFERMANAGER_H_
#define HANS_AUDIO_AUDIOBUFFERMANAGER_H_

#include <vector>
#include "hans/audio/types.hpp"
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"

namespace hans {
namespace audio {

class AudioBufferManager {
 public:
  explicit AudioBufferManager(common::ListView<Buffer>& buffers);
  Buffer make(engine::ObjectDef::ID id, hash name);
  sample* get(const Buffer& buff, uint8_t channel) const;

 private:
  Buffer* m_buffers = nullptr;
  size_t m_buffers_len = 0;
  char* m_base = nullptr;
  hans::common::LinearAllocator m_allocator;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIOBUFFERMANAGER_H_
