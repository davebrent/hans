#ifndef HANS_AUDIO_AUDIOBUFFERMANAGER_H_
#define HANS_AUDIO_AUDIOBUFFERMANAGER_H_

#include "hans/common/types.hpp"
#include <vector>

namespace hans {
namespace audio {

class AudioBufferManager {
 public:
  explicit AudioBufferManager(uint16_t blocksize);
  ~AudioBufferManager();

  /// Create a number of audio buffers all with uniform number of channels and
  /// samples in each buffer
  hans_audio_buffer* create(uint8_t num_channels, unsigned num_samples,
                            unsigned num_buffers);

  int make(hans_object_resource* resources, int len);

 private:
  uint16_t m_blocksize;
  std::vector<void*> m_buffers;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIOBUFFERMANAGER_H_
