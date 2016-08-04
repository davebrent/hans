#ifndef HANS_AUDIO_AUDIOBUSMANAGER_H_
#define HANS_AUDIO_AUDIOBUSMANAGER_H_

#include <vector>
#include "hans/audio/AudioBufferManager.hpp"
#include "hans/audio/types.hpp"
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace audio {

class AudioBusManager {
 public:
  /// Creates N audio buses
  explicit AudioBusManager(const common::Config& config, size_t num);
  /// Create an audio bus
  bus_handle make();
  /// Write samples to a channel of a specified audio bus
  bool write(bus_handle handle, uint8_t channel, const sample* samples);
  /// Read data from the audio bus (the data should not be modified)
  sample* read(bus_handle handle, uint8_t channel);
  /// Returns true if the bus's data has changed
  bool is_dirty(bus_handle handle);
  /// Sets a bus as no longer being dirty
  void set_clean(bus_handle handle);

 private:
  common::LinearAllocator m_allocator;
  uint16_t m_blocksize = 0;
  uint8_t m_channels = 0;
  char* m_base = nullptr;
  size_t m_ids = 0;
  size_t m_max = 0;
  uint64_t* m_revisions;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIOBUSMANAGER_H_
