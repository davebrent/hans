#ifndef HANS_ENGINE_AUDIOBUSES_H_
#define HANS_ENGINE_AUDIOBUSES_H_

#include "hans/engine/audio_buffers.hpp"
#include "hans/engine/linear_allocator.hpp"
#include "hans/engine/primitives.hpp"

namespace hans {
namespace engine {

class AudioBuses {
 public:
  /// Creates N audio buses
  AudioBuses(const AudioBuses& other) = delete;
  explicit AudioBuses(const Settings& settings, size_t num);
  ~AudioBuses();
  /// Create an audio bus
  audio::bus_handle make();
  /// Write samples to a channel of a specified audio bus
  bool write(audio::bus_handle handle, uint8_t channel,
             const audio::sample* samples);
  /// Read data from the audio bus (the data should not be modified)
  audio::sample* read(audio::bus_handle handle, uint8_t channel);
  /// Returns true if the bus's data has changed
  bool is_dirty(audio::bus_handle handle);
  /// Sets a bus as no longer being dirty
  void set_clean(audio::bus_handle handle);

 private:
  LinearAllocator m_allocator;
  uint16_t m_blocksize = 0;
  uint8_t m_channels = 0;
  char* m_base = nullptr;
  size_t m_ids = 0;
  size_t m_max = 0;
  uint64_t* m_revisions;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIOBUSES_H_
