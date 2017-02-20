#ifndef HANS_AUDIOBUSES_H_
#define HANS_AUDIOBUSES_H_

#include "hans/audio_buffers.hpp"
#include "hans/linear_allocator.hpp"
#include "hans/primitives.hpp"

namespace hans {

class AudioBuses {
 public:
  AudioBuses(const AudioBuses& other) = delete;
  AudioBuses(const Settings::Audio& settings, size_t num);
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
  LinearAllocator _allocator;
  const Settings::Audio& _settings;
  char* _base = nullptr;
  size_t _ids = 0;
  size_t _max = 0;
  uint64_t* _revisions;
};

} // namespace hans

#endif // HANS_AUDIOBUSES_H_
