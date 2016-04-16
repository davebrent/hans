#ifndef HANS_AUDIO_AUDIOBUSMANAGER_H_
#define HANS_AUDIO_AUDIOBUSMANAGER_H_

#include <vector>
#include "hans/audio/AudioBufferManager.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace audio {

class AudioBusManager {
 public:
  /// Audio buses are used to route audio data around on the application level
  /// decoupling the audio device from the audio graph
  explicit AudioBusManager(hans::audio::AudioBufferManager& buffer_manager);

  /// Create an audio bus
  /// Only applications are expected to create buses, not objects
  hans_audio_bus_handle make(uint8_t channels, uint16_t blocksize);

  /// Write samples to a channel of a specified audio bus
  bool write(hans_audio_bus_handle handle, uint8_t channel,
             const hans_audio_sample* samples);

  /// Read data from the audio bus (the data should not be modified)
  hans_audio_sample* read(hans_audio_bus_handle handle, uint8_t channel);

 private:
  hans::audio::AudioBufferManager& m_buffer_manager;
  std::vector<hans_audio_buffer*> m_buses;
  uint16_t m_ids;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIOBUSMANAGER_H_
