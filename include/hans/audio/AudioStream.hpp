#ifndef HANS_AUDIO_AUDIOSTREAM_H_
#define HANS_AUDIO_AUDIOSTREAM_H_

#include <portaudio.h>
#include "hans/audio/AudioBusManager.hpp"
#include "hans/audio/AudioDevices.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace audio {

/// A live audio stream, other streams maybe an offline audio stream that just
/// ticks the audio graph as fast as possible
class AudioStream {
 public:
  enum state {
    HANS_AUDIO_STOPPED,
    HANS_AUDIO_PENDING,
    HANS_AUDIO_STARTED,
    HANS_AUDIO_ERROR
  };

  // FIXME: audioDevices should be marked as const
  explicit AudioStream(const common::Config& config,
                       AudioDevices& audio_devices,
                       AudioBusManager& audio_bus_manager,
                       std::function<void()> callback);
  ~AudioStream();

  void set_input_device(const Device& device);
  void set_output_device(const Device& device);

  bool open();
  bool start();
  bool stop();
  bool close();

  /// Audio callback, processes non-interleaved audio data. Audio data may
  /// come from the soundcard, another application etc.
  void callback(const sample** input, sample** output);

 private:
  AudioStream::state m_state;
  AudioDevices& m_audio_devices;
  AudioBusManager& m_audio_bus_manager;
  std::function<void()> m_callback;
  const common::Config& m_config;

  bus_handle m_bus;
  Device::ID m_input_device;
  Device::ID m_output_device;
  PaStream* m_stream;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIOSTREAM_H_
