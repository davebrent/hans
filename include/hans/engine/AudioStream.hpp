#ifndef HANS_ENGINE_AUDIOSTREAM_H_
#define HANS_ENGINE_AUDIOSTREAM_H_

#include <portaudio.h>
#include "hans/common/types.hpp"
#include "hans/engine/AudioBusManager.hpp"
#include "hans/engine/AudioDevices.hpp"

namespace hans {
namespace engine {

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

  void set_input_device(const audio::Device& device);
  void set_output_device(const audio::Device& device);

  bool open();
  bool start();
  bool stop();
  bool close();

  /// Audio callback, processes non-interleaved audio data. Audio data may
  /// come from the soundcard, another application etc.
  void callback(const audio::sample** input, audio::sample** output);

 private:
  AudioStream::state m_state;
  AudioDevices& m_audio_devices;
  AudioBusManager& m_audio_bus_manager;
  std::function<void()> m_callback;
  const common::Config& m_config;

  audio::bus_handle m_bus;
  audio::Device::ID m_input_device;
  audio::Device::ID m_output_device;
  PaStream* m_stream;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIOSTREAM_H_
