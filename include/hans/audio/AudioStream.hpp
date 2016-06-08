#ifndef HANS_AUDIO_AUDIOSTREAM_H_
#define HANS_AUDIO_AUDIOSTREAM_H_

#include <portaudio.h>
#include "hans/audio/AudioDevices.hpp"
#include "hans/common/Logger.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/ProgramManager.hpp"

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
  explicit AudioStream(const hans_config& config,
                       hans::audio::AudioDevices& audio_devices,
                       hans::audio::AudioBusManager& audio_bus_manager,
                       hans::engine::ProgramManager& program_manager);
  ~AudioStream();

  void set_input_device(const hans_audio_device& device);
  void set_output_device(const hans_audio_device& device);

  bool open();
  bool start();
  bool stop();
  bool close();

  /// Audio callback, processes non-interleaved audio data. Audio data may
  /// come from the soundcard, another application etc.
  void callback(const hans_audio_sample** input, hans_audio_sample** output);

 private:
  AudioStream::state m_state;
  hans::audio::AudioDevices& m_audio_devices;
  hans::audio::AudioBusManager& m_audio_bus_manager;
  hans::engine::ProgramManager& m_program_manager;
  const hans_config& m_config;
  // hans::common::Logger& m_logger;

  hans_audio_bus_handle m_bus;
  hans_audio_device_id m_input_device;
  hans_audio_device_id m_output_device;
  PaStream* m_stream;
};

} // namespace audio
} // namespace hans

#endif // HANS_AUDIO_AUDIOSTREAM_H_
