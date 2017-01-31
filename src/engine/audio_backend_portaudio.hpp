#ifndef HANS_ENGINE_AUDIOBACKEND_PORTAUDIO_H_
#define HANS_ENGINE_AUDIOBACKEND_PORTAUDIO_H_

#include <portaudio.h>
#include "hans/engine/audio_backend_base.hpp"

namespace hans {
namespace engine {

class AudioBackendPortAudio : public AudioBackendBase {
 public:
  using AudioBackendBase::AudioBackendBase;
  enum State { STOPPED, PENDING, STARTED, ERROR };

  virtual bool open() override;
  virtual bool start() override;
  virtual bool stop() override;
  virtual bool close() override;
  void callback(const audio::sample** input, audio::sample** output);

 private:
  State m_state = STOPPED;
  audio::bus_handle m_bus;
  PaStream* m_stream = nullptr;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIOBACKEND_PORTAUDIO_H_
