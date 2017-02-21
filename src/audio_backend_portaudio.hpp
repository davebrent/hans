#ifndef HANS_AUDIOBACKEND_PORTAUDIO_H_
#define HANS_AUDIOBACKEND_PORTAUDIO_H_
#include "hans/config.hpp"
#ifdef PORTAUDIO_FOUND

#include <portaudio.h>
#include "hans/audio_backend_base.hpp"

namespace hans {

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

} // namespace hans

#endif // PORTAUDIO_FOUND
#endif // HANS_AUDIOBACKEND_PORTAUDIO_H_
