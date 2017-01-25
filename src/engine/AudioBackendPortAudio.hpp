#ifndef HANS_ENGINE_AUDIOBACKEND_PORTAUDIO_H_
#define HANS_ENGINE_AUDIOBACKEND_PORTAUDIO_H_

#include <portaudio.h>
#include "hans/engine/AudioBackendImpl.hpp"

namespace hans {
namespace engine {

class AudioBackendPortAudio : protected AudioBackendImpl {
 public:
  using AudioBackendImpl::AudioBackendImpl;
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
