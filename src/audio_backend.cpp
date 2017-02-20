#include "hans/audio_backend.hpp"
#include "./audio_backend_jack.hpp"
#include "./audio_backend_portaudio.hpp"
#include "hans/config.hpp"
#include "hans/hasher.hpp"

using namespace hans;

AudioBackendBase* hans::make_audio_backend(hans::Settings::Audio& settings,
                                           hans::AudioBuses& buses,
                                           std::function<void(void)> callback) {
  if (hasher("portaudio") == settings.backend) {
#ifdef PORTAUDIO_FOUND
    return new hans::AudioBackendPortAudio(settings, buses, callback);
#else
    throw std::runtime_error("Portaudio backend not supported");
#endif
  } else if (hasher("jack") == settings.backend) {
#ifdef JACK_FOUND
    return new hans::AudioBackendJack(settings, buses, callback);
#else
    throw std::runtime_error("JACK backend not supported");
#endif
  }
  return nullptr;
}
