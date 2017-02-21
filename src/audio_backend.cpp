#include "hans/audio_backend.hpp"
#include "./audio_backend_jack.hpp"
#include "./audio_backend_portaudio.hpp"
#include "hans/config.hpp"
#include "hans/hasher.hpp"

using namespace hans;

AudioBackendBase* hans::make_audio_backend(hans::Settings& settings,
                                           hans::AudioBuses& buses,
                                           std::function<void(void)> callback) {
#ifdef PORTAUDIO_FOUND
  return new hans::AudioBackendPortAudio(settings, buses, callback);
#endif
#ifdef JACK_FOUND
  return new hans::AudioBackendJack(settings, buses, callback);
#endif
  return nullptr;
}
