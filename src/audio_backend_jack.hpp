#ifndef HANS_AUDIOBACKEND_JACK_H_
#define HANS_AUDIOBACKEND_JACK_H_
#include "hans/config.hpp"
#ifdef JACK_FOUND

#include <jack/jack.h>
#include "hans/audio_backend_base.hpp"

namespace hans {

class AudioBackendJack : public AudioBackendBase {
 public:
  using AudioBackendBase::AudioBackendBase;

  bool open();
  bool start();
  bool stop();
  bool close();

 private:
  void callback(jack_nframes_t nframes);
  static int jack_callback(jack_nframes_t nframes, void* arg) {
    static_cast<AudioBackendJack*>(arg)->callback(nframes);
    return 0;
  }

  audio::bus_handle _bus;
  jack_client_t* _client = nullptr;
  std::vector<jack_port_t*> _output_ports;
  std::vector<jack_port_t*> _input_ports;
};

} // namespace hans

#endif // JACK_FOUND
#endif // HANS_AUDIOBACKEND_JACK_H_
