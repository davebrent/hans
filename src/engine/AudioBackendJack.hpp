#ifndef HANS_ENGINE_AUDIOBACKEND_JACK_H_
#define HANS_ENGINE_AUDIOBACKEND_JACK_H_

#include <jack/jack.h>
#include "hans/engine/AudioBackendImpl.hpp"

namespace hans {
namespace engine {

class AudioBackendJack : protected AudioBackendImpl {
 public:
  using AudioBackendImpl::AudioBackendImpl;

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

  audio::bus_handle m_bus;
  jack_client_t* m_client;
  std::vector<jack_port_t*> m_output_ports;
  std::vector<jack_port_t*> m_input_ports;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIOBACKEND_JACK_H_
