#include "./audio_backend_jack.hpp"
#ifdef JACK_FOUND
#include <jack/jack.h>
#include <cassert>
#include <iostream>

using namespace hans;

bool AudioBackendJack::open() {
  _bus = _buses.make();

  jack_options_t options = JackNullOption;
  jack_status_t status;

  _client = jack_client_open("Hans", options, &status, nullptr);
  if (_client == nullptr) {
    return false;
  }

  if (status & JackServerFailed) {
    throw std::runtime_error("Hans unable to connect to JACK server");
  }

  if (jack_get_sample_rate(_client) != _settings.samplerate) {
    throw std::runtime_error("Hans samplerate not equal to JACK samplerate");
  }

  jack_set_buffer_size(_client, _settings.blocksize);

  _output_ports.reserve(_settings.output_channels.size());
  _input_ports.reserve(_settings.input_channels.size());

  for (const auto c : _settings.input_channels) {
    auto input_name = "input_" + std::to_string(c);
    _input_ports.push_back(jack_port_register(_client, input_name.c_str(),
                                              JACK_DEFAULT_AUDIO_TYPE,
                                              JackPortIsInput, 0));
  }

  for (const auto c : _settings.output_channels) {
    auto output_name = "output_" + std::to_string(c);
    _output_ports.push_back(jack_port_register(_client, output_name.c_str(),
                                               JACK_DEFAULT_AUDIO_TYPE,
                                               JackPortIsOutput, 0));
  }

  jack_set_process_callback(_client, jack_callback, this);
  return true;
}

bool AudioBackendJack::start() {
  jack_activate(_client);
  return true;
}

bool AudioBackendJack::stop() {
  return true;
}

bool AudioBackendJack::close() {
  jack_client_close(_client);
  return true;
}

void AudioBackendJack::callback(jack_nframes_t nframes) {
  assert(_settings.blocksize == nframes);

  // Write input data to the audio bus
  for (auto c = 0; c < _settings.input_channels.size(); ++c) {
    auto buffer = static_cast<audio::sample*>(
        jack_port_get_buffer(_input_ports.at(c), nframes));
    _buses.write(_bus, c, buffer);
  }

  _buses.set_clean(_bus);
  // Tick the audio graph
  _callback();

  if (_buses.is_dirty(_bus)) {
    // Read data back off the bus, then send it back out to JACK
    for (auto c = 0; c < _settings.output_channels.size(); ++c) {
      auto buffer = static_cast<audio::sample*>(
          jack_port_get_buffer(_output_ports.at(c), nframes));
      auto samples = _buses.read(_bus, c);

      for (auto s = 0; s < nframes; ++s) {
        buffer[s] = samples[s];
      }
    }
  }
}

#endif // JACK_FOUND
