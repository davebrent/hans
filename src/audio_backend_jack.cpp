#include "./audio_backend_jack.hpp"
#include <jack/jack.h>
#include <cassert>
#include <iostream>

using namespace hans;

bool AudioBackendJack::open() {
  m_bus = m_buses.make();

  jack_options_t options = JackNullOption;
  jack_status_t status;

  m_client = jack_client_open("Hans", options, &status, nullptr);
  if (m_client == nullptr) {
    return false;
  }

  if (status & JackServerFailed) {
    throw std::runtime_error("Hans unable to connect to JACK server");
  }

  if (jack_get_sample_rate(m_client) != m_settings.samplerate) {
    throw std::runtime_error("Hans samplerate not equal to JACK samplerate");
  }

  jack_set_buffer_size(m_client, m_settings.blocksize);

  m_output_ports.reserve(m_settings.channels);
  m_input_ports.reserve(m_settings.channels);

  for (auto i = 0; i < m_settings.channels; ++i) {
    auto port = std::to_string(i);
    auto output_name = "output" + port;
    auto input_name = "input" + port;
    m_output_ports.push_back(jack_port_register(m_client, output_name.c_str(),
                                                JACK_DEFAULT_AUDIO_TYPE,
                                                JackPortIsOutput, 0));
    m_input_ports.push_back(jack_port_register(m_client, input_name.c_str(),
                                               JACK_DEFAULT_AUDIO_TYPE,
                                               JackPortIsInput, 0));
  }

  jack_set_process_callback(m_client, jack_callback, this);
  return true;
}

bool AudioBackendJack::start() {
  jack_activate(m_client);
  return true;
}

bool AudioBackendJack::stop() {
  return true;
}

bool AudioBackendJack::close() {
  jack_client_close(m_client);
  return true;
}

void AudioBackendJack::callback(jack_nframes_t nframes) {
  assert(m_settings.blocksize == nframes);

  // Write input data to the audio bus
  for (auto c = 0; c < m_settings.channels; ++c) {
    auto buffer = static_cast<audio::sample*>(
        jack_port_get_buffer(m_input_ports.at(c), nframes));
    m_buses.write(m_bus, c, buffer);
  }

  m_buses.set_clean(m_bus);
  // Tick the audio graph
  m_callback();

  if (m_buses.is_dirty(m_bus)) {
    // Read data back off the bus, then send it back out to JACK
    for (auto c = 0; c < m_settings.channels; ++c) {
      auto buffer = static_cast<audio::sample*>(
          jack_port_get_buffer(m_output_ports.at(c), nframes));
      auto samples = m_buses.read(m_bus, c);

      for (auto s = 0; s < nframes; ++s) {
        buffer[s] = samples[s];
      }
    }
  }
}
