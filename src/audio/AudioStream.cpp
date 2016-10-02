#include "hans/audio/AudioStream.hpp"
#include <portaudio.h>
#include <chrono>
#include <thread>

using namespace hans;
using namespace hans::audio;
using namespace hans::common;
using namespace hans::engine;

static int audio_callback(const void* input, void* output,
                          unsigned long frame_count,
                          const PaStreamCallbackTimeInfo* time_info,
                          PaStreamCallbackFlags status_flags, void* user_data) {
  AudioStream* stream = static_cast<AudioStream*>(user_data);
  stream->callback(static_cast<const sample**>(const_cast<void*>(input)),
                   static_cast<sample**>(output));
  return paContinue;
}

AudioStream::AudioStream(const Config& config, AudioDevices& audio_devices,
                         AudioBusManager& audio_bus_manager,
                         std::function<void()> callback)
    : m_audio_devices(audio_devices),
      m_audio_bus_manager(audio_bus_manager),
      m_callback(callback),
      m_config(config) {
  m_stream = nullptr;
  m_state = HANS_AUDIO_STOPPED;

  for (auto& device : m_audio_devices) {
    if (device.default_input) {
      m_input_device = device.id;
    }

    if (device.default_output) {
      m_output_device = device.id;
    }
  }
}

AudioStream::~AudioStream() {
  if (m_stream != nullptr) {
    Pa_CloseStream(m_stream);
  }
}

void AudioStream::set_input_device(const Device& device) {
  m_input_device = device.id;
}

void AudioStream::set_output_device(const Device& device) {
  m_output_device = device.id;
}

bool AudioStream::open() {
  PaStreamParameters input_params;
  input_params.device = m_input_device;
  input_params.channelCount = m_config.channels;
  input_params.sampleFormat = paFloat32 | paNonInterleaved;
  input_params.hostApiSpecificStreamInfo = nullptr;

  PaStreamParameters output_params;
  output_params.device = m_output_device;
  output_params.channelCount = m_config.channels;
  output_params.sampleFormat = paFloat32 | paNonInterleaved;
  output_params.hostApiSpecificStreamInfo = nullptr;

  m_bus = m_audio_bus_manager.make();

  PaError error = Pa_OpenStream(&m_stream, &input_params, &output_params,
                                m_config.samplerate, m_config.blocksize,
                                paClipOff, &audio_callback, this);

  if (error == paNoError) {
    return true;
  }

  m_state = HANS_AUDIO_ERROR;
  return false;
}

void AudioStream::callback(const sample** input, sample** output) {
  if (m_state == HANS_AUDIO_PENDING) {
    m_state = HANS_AUDIO_STARTED;
  }

  if (m_state == HANS_AUDIO_STOPPED) {
    return;
  }

  // Write input data to the default audio bus, channel by channel
  for (int channel = 0; channel < m_config.channels; ++channel) {
    m_audio_bus_manager.write(m_bus, channel, input[channel]);
  }

  m_audio_bus_manager.set_clean(m_bus);

  // Tick the audio graph
  // XXX: Maybe use a message/signal thats executed immediately to run a cycle
  //      of the audio graph?
  m_callback();

  // A prorgam wrote some data to the streams bus so should be flushed out
  if (m_audio_bus_manager.is_dirty(m_bus)) {
    // Read the data on the bus back and send to the sound card
    for (int channel = 0; channel < m_config.channels; ++channel) {
      auto samples = m_audio_bus_manager.read(m_bus, channel);
      for (int s = 0; s < m_config.blocksize; ++s) {
        output[channel][s] = samples[s];
      }
    }
  }
}

bool AudioStream::start() {
  if (m_stream == nullptr) {
    return false;
  }

  m_state = HANS_AUDIO_PENDING;

  PaError error = Pa_StartStream(m_stream);

  if (error == paNoError) {
    return true;
  }

  m_state = HANS_AUDIO_ERROR;
  return false;
}

bool AudioStream::stop() {
  if (m_stream == nullptr) {
    return true;
  }

  while (m_state == HANS_AUDIO_PENDING) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }

  PaError error = Pa_StopStream(m_stream);
  if (error == paNoError) {
    m_state = HANS_AUDIO_STOPPED;
    return true;
  }

  m_state = HANS_AUDIO_ERROR;
  return false;
}

bool AudioStream::close() {
  if (m_stream == nullptr) {
    return false;
  }

  return Pa_CloseStream(m_stream) == paNoError;
}
