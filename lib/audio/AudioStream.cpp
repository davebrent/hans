#include "hans/audio/AudioStream.hpp"
#include <portaudio.h>
#include <chrono>
#include <thread>

using namespace hans;

static int audio_callback(const void* input, void* output,
                          unsigned long frame_count,
                          const PaStreamCallbackTimeInfo* time_info,
                          PaStreamCallbackFlags status_flags, void* user_data) {
  audio::AudioStream* stream = static_cast<audio::AudioStream*>(user_data);
  stream->callback(
      static_cast<const hans_audio_sample**>(const_cast<void*>(input)),
      static_cast<hans_audio_sample**>(output));
  return paContinue;
}

audio::AudioStream::AudioStream(const hans_audio_device_parameters& parameters,
                                audio::AudioDevices& audio_devices,
                                audio::AudioBusManager& audio_bus_manager,
                                engine::ProgramManager& program_manager,
                                common::Logger& logger)
    : m_audio_devices(audio_devices),
      m_audio_bus_manager(audio_bus_manager),
      m_program_manager(program_manager),
      m_parameters(parameters),
      m_logger(logger) {
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

audio::AudioStream::~AudioStream() {
  if (m_stream != nullptr) {
    Pa_CloseStream(m_stream);
  }
}

void audio::AudioStream::set_input_device(const hans_audio_device& device) {
  m_input_device = device.id;
}

void audio::AudioStream::set_output_device(const hans_audio_device& device) {
  m_output_device = device.id;
}

void audio::AudioStream::set_parameters(
    const hans_audio_device_parameters& parameters) {
  m_parameters.num_channels = parameters.num_channels;
  m_parameters.sample_rate = parameters.sample_rate;
  m_parameters.block_size = parameters.block_size;
}

hans_audio_device_parameters audio::AudioStream::get_parameters() const {
  return m_parameters;
}

bool audio::AudioStream::is_running() const {
  return m_stream != nullptr;
}

bool audio::AudioStream::open() {
  PaStreamParameters input_params;
  input_params.device = m_input_device;
  input_params.channelCount = m_parameters.num_channels;
  input_params.sampleFormat = paFloat32 | paNonInterleaved;
  input_params.hostApiSpecificStreamInfo = nullptr;

  PaStreamParameters output_params;
  output_params.device = m_output_device;
  output_params.channelCount = m_parameters.num_channels;
  output_params.sampleFormat = paFloat32 | paNonInterleaved;
  output_params.hostApiSpecificStreamInfo = nullptr;

  // Create corresponding audio bus to write (or read) audio data to (or from)
  m_bus = m_audio_bus_manager.make(m_parameters.num_channels,
                                   m_parameters.block_size);

  PaError error = Pa_OpenStream(
      &m_stream, &input_params, &output_params, m_parameters.sample_rate,
      m_parameters.block_size, paClipOff, &audio_callback, this);

  if (error == paNoError) {
    return true;
  }

  m_state = HANS_AUDIO_ERROR;
  m_logger.log(common::Logger::ERROR, Pa_GetErrorText(error));
  return false;
}

void audio::AudioStream::callback(const hans_audio_sample** input,
                                  hans_audio_sample** output) {
  if (m_state == HANS_AUDIO_PENDING) {
    m_state = HANS_AUDIO_STARTED;
  }

  if (m_state == HANS_AUDIO_STOPPED) {
    return;
  }

  // Write input data to the default audio bus, channel by channel
  for (int channel = 0; channel < m_parameters.num_channels; ++channel) {
    bool res = m_audio_bus_manager.write(m_bus, channel, input[channel]);
    assert(res);
  }

  // Tick the audio graph
  // XXX: Maybe use a message/signal thats executed immediately to run a cycle
  //      of the audio graph?
  m_program_manager.process_audio();

  // Read the data on the bus back and send to the sound card
  for (int channel = 0; channel < m_parameters.num_channels; ++channel) {
    auto samples = m_audio_bus_manager.read(m_bus, channel);
    for (int s = 0; s < m_parameters.block_size; ++s) {
      output[channel][s] = samples[s];
    }
  }
}

bool audio::AudioStream::start() {
  if (m_stream == nullptr) {
    return false;
  }

  m_state = HANS_AUDIO_PENDING;

  PaError error = Pa_StartStream(m_stream);

  if (error == paNoError) {
    return true;
  }

  m_state = HANS_AUDIO_ERROR;
  m_logger.log(common::Logger::ERROR, Pa_GetErrorText(error));
  return false;
}

bool audio::AudioStream::stop() {
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
  m_logger.log(common::Logger::ERROR, Pa_GetErrorText(error));
  return false;
}

bool audio::AudioStream::close() {
  if (m_stream == nullptr) {
    return false;
  }

  return Pa_CloseStream(m_stream) == paNoError;
}
