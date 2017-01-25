#include "./AudioBackendPortAudio.hpp"
#include <chrono>
#include <stdexcept>
#include <thread>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

static int _portaudio_callback(const void* input, void* output,
                               unsigned long frame_count,
                               const PaStreamCallbackTimeInfo* time_info,
                               PaStreamCallbackFlags status_flags,
                               void* user_data) {
  auto stream = static_cast<AudioBackendPortAudio*>(user_data);
  stream->callback(static_cast<const audio::sample**>(const_cast<void*>(input)),
                   static_cast<audio::sample**>(output));
  return paContinue;
}

bool AudioBackendPortAudio::open() {
  auto error = Pa_Initialize();
  if (error != paNoError) {
    throw std::runtime_error(Pa_GetErrorText(error));
  }

  auto samplerate = m_settings.samplerate;
  auto channels = m_settings.channels;
  auto blocksize = m_settings.blocksize;

  m_state = STOPPED;

  PaStreamParameters input;
  input.device = Pa_GetDefaultInputDevice();
  input.channelCount = channels;
  input.sampleFormat = paFloat32 | paNonInterleaved;
  input.hostApiSpecificStreamInfo = nullptr;

  PaStreamParameters output;
  output.device = Pa_GetDefaultOutputDevice();
  output.channelCount = channels;
  output.sampleFormat = paFloat32 | paNonInterleaved;
  output.hostApiSpecificStreamInfo = nullptr;

  m_bus = m_buses.make();

  error = Pa_OpenStream(&m_stream, &input, &output, samplerate, blocksize,
                        paClipOff, &_portaudio_callback, this);
  if (error == paNoError) {
    return true;
  }

  m_state = ERROR;
  return false;
}

bool AudioBackendPortAudio::start() {
  if (m_stream == nullptr) {
    return false;
  }

  m_state = PENDING;

  auto error = Pa_StartStream(m_stream);
  if (error == paNoError) {
    return true;
  }

  m_state = ERROR;
  return false;
}

void AudioBackendPortAudio::callback(const audio::sample** input,
                                     audio::sample** output) {
  if (m_state == PENDING) {
    m_state = STARTED;
  }

  if (m_state == STOPPED) {
    return;
  }

  // Write input data to the default audio bus, channel by channel
  for (int channel = 0; channel < m_settings.channels; ++channel) {
    m_buses.write(m_bus, channel, input[channel]);
  }

  m_buses.set_clean(m_bus);

  // Tick the audio graph
  // XXX: Maybe use a message/signal thats executed immediately to run a cycle
  //      of the audio graph?
  m_callback();

  // A prorgam wrote some data to the streams bus so should be flushed out
  if (m_buses.is_dirty(m_bus)) {
    // Read the data on the bus back and send to the sound card
    for (int channel = 0; channel < m_settings.channels; ++channel) {
      auto samples = m_buses.read(m_bus, channel);
      for (int s = 0; s < m_settings.blocksize; ++s) {
        output[channel][s] = samples[s];
      }
    }
  }
}

bool AudioBackendPortAudio::stop() {
  if (m_stream == nullptr) {
    return true;
  }

  while (m_state == PENDING) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }

  auto error = Pa_StopStream(m_stream);
  if (error == paNoError) {
    m_state = STOPPED;
    return true;
  }

  m_state = ERROR;
  return false;
}

bool AudioBackendPortAudio::close() {
  auto success = true;
  if (m_stream != nullptr) {
    success = Pa_CloseStream(m_stream) == paNoError;
  }

  Pa_Terminate();
  return success;
}
