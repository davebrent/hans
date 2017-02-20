#include "./audio_backend_portaudio.hpp"
#ifdef PORTAUDIO_FOUND
#include <algorithm>
#include <chrono>
#include <stdexcept>
#include <thread>
#include "hans/hasher.hpp"

using namespace hans;

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

  auto input_device = -1;
  auto output_device = -1;
  auto num_devices = Pa_GetDeviceCount();
  for (auto i = 0; i < num_devices; i++) {
    auto name = hasher(Pa_GetDeviceInfo(i)->name);
    if (name == _settings.input_device) {
      input_device = i;
    }
    if (name == _settings.output_device) {
      output_device = i;
    }
  }

  if (input_device == -1) {
    throw std::runtime_error("Hans unable to find audio device");
  }

  _state = STOPPED;

  PaStreamParameters input;
  input.device = input_device;
  input.channelCount = (*std::max_element(_settings.input_channels.begin(),
                                          _settings.input_channels.end()));
  input.channelCount += 1;
  input.sampleFormat = paFloat32 | paNonInterleaved;
  input.hostApiSpecificStreamInfo = nullptr;

  PaStreamParameters output;
  output.device = output_device;
  output.channelCount = (*std::max_element(_settings.output_channels.begin(),
                                           _settings.output_channels.end()));
  output.channelCount += 1;
  output.sampleFormat = paFloat32 | paNonInterleaved;
  output.hostApiSpecificStreamInfo = nullptr;

  _bus = _buses.make();

  auto samplerate = _settings.samplerate;
  auto blocksize = _settings.blocksize;

  error = Pa_OpenStream(&_stream, &input, &output, samplerate, blocksize,
                        paClipOff, &_portaudio_callback, this);
  if (error == paNoError) {
    return true;
  }

  _state = ERROR;
  return false;
}

bool AudioBackendPortAudio::start() {
  if (_stream == nullptr) {
    return false;
  }

  _state = PENDING;

  auto error = Pa_StartStream(_stream);
  if (error == paNoError) {
    return true;
  }

  _state = ERROR;
  return false;
}

void AudioBackendPortAudio::callback(const audio::sample** input,
                                     audio::sample** output) {
  if (_state == PENDING) {
    _state = STARTED;
  }

  if (_state == STOPPED) {
    return;
  }

  // Write input data to the default audio bus, channel by channel
  for (auto i = 0; i < _settings.input_channels.size(); ++i) {
    _buses.write(_bus, i, input[_settings.input_channels[i]]);
  }

  _buses.set_clean(_bus);

  // Tick the audio graph
  // XXX: Maybe use a message/signal thats executed immediately to run a cycle
  //      of the audio graph?
  _callback();

  // A prorgam wrote some data to the streams bus so should be flushed out
  if (_buses.is_dirty(_bus)) {
    // Read the data on the bus back and send to the sound card
    for (auto c = 0; c < _settings.output_channels.size(); ++c) {
      auto samples = _buses.read(_bus, c);
      for (int s = 0; s < _settings.blocksize; ++s) {
        output[_settings.output_channels[c]][s] = samples[s];
      }
    }
  }
}

bool AudioBackendPortAudio::stop() {
  if (_stream == nullptr) {
    return true;
  }

  while (_state == PENDING) {
    std::this_thread::sleep_for(std::chrono::milliseconds(1));
  }

  auto error = Pa_StopStream(_stream);
  if (error == paNoError) {
    _state = STOPPED;
    return true;
  }

  _state = ERROR;
  return false;
}

bool AudioBackendPortAudio::close() {
  auto success = true;
  if (_stream != nullptr) {
    success = Pa_CloseStream(_stream) == paNoError;
  }

  Pa_Terminate();
  return success;
}

#endif // PORTAUDIO_FOUND
