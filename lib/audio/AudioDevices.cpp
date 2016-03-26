#include "hans/audio/AudioDevices.hpp"
#include <stdexcept>
#include <vector>
#include <portaudio.h>

using namespace hans;

audio::AudioDevices::AudioDevices() {
  PaError error = Pa_Initialize();

  if (error != paNoError) {
    throw std::runtime_error(Pa_GetErrorText(error));
  }

  auto num_devices = Pa_GetDeviceCount();
  m_devices.reserve(num_devices);

  auto default_output = Pa_GetDefaultOutputDevice();
  auto default_input = Pa_GetDefaultInputDevice();

  for (unsigned i = 0; i < num_devices; ++i) {
    auto info = Pa_GetDeviceInfo(i);

    hans_audio_device device;
    device.id = i;
    device.name = info->name;
    device.max_input_channels = info->maxInputChannels;
    device.max_output_channels = info->maxOutputChannels;
    device.default_input = default_input == i;
    device.default_output = default_output == i;

    m_devices.push_back(device);
  }
}

audio::AudioDevices::~AudioDevices() {
  Pa_Terminate();
}

audio::AudioDevices::Iterator::Iterator(
    typename std::vector<hans_audio_device>::iterator c)
    : std::vector<hans_audio_device>::iterator(c) {
}

audio::AudioDevices::Iterator audio::AudioDevices::begin() {
  return Iterator(m_devices.begin());
}

audio::AudioDevices::Iterator audio::AudioDevices::end() {
  return Iterator(m_devices.end());
}

std::vector<hans_audio_device> audio::AudioDevices::get_devices() {
  return m_devices;
}
