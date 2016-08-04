#include "hans/audio/AudioDevices.hpp"
#include <portaudio.h>
#include <stdexcept>
#include <vector>

using namespace hans;
using namespace hans::audio;

AudioDevices::AudioDevices() {
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

    Device device;
    device.id = i;
    device.name = info->name;
    device.max_input_channels = info->maxInputChannels;
    device.max_output_channels = info->maxOutputChannels;
    device.default_input = default_input == i;
    device.default_output = default_output == i;

    m_devices.push_back(device);
  }
}

AudioDevices::~AudioDevices() {
  Pa_Terminate();
}

AudioDevices::Iterator::Iterator(typename std::vector<Device>::iterator c)
    : std::vector<Device>::iterator(c) {
}

AudioDevices::Iterator AudioDevices::begin() {
  return Iterator(m_devices.begin());
}

AudioDevices::Iterator AudioDevices::end() {
  return Iterator(m_devices.end());
}

std::vector<Device> AudioDevices::get_devices() {
  return m_devices;
}
