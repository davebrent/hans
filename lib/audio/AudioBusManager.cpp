#include "hans/audio/AudioBusManager.hpp"
#include <cassert>

using namespace hans;

audio::AudioBusManager::AudioBusManager(
    audio::AudioBufferManager& buffer_manager)
    : m_buffer_manager(buffer_manager) {
  m_ids = 0;
}

hans_audio_bus_handle audio::AudioBusManager::make(uint8_t channels,
                                                   uint16_t blocksize) {
  hans_audio_buffer* bus = m_buffer_manager.create(channels, blocksize, 1);
  assert(bus != nullptr);
  m_buses.push_back(bus);

  hans_audio_bus_handle handle = m_ids;
  m_ids++;
  return handle;
}

bool audio::AudioBusManager::write(hans_audio_bus_handle handle,
                                   uint8_t channel,
                                   const hans_audio_sample* samples) {
  assert(samples != nullptr);
  if (handle >= m_buses.size() || handle < 0) {
    return false;
  }

  hans_audio_buffer* bus = m_buses.at(handle);
  assert(channel < bus->channels);
  hans_audio_sample* dest = bus->samples[channel];

  for (int i = 0; i < bus->samples_len; ++i) {
    dest[i] = samples[i];
  }

  return true;
}

hans_audio_sample* audio::AudioBusManager::read(hans_audio_bus_handle handle,
                                                uint8_t channel) {
  if (handle >= m_buses.size() || handle < 0) {
    return nullptr;
  }

  auto bus = m_buses.at(handle);
  return bus->samples[channel];
}
