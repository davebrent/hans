#include "hans/audio/AudioBusManager.hpp"
#include <stdexcept>

using namespace hans;

audio::AudioBusManager::AudioBusManager(const hans_config& config,
                                        size_t num) {
  auto blocksize = config.blocksize;
  auto channels = config.channels;
  auto bytes = num * channels * blocksize * sizeof(hans_audio_sample);
  m_allocator.reset(bytes);
  auto data = m_allocator.allocate(bytes, alignof(hans_audio_sample));

  m_base = static_cast<char*>(data);
  m_ids = 0;
  m_channels = channels;
  m_blocksize = blocksize;
  m_max = num;

  m_revisions = new uint64_t[num];
}

hans_audio_bus_handle audio::AudioBusManager::make() {
  if (m_ids == m_max) {
    throw std::runtime_error("AudioBusManager: Exceeded number of buses");
  }

  hans_audio_bus_handle handle = m_ids;
  m_ids++;
  return handle;
}

bool audio::AudioBusManager::write(hans_audio_bus_handle handle,
                                   uint8_t channel,
                                   const hans_audio_sample* samples) {
  auto size = sizeof(hans_audio_sample);
  auto offset = handle * m_blocksize * m_channels * size;
  auto block = m_base + offset + (m_blocksize * channel * size);
  m_revisions[handle]++;
  std::memcpy(block, samples, size * m_blocksize);
  return true;
}

hans_audio_sample* audio::AudioBusManager::read(hans_audio_bus_handle handle,
                                                uint8_t channel) {
  auto size = sizeof(hans_audio_sample);
  auto offset = handle * m_blocksize * m_channels * size;
  auto block = m_base + offset + (m_blocksize * channel * size);
  return reinterpret_cast<hans_audio_sample*>(block);
}

bool audio::AudioBusManager::is_dirty(hans_audio_bus_handle handle) {
  return m_revisions[handle] != 0;
}

void audio::AudioBusManager::set_clean(hans_audio_bus_handle handle) {
  m_revisions[handle] = 0;
}
