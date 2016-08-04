#include "hans/audio/AudioBusManager.hpp"
#include <cstring>
#include <stdexcept>

using namespace hans;
using namespace hans::audio;
using namespace hans::common;

AudioBusManager::AudioBusManager(const Config& config, size_t num) {
  auto blocksize = config.blocksize;
  auto channels = config.channels;
  auto bytes = num * channels * blocksize * sizeof(audio::sample);
  m_allocator.reset(bytes);
  auto data = m_allocator.allocate(bytes, alignof(audio::sample));

  m_base = static_cast<char*>(data);
  m_ids = 0;
  m_channels = channels;
  m_blocksize = blocksize;
  m_max = num;

  m_revisions = new uint64_t[num];
}

bus_handle AudioBusManager::make() {
  if (m_ids == m_max) {
    throw std::runtime_error("AudioBusManager: Exceeded number of buses");
  }

  auto handle = m_ids;
  m_ids++;
  return handle;
}

bool AudioBusManager::write(bus_handle handle, uint8_t channel,
                            const audio::sample* samples) {
  auto size = sizeof(audio::sample);
  auto offset = handle * m_blocksize * m_channels * size;
  auto block = m_base + offset + (m_blocksize * channel * size);
  m_revisions[handle]++;
  std::memcpy(block, samples, size * m_blocksize);
  return true;
}

audio::sample* AudioBusManager::read(bus_handle handle, uint8_t channel) {
  auto size = sizeof(audio::sample);
  auto offset = handle * m_blocksize * m_channels * size;
  auto block = m_base + offset + (m_blocksize * channel * size);
  return reinterpret_cast<audio::sample*>(block);
}

bool AudioBusManager::is_dirty(bus_handle handle) {
  return m_revisions[handle] != 0;
}

void AudioBusManager::set_clean(bus_handle handle) {
  m_revisions[handle] = 0;
}
