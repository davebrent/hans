#include "hans/audio_buses.hpp"
#include <cstring>
#include <stdexcept>

using namespace hans;

AudioBuses::AudioBuses(const Settings& settings, size_t num) {
  auto blocksize = settings.blocksize;
  auto channels = settings.channels;
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

AudioBuses::~AudioBuses() {
  delete[] m_revisions;
}

audio::bus_handle AudioBuses::make() {
  if (m_ids == m_max) {
    throw std::runtime_error("AudioBuses: Exceeded number of buses");
  }

  auto handle = m_ids;
  m_ids++;
  return handle;
}

bool AudioBuses::write(audio::bus_handle handle, uint8_t channel,
                       const audio::sample* samples) {
  auto size = sizeof(audio::sample);
  auto offset = handle * m_blocksize * m_channels * size;
  auto block = m_base + offset + (m_blocksize * channel * size);
  m_revisions[handle]++;
  std::memcpy(block, samples, size * m_blocksize);
  return true;
}

audio::sample* AudioBuses::read(audio::bus_handle handle, uint8_t channel) {
  auto size = sizeof(audio::sample);
  auto offset = handle * m_blocksize * m_channels * size;
  auto block = m_base + offset + (m_blocksize * channel * size);
  return reinterpret_cast<audio::sample*>(block);
}

bool AudioBuses::is_dirty(audio::bus_handle handle) {
  return m_revisions[handle] != 0;
}

void AudioBuses::set_clean(audio::bus_handle handle) {
  m_revisions[handle] = 0;
}
