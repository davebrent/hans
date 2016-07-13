#include "hans/audio/AudioBufferManager.hpp"
#include <cstdlib>
#include <stdexcept>

using namespace hans;

audio::AudioBufferManager::AudioBufferManager(
    common::ListView<hans_audio_buffer>& buffers) {
  m_buffers = &buffers[0];
  m_buffers_len = buffers.size();

  size_t total = 0;

  for (auto& buffer : buffers) {
    auto samples = buffer.size * buffer.channels;
    auto bytes = sizeof(hans_audio_sample) * samples;
    buffer.offset = total;
    total += bytes;
  }

  m_allocator.reset(total);
  auto data = m_allocator.allocate(total, alignof(hans_audio_sample));
  m_base = static_cast<char*>(data);
}

hans_audio_buffer audio::AudioBufferManager::make(hans_instance_id id,
                                                  hans_hash name) {
  auto& buffers = m_buffers;
  for (auto i = 0; i < m_buffers_len; ++i) {
    auto& buffer = buffers[i];
    if (buffer.object == id && buffer.name == name) {
      return buffer;
    }
  }
  throw std::runtime_error("Unknown audio buffer");
}

hans_audio_sample* audio::AudioBufferManager::get(const hans_audio_buffer& buff,
                                                  uint8_t channel) const {
  char* base = m_base + buff.offset;
  return reinterpret_cast<hans_audio_sample*>(
      base + (sizeof(hans_audio_sample) * buff.size * channel));
}
