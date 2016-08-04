#include "hans/audio/AudioBufferManager.hpp"
#include <cstdlib>
#include <stdexcept>

using namespace hans;
using namespace hans::audio;
using namespace hans::common;
using namespace hans::engine;

AudioBufferManager::AudioBufferManager(ListView<Buffer>& buffers) {
  m_buffers = &buffers[0];
  m_buffers_len = buffers.size();

  size_t total = 0;

  for (auto& buffer : buffers) {
    auto samples = buffer.size * buffer.channels;
    auto bytes = sizeof(audio::sample) * samples;
    buffer.offset = total;
    total += bytes;
  }

  m_allocator.reset(total);
  auto data = m_allocator.allocate(total, alignof(audio::sample));
  m_base = static_cast<char*>(data);
}

Buffer AudioBufferManager::make(ObjectDef::ID id, hash name) {
  auto& buffers = m_buffers;
  for (auto i = 0; i < m_buffers_len; ++i) {
    auto& buffer = buffers[i];
    if (buffer.object == id && buffer.name == name) {
      return buffer;
    }
  }
  throw std::runtime_error("Unknown audio buffer");
}

sample* AudioBufferManager::get(const Buffer& buff, uint8_t channel) const {
  char* base = m_base + buff.offset;
  return reinterpret_cast<audio::sample*>(
      base + (sizeof(audio::sample) * buff.size * channel));
}
