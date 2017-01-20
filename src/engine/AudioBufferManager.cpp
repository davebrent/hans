#include "hans/engine/AudioBufferManager.hpp"
#include <cstdlib>
#include <stdexcept>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

AudioBufferManager::AudioBufferManager(std::vector<audio::Buffer>& buffers)
    : m_buffers(buffers) {
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

audio::Buffer AudioBufferManager::make(ObjectDef::ID id, hash name) {
  for (const auto& buffer : m_buffers) {
    if (buffer.object == id && buffer.name == name) {
      return buffer;
    }
  }
  throw std::runtime_error("Unknown audio buffer");
}

audio::sample* AudioBufferManager::get(const audio::Buffer& buff,
                                       uint8_t channel) const {
  char* base = m_base + buff.offset;
  return reinterpret_cast<audio::sample*>(
      base + (sizeof(audio::sample) * buff.size * channel));
}
