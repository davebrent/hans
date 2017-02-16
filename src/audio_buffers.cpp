#include "hans/audio_buffers.hpp"
#include <cstdlib>
#include <stdexcept>

using namespace hans;

AudioBuffers::AudioBuffers(std::vector<audio::Buffer>& buffers)
    : m_buffers(buffers) {
  size_t total = 0;
  for (auto& buffer : buffers) {
    total += buffer.size * buffer.channels;
  }

  m_allocator.reset(total * sizeof(audio::sample));
  auto data = m_allocator.allocate(total, alignof(audio::sample));
  m_base = static_cast<char*>(data);
}

audio::Buffer AudioBuffers::make(ObjectDef::ID id, hash name) {
  for (const auto& buffer : m_buffers) {
    if (buffer.object == id && buffer.name == name) {
      return buffer;
    }
  }
  throw std::runtime_error("Unknown audio buffer");
}

audio::sample* AudioBuffers::get(const audio::Buffer& buff,
                                 uint8_t channel) const {
  char* base = m_base + (buff.offset * sizeof(audio::sample));
  return reinterpret_cast<audio::sample*>(
      base + (sizeof(audio::sample) * buff.size * channel));
}
