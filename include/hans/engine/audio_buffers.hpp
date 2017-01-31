#ifndef HANS_ENGINE_AUDIOBUFFERS_H_
#define HANS_ENGINE_AUDIOBUFFERS_H_

#include <vector>
#include "hans/engine/linear_allocator.hpp"
#include "hans/engine/primitives.hpp"

namespace hans {
namespace engine {

class AudioBuffers {
 public:
  AudioBuffers(const AudioBuffers& other) = delete;
  explicit AudioBuffers(std::vector<audio::Buffer>& buffers);
  audio::Buffer make(ObjectDef::ID id, hash name);
  audio::sample* get(const audio::Buffer& buff, uint8_t channel) const;

 private:
  std::vector<audio::Buffer>& m_buffers;
  char* m_base = nullptr;
  LinearAllocator m_allocator;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIOBUFFERS_H_
