#ifndef HANS_AUDIOBUFFERS_H_
#define HANS_AUDIOBUFFERS_H_

#include <vector>
#include "hans/linear_allocator.hpp"
#include "hans/primitives.hpp"

namespace hans {

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

} // namespace hans

#endif // HANS_AUDIOBUFFERS_H_
