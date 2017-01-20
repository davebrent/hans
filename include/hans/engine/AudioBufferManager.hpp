#ifndef HANS_ENGINE_AUDIOBUFFERMANAGER_H_
#define HANS_ENGINE_AUDIOBUFFERMANAGER_H_

#include <vector>
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

class AudioBufferManager {
 public:
  AudioBufferManager(const AudioBufferManager& other) = delete;
  explicit AudioBufferManager(std::vector<audio::Buffer>& buffers);
  audio::Buffer make(ObjectDef::ID id, hash name);
  audio::sample* get(const audio::Buffer& buff, uint8_t channel) const;

 private:
  std::vector<audio::Buffer>& m_buffers;
  char* m_base = nullptr;
  hans::common::LinearAllocator m_allocator;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIOBUFFERMANAGER_H_
