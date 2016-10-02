#ifndef HANS_ENGINE_AUDIOBUFFERMANAGER_H_
#define HANS_ENGINE_AUDIOBUFFERMANAGER_H_

#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/engine/types.hpp"

namespace hans {
namespace engine {

class AudioBufferManager {
 public:
  explicit AudioBufferManager(common::ListView<audio::Buffer> buffers);
  audio::Buffer make(engine::ObjectDef::ID id, hash name);
  audio::sample* get(const audio::Buffer& buff, uint8_t channel) const;

 private:
  common::ListView<audio::Buffer> m_buffers;
  char* m_base = nullptr;
  hans::common::LinearAllocator m_allocator;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_AUDIOBUFFERMANAGER_H_
