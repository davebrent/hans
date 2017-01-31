#ifndef HANS_ENGINE_LINEARALLOCATOR_H_
#define HANS_ENGINE_LINEARALLOCATOR_H_

#include <stddef.h>

namespace hans {
namespace engine {

class LinearAllocator {
 public:
  LinearAllocator();
  explicit LinearAllocator(size_t size);
  ~LinearAllocator();

  void* allocate(size_t size, size_t alignment);
  void* allocate(size_t size);

  void reset();
  void reset(size_t size);

  void* start() const;
  void* end() const;

 private:
  char* m_start;
  char* m_current;
  char* m_end;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_LINEARALLOCATOR_H_
