#ifndef HANS_MEMORY_LINEARALLOCATOR_H_
#define HANS_MEMORY_LINEARALLOCATOR_H_

#include <stddef.h>

namespace hans {
namespace memory {

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

} // namespace memory
} // namespace hans

#endif // HANS_MEMORY_LINEARALLOCATOR_H_
