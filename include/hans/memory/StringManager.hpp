#ifndef HANS_MEMORY_STRINGMANAGER_H_
#define HANS_MEMORY_STRINGMANAGER_H_

#include <string>
#include <vector>
#include "hans/common/types.hpp"
#include "hans/memory/LinearAllocator.hpp"

namespace hans {
namespace memory {

class StringManager {
 public:
  explicit StringManager(size_t size);

  /// Intern a string and retrieve its hash
  hans_hash intern(const char* string);
  hans_hash intern(const unsigned char* string);
  hans_hash intern(const std::string string);

  /// Return the original string for a given hash
  const char* lookup(const hans_hash& hash) const;
  void* start() const;
  void* end() const;

 private:
  memory::LinearAllocator m_allocator;
  std::vector<hans_hash> m_hashes;
  std::vector<const char*> m_strings;
};

} // namespace memory
} // namespace hans

#endif // HANS_MEMORY_STRINGMANAGER_H_
