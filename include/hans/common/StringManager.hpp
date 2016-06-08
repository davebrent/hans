#ifndef HANS_COMMON_STRINGMANAGER_H_
#define HANS_COMMON_STRINGMANAGER_H_

#include <vector>
#include "hans/common/LinearAllocator.hpp"
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace common {

class StringManager {
 public:
  explicit StringManager(size_t size);
  StringManager(hans::common::ListView<hans_hash> hashes,
                hans::common::ListView<size_t> offsets,
                hans::common::ListView<const char> data);

  /// Intern a string and retrieve its hash
  hans_hash intern(const char* string);
  /// Return the original string for a given hash
  const char* lookup(const hans_hash& hash) const;

 private:
  common::LinearAllocator m_allocator;
  std::vector<hans_hash> m_hashes;
  std::vector<const char*> m_strings;
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_STRINGMANAGER_H_
