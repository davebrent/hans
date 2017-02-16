#ifndef HANS_STRINGS_H_
#define HANS_STRINGS_H_

#include <vector>
#include "hans/linear_allocator.hpp"
#include "hans/primitives.hpp"

namespace hans {

class StringManager {
 public:
  explicit StringManager(size_t size);
  explicit StringManager(const Strings& strings);
  StringManager(const StringManager& other) = delete;

  /// Intern a string and retrieve its hash
  hash intern(const char* string);

  /// Return the original string for a given hash
  const char* lookup(hash hashed) const;

 private:
  LinearAllocator m_allocator;
  std::vector<hash> m_hashes;
  std::vector<const char*> m_strings;
};

} // namespace hans

#endif // HANS_STRINGS_H_
