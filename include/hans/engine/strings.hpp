#ifndef HANS_ENGINE_STRINGS_H_
#define HANS_ENGINE_STRINGS_H_

#include <vector>
#include "hans/engine/linear_allocator.hpp"
#include "hans/engine/primitives.hpp"

namespace hans {
namespace engine {

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

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_STRINGS_H_
