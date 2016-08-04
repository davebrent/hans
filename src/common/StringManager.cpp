#include "hans/common/StringManager.hpp"
#include <algorithm>
#include <cassert>
#include <cstring>
#include "hans/common/hasher.hpp"

using namespace hans;
using namespace hans::common;

StringManager::StringManager(size_t size) : m_allocator(size) {
}

StringManager::StringManager(ListView<hash> hashes, ListView<size_t> offsets,
                             ListView<const char> data) {
  auto bytes = data.size();

  m_allocator.reset(bytes);
  auto dest = m_allocator.allocate(bytes);
  std::memcpy(dest, static_cast<void*>(const_cast<char*>(&data[0])), bytes);
  auto base = static_cast<char*>(m_allocator.start());

  auto length = hashes.size();
  for (auto i = 0; i < length; ++i) {
    auto offset = offsets[i];
    m_hashes.push_back(hashes[i]);
    m_strings.push_back(&base[offset]);
  }
}

hash StringManager::intern(const char* string) {
  // Check if we have seen the string before
  size_t len = strlen(string);
  auto hashed = hasher(string);

  auto end = m_hashes.end();
  auto it = std::find(m_hashes.begin(), end, hashed);
  if (it != end) {
    return hashed;
  }

  // Calculate the space needed to copy the string

  size_t size = sizeof(char) * (len + 1);

  void* ptr = m_allocator.allocate(size);
  if (ptr == nullptr) {
    return hashed;
  }

  // Perform the copy
  void* src = static_cast<void*>(const_cast<char*>(string));
  std::memcpy(ptr, src, size);
  static_cast<char*>(ptr)[len] = '\0';

  // Store the hash and a reference to the copied string
  m_strings.push_back(static_cast<const char*>(ptr));
  m_hashes.push_back(hashed);
  return hashed;
}

const char* StringManager::lookup(hash hashed) const {
  auto begin = m_hashes.begin();
  auto end = m_hashes.end();
  auto it = std::find(begin, end, hashed);
  if (it != end) {
    return m_strings.at(it - begin);
  }
  return nullptr;
}
