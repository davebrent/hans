#include "hans/common/StringManager.hpp"
#include <algorithm>
#include <cassert>
#include <cstring>
#include "hans/common/hasher.hpp"

using namespace hans;

common::StringManager::StringManager(size_t size) : m_allocator(size) {
}

common::StringManager::StringManager(common::ListView<hans_hash> hashes,
                                     common::ListView<size_t> offsets,
                                     common::ListView<const char> data) {
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

hans_hash common::StringManager::intern(const char* string) {
  // Check if we have seen the string before
  size_t len = strlen(string);
  hans_hash hash = common::hasher(string);

  auto end = m_hashes.end();
  auto it = std::find(m_hashes.begin(), end, hash);
  if (it != end) {
    return hash;
  }

  // Calculate the space needed to copy the string

  size_t size = sizeof(char) * (len + 1);

  void* ptr = m_allocator.allocate(size);
  assert(ptr != nullptr && "Out of memory");

  // Perform the copy
  void* src = static_cast<void*>(const_cast<char*>(string));
  std::memcpy(ptr, src, size);
  static_cast<char*>(ptr)[len] = '\0';

  // Store the hash and a reference to the copied string
  m_strings.push_back(static_cast<const char*>(ptr));
  m_hashes.push_back(hash);
  return hash;
}

const char* common::StringManager::lookup(const hans_hash& hash) const {
  auto begin = m_hashes.begin();
  auto end = m_hashes.end();
  auto it = std::find(begin, end, hash);
  if (it != end) {
    return m_strings.at(it - begin);
  }
  return nullptr;
}
