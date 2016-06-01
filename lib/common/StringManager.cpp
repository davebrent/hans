#include "hans/common/StringManager.hpp"
#include <algorithm>
#include <cassert>
#include <cstring>
#include "hans/common/hasher.hpp"

using namespace hans;

common::StringManager::StringManager(size_t size) : m_allocator(size) {
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

hans_hash common::StringManager::intern(const unsigned char* string) {
  return intern(reinterpret_cast<const char*>(string));
}

hans_hash common::StringManager::intern(const std::string string) {
  return intern(string.c_str());
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

void* common::StringManager::start() const {
  return m_allocator.start();
}

void* common::StringManager::end() const {
  return m_allocator.end();
}
