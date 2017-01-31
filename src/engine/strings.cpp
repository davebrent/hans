#include "hans/engine/strings.hpp"
#include <algorithm>
#include <cassert>
#include <cstring>
#include <iostream>
#include "hans/engine/hasher.hpp"

using namespace hans;
using namespace hans::engine;

StringManager::StringManager(size_t size) : m_allocator(size) {
}

StringManager::StringManager(const Strings& strings)
    : m_hashes(strings.hashes) {
  auto num_bytes = strings.buffer.size() + strings.lengths.size() + 1;
  m_allocator.reset(num_bytes);

  auto dest = static_cast<char*>(m_allocator.allocate(num_bytes));
  auto src = strings.buffer.c_str();

  m_strings.reserve(strings.lengths.size());
  for (auto& length : strings.lengths) {
    m_strings.push_back(dest);
    std::memcpy(dest, static_cast<void*>(const_cast<char*>(src)), length);
    dest[length] = '\0';
    dest += length + 1;
    src += length;
  }
}

hash StringManager::intern(const char* string) {
  auto len = strlen(string);
  auto hashed = hasher(string);

  auto end = m_hashes.end();
  auto it = std::find(m_hashes.begin(), end, hashed);
  if (it != end) {
    return hashed;
  }

  auto size = sizeof(char) * (len + 1);
  auto ptr = m_allocator.allocate(size);
  if (ptr == nullptr) {
    return hashed;
  }

  auto src = static_cast<void*>(const_cast<char*>(string));
  std::memcpy(ptr, src, size);
  static_cast<char*>(ptr)[len] = '\0';

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
