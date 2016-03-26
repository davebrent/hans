#include "hans/memory/StringManager.hpp"
#include <cassert>
#include <cstring>
#include <algorithm>

using namespace hans;

static uint64_t murmur_hash_64(const void* key, uint32_t len, uint64_t seed) {
  const uint64_t m = 0xc6a4a7935bd1e995ULL;
  const uint32_t r = 47;

  uint64_t h = seed ^ (len * m);

  const uint64_t* data = static_cast<const uint64_t*>(key);
  const uint64_t* end = data + (len / 8);

  while (data != end) {
    uint64_t k = *data++;

    k *= m;
    k ^= k >> r;
    k *= m;

    h ^= k;
    h *= m;
  }

  const unsigned char* data2 = reinterpret_cast<const unsigned char*>(data);

  switch (len & 7) {
  case 7:
    h ^= uint64_t(data2[6]) << 48;
  case 6:
    h ^= uint64_t(data2[5]) << 40;
  case 5:
    h ^= uint64_t(data2[4]) << 32;
  case 4:
    h ^= uint64_t(data2[3]) << 24;
  case 3:
    h ^= uint64_t(data2[2]) << 16;
  case 2:
    h ^= uint64_t(data2[1]) << 8;
  case 1:
    h ^= uint64_t(data2[0]);
    h *= m;
  };

  h ^= h >> r;
  h *= m;
  h ^= h >> r;
  return h;
}

memory::StringManager::StringManager(size_t size) : m_allocator(size) {
}

hans_hash memory::StringManager::intern(const char* string) {
  // Check if we have seen the string before
  size_t len = strlen(string);
  hans_hash hash = murmur_hash_64(string, len * sizeof(char), 0);

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

hans_hash memory::StringManager::intern(const unsigned char* string) {
  return intern(reinterpret_cast<const char*>(string));
}

hans_hash memory::StringManager::intern(const std::string string) {
  return intern(string.c_str());
}

const char* memory::StringManager::lookup(const hans_hash& hash) const {
  auto begin = m_hashes.begin();
  auto end = m_hashes.end();
  auto it = std::find(begin, end, hash);
  if (it != end) {
    return m_strings.at(it - begin);
  }
  return nullptr;
}
