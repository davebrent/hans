#include "hans/common/LinearAllocator.hpp"
#include <cassert>
#include <cstdlib>
#include <memory>

using namespace hans;

common::LinearAllocator::LinearAllocator() {
  m_start = nullptr;
  m_current = nullptr;
  m_end = nullptr;
}

common::LinearAllocator::LinearAllocator(size_t size) {
  m_start = nullptr;
  reset(size);
}

common::LinearAllocator::~LinearAllocator() {
  std::free(m_start);
}

void* common::LinearAllocator::allocate(size_t size, size_t alignment) {
  void* current = static_cast<void*>(m_current);
  size_t space = m_end - m_current;
  void* aligned = std::align(alignment, size, current, space);

  m_current = static_cast<char*>(aligned);

  void* user_ptr = m_current;
  m_current += size;
  if (m_current > m_end) {
    return nullptr;
  }

  return user_ptr;
}

void* common::LinearAllocator::allocate(size_t size) {
  return allocate(size, 1);
}

void common::LinearAllocator::reset() {
  m_current = m_start;
}

void common::LinearAllocator::reset(size_t size) {
  std::free(m_start);
  m_start = static_cast<char*>(std::calloc(1, size));
  assert(m_start != nullptr && "Out of memory");

  m_end = m_start + size;
  m_current = m_start;
}

void* common::LinearAllocator::start() const {
  return m_start;
}

void* common::LinearAllocator::end() const {
  return m_current;
}
