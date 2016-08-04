#ifndef HANS_COMMON_LISTVIEW_H_
#define HANS_COMMON_LISTVIEW_H_

#include <stddef.h>
#include "hans/common/types.hpp"

namespace hans {
namespace common {

template <typename T>
class ListView {
 public:
  explicit ListView(const DataFile::Blob& blob) {
    m_length = blob.size / sizeof(T);
    m_ptr = static_cast<T*>(blob.data);
  }

  ListView(T* ptr, size_t length) {
    m_length = length;
    m_ptr = ptr;
  }

  ListView() {
    m_length = 0;
    m_ptr = nullptr;
  }

  size_t size() const {
    return m_length;
  }

  T& operator[](const int index) const {
    return m_ptr[index];
  }

  class iterator {
   public:
    explicit iterator(T* ptr) {
      m_base = ptr;
    }

    bool operator==(const iterator& rhs) {
      return m_base == rhs.m_base;
    }

    bool operator!=(const iterator& rhs) {
      return m_base != rhs.m_base;
    }

    iterator& operator++() {
      m_base =
          reinterpret_cast<T*>(reinterpret_cast<char*>(m_base) + sizeof(T));
      return *this;
    }

    T& operator*() {
      return *m_base;
    }

   private:
    T* m_base;
  };

  iterator begin() const {
    return iterator(m_ptr);
  }

  iterator end() const {
    return iterator(&m_ptr[m_length]);
  }

 private:
  T* m_ptr;
  size_t m_length;
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_LISTVIEW_H_
