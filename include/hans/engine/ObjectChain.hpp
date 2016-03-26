#ifndef HANS_ENGINE_OBJECTCHAIN_H_
#define HANS_ENGINE_OBJECTCHAIN_H_

#include "hans/common/types.hpp"
#include "hans/memory/LinearAllocator.hpp"
#include <cassert>
#include <cstring>
#include <vector>
#include <algorithm>

namespace hans {
namespace engine {

template <class T>
class ObjectChain {
 public:
  ObjectChain(const ObjectChain& other) = delete;

  explicit ObjectChain(const std::vector<hans_object>& object_classes)
      : m_object_classes(object_classes) {
    m_largest_size = 0;
  }

  ~ObjectChain() {
    destroy();
  }

  /// Call all object destructors, must be called before libraries are released
  void destroy() {
    if (m_allocator.start() == nullptr) {
      return;
    }

    int i = 0;

    for (auto it = this->begin(); it != this->end(); ++it) {
      auto instance = *it;

      auto begin = m_object_classes.begin();
      auto end = m_object_classes.end();
      auto object_id = m_object_ids.at(i);
      auto class_it = std::find_if(begin, end, [&](const auto& object) {
        return object.id == object_id;
      });

      // This case should never be possible, as the object was constructed
      assert(class_it != end);

      auto object = *class_it;
      if (object.destroy != nullptr) {
        object.destroy(&instance);
      }

      i++;
    }

    m_largest_size = 0;
    m_allocator.reset();
  }

  /// Create enough space for a number of objects consisting of a total size
  bool set_capacity(unsigned num_objects, size_t total_size) {
    destroy();
    m_object_ids.clear();
    m_object_ids.reserve(num_objects);
    // Store the size of each object in the buffer as well
    // | size a, object a ... | size b, object b ... |
    m_allocator.reset((sizeof(size_t) * num_objects) + total_size);
    return true;
  }

  bool create_object(hans_constructor_api* api, hans_object_id object_id,
                     hans_instance_id instance_id) {
    auto end = m_object_classes.end();
    auto it = std::find_if(m_object_classes.begin(), end,
                           [object_id](const hans_object& object) {
                             return object.id == object_id;
                           });
    if (it == end) {
      return false;
    }

    auto object = *it;
    if (object.make == nullptr) {
      return false;
    }

    // Keep track of largest object in the chain for error checking in iterator
    if (object.size > m_largest_size) {
      m_largest_size = object.size;
    }

    auto sos = sizeof(size_t);
    char* buffer = static_cast<char*>(m_allocator.allocate(sos + object.size));
    if (buffer == nullptr) {
      return false;
    }

    std::memcpy(buffer, &object.size, sos);
    buffer += sos;

    // Copy the instance ID into the object
    T* instance = reinterpret_cast<T*>(buffer);
    instance->object_id = object_id;
    instance->instance_id = instance_id;

    // Call the objects constructor and update internal state
    object.make(api, buffer, object.size);
    m_object_ids.push_back(object_id);
    return true;
  }

  class iterator {
   public:
    explicit iterator(char* ptr, unsigned max_size) : m_ptr(ptr) {
      // Advance to the start of the first object
      assert(m_ptr != nullptr);
      m_ptr += sizeof(size_t);
      m_max_size = max_size;
    }

    bool operator==(const iterator& rhs) {
      return m_ptr == rhs.m_ptr;
    }

    bool operator!=(const iterator& rhs) {
      return m_ptr != rhs.m_ptr;
    }

    iterator& operator++() {
      // Look back and see how large the object was and advance
      size_t size = *reinterpret_cast<size_t*>(m_ptr - sizeof(size_t));
      assert(size > 0);
      assert(size <= m_max_size);

      m_ptr += size;
      // Then advance past the size of the object
      m_ptr += sizeof(size_t);
      return *this;
    }

    T& operator*() {
      return *reinterpret_cast<T*>(m_ptr);
    }

   private:
    char* m_ptr;
    unsigned m_max_size;
  };

  iterator begin() {
    return iterator(static_cast<char*>(m_allocator.start()), m_largest_size);
  }

  iterator end() {
    return iterator(static_cast<char*>(m_allocator.end()), m_largest_size);
  }

 private:
  hans::memory::LinearAllocator m_allocator;
  const std::vector<hans_object>& m_object_classes;
  std::vector<hans_object_id> m_object_ids;
  unsigned m_largest_size;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_OBJECTCHAIN_H_
