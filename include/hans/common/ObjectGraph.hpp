#ifndef HANS_COMMON_OBJECTGRAPH_H_
#define HANS_COMMON_OBJECTGRAPH_H_

#include "hans/common/types.hpp"

namespace hans {
namespace common {

class ObjectGraph {
 public:
  template <class T>
  class PropertyList {
   public:
    typedef T* iterator;
    typedef const T* const_iterator;

    PropertyList(T* array, uint32_t size) {
      m_array = array;
      m_size = size;
    }

    iterator begin() {
      return iterator(m_array);
    }

    iterator end() {
      return iterator(&m_array[m_size]);
    }

    T* get() {
      return m_array;
    }

    uint32_t size() {
      return m_size;
    }

   private:
    T* m_array;
    uint32_t m_size;
  };

  typedef PropertyList<hans_user_object> ObjectList;
  typedef PropertyList<hans_object_connection> ConnectionList;
  typedef PropertyList<hans_user_arg> ArgumentList;

  /// A user requested graph representation
  typedef struct {
    size_t bytes;
    uint32_t objects_len;
    uint32_t connections_len;
    uint32_t arguments_len;
    hans_user_object* objects;
    hans_object_connection* connections;
    hans_user_arg* arguments;
  } data;

  ObjectGraph();
  ObjectGraph(uint32_t objects, uint32_t connections, uint32_t arguments);
  explicit ObjectGraph(data* graph);
  ObjectGraph(const ObjectGraph& other) = delete;
  ~ObjectGraph();

  void clear();
  bool copy(const ObjectGraph& other);

  ObjectList get_objects() const;
  ConnectionList get_connections() const;
  ArgumentList get_arguments() const;

  hans_user_object* object_at(uint32_t index) const;
  hans_object_connection* connection_at(uint32_t index) const;
  hans_user_arg* argument_at(uint32_t index) const;

  bool topological_sort();
  void sort_edges();

 private:
  ObjectGraph::data* m_data;
};

} // namespace common
} // namespace hans

#endif // HANS_COMMON_OBJECTGRAPH_H_
