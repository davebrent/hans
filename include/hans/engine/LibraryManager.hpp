#ifndef HANS_LIBRARYMANAGER_H_
#define HANS_LIBRARYMANAGER_H_

#include <functional>
#include <iostream>
#include <vector>
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

class LibraryManager {
 public:
  LibraryManager(hans::common::StringManager& string_manager,
                 hans::common::ListView<hans_object>& objects);
  ~LibraryManager();
  void load(const common::ListView<hans_library>& libraries);

  template <typename State, typename Object>
  bool add_object(const char* name) {
    auto size = sizeof(State);

    auto make = [](hans_instance_id id, void* state) {
      auto instance = new Object(id);
      instance->state = *reinterpret_cast<State*>(state);
      return instance;
    };

    auto destroy = [](void* instance) {
      delete static_cast<Object*>(instance);
    };

    auto serialize = [](void* instance) {
      auto obj = static_cast<Object*>(instance);
      return static_cast<void*>(&obj->state);
    };

    auto found = false;
    auto hash = m_string_manager.intern(name);
    auto objects = m_objects;

    for (int i = 0; i < m_length; ++i) {
      auto& object = objects[i];
      if (object.name == hash) {
        object.size = size;
        object.make = make;
        object.serialize = serialize;
        object.destroy = destroy;
        found = true;
      }
    }

    return found;
  }

 private:
  hans::common::StringManager& m_string_manager;
  std::vector<void*> m_handles;
  hans_object* m_objects;
  size_t m_length;
};

} // namespace engine
} // namespace hans

#endif // HANS_LIBRARYMANAGER_H_
