#ifndef HANS_LIBRARYMANAGER_H_
#define HANS_LIBRARYMANAGER_H_

#include <functional>
#include <iostream>
#include <vector>
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/types.hpp"

namespace hans {
namespace engine {

class LibraryManager {
 public:
  LibraryManager(common::StringManager& string_manager,
                 common::ListView<ObjectDef>& objects);
  ~LibraryManager();
  void load(const common::ListView<Library>& libraries);

  template <typename State, typename Object>
  bool add_object(const char* name) {
    auto size = sizeof(State);

    auto create = [](ObjectDef::ID id, void* state) {
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
        object.create = create;
        object.serialize = serialize;
        object.destroy = destroy;
        found = true;
      }
    }

    return found;
  }

 private:
  common::StringManager& m_string_manager;
  std::vector<void*> m_handles;
  ObjectDef* m_objects;
  size_t m_length;
};

} // namespace engine
} // namespace hans

#endif // HANS_LIBRARYMANAGER_H_
