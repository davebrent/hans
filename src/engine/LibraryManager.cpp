#include "hans/engine/LibraryManager.hpp"
#include <dlfcn.h>
#include <iostream>

using namespace hans;

engine::LibraryManager::LibraryManager(common::StringManager& string_manager,
                                       common::ListView<hans_object>& objects)
    : m_string_manager(string_manager) {
  m_objects = &objects[0];
  m_length = objects.size();
}

engine::LibraryManager::~LibraryManager() {
  for (auto& handle : m_handles) {
    dlclose(handle);
  }
}

static bool hans_register_object(hans_library_api* api, const char* name,
                                 size_t size, hans_new_object make,
                                 hans_init_object init,
                                 hans_del_object destroy) {
  auto library_manager = static_cast<engine::LibraryManager*>(api->data);
  return library_manager->declare(name, size, make, init, destroy);
}

void engine::LibraryManager::load(
    const common::ListView<hans_library>& libraries) {
  hans_library_api api = {.register_object = hans_register_object,
                          .data = this};
  for (auto i = 0; i < libraries.size(); ++i) {
    auto library = libraries[i];
    auto filepath = m_string_manager.lookup(library.filepath);
    auto handle = dlopen(filepath, RTLD_NOW);

    if (handle != nullptr) {
      m_handles.push_back(handle);
      auto symbol = dlsym(handle, "setup");
      if (symbol != nullptr) {
        ((hans_module_setup)symbol)(&api);
      }
    } else {
      std::cerr << "Library error: " << dlerror() << std::endl;
    }
  }
}

bool engine::LibraryManager::declare(const char* name, size_t size,
                                     hans_new_object make,
                                     hans_init_object init,
                                     hans_del_object destroy) {
  auto hash = m_string_manager.intern(name);
  auto found = false;
  auto objects = m_objects;

  for (int i = 0; i < m_length; ++i) {
    auto& object = objects[i];
    if (object.name == hash) {
      object.size = size;
      object.make = make;
      object.init = init;
      object.destroy = destroy;
      found = true;
    }
  }

  return found;
}
