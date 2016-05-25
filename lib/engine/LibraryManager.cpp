#include "hans/engine/LibraryManager.hpp"
#include <dlfcn.h>
#include <algorithm>

using namespace hans;

static bool hans_register(hans_library_api* api, const char* name, size_t size,
                          hans_new_object make, hans_del_object destroy) {
  auto library_manager = static_cast<engine::LibraryManager*>(api->data);
  return library_manager->register_object(name, size, make, destroy);
}

engine::LibraryManager::LibraryManager(common::StringManager& string_manager,
                                       std::vector<hans_object>& objects)
    : m_string_manager(string_manager), m_objects(objects) {
}

engine::LibraryManager::~LibraryManager() {
  for (auto& handle : m_handles) {
    dlclose(handle);
  }
}

void engine::LibraryManager::load_libraries(
    std::vector<hans_library>& libraries) {
  hans_library_api api = {.register_object = hans_register, .data = this};

  for (auto& library : libraries) {
    const char* filepath = m_string_manager.lookup(library.filepath);
    library.handle = dlopen(filepath, RTLD_NOW);

    if (library.handle != nullptr) {
      m_handles.push_back(library.handle);
      void* symbol = dlsym(library.handle, "setup");

      if (symbol != nullptr) {
        ((hans_module_setup)symbol)(&api);
      }
    }
  }
}

bool engine::LibraryManager::register_object(const char* name, size_t size,
                                             hans_new_object make,
                                             hans_del_object destroy) {
  auto hash = m_string_manager.intern(name);
  auto found = false;

  for (int i = 0; i < m_objects.size(); ++i) {
    hans_object& object = m_objects.at(i);
    if (object.name == hash) {
      object.size = size;
      object.make = make;
      object.destroy = destroy;
      found = true;
    }
  }

  return found;
}

std::vector<hans_object> engine::LibraryManager::filter_objects(
    hans_object_type type) {
  std::vector<hans_object> objects(m_objects.size());
  auto it = std::copy_if(m_objects.begin(), m_objects.end(), objects.begin(),
                         [&](auto& object) {
                           return object.type == type && object.make != nullptr;
                         });
  objects.resize(std::distance(objects.begin(), it));
  return objects;
}
