#include "hans/engine/LibraryManager.hpp"
#include <dlfcn.h>
#include <iostream>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

typedef void (*setup)(LibraryManager*);

LibraryManager::LibraryManager(StringManager& string_manager,
                               ListView<ObjectDef>& objects)
    : m_string_manager(string_manager) {
  m_objects = &objects[0];
  m_length = objects.size();
}

LibraryManager::~LibraryManager() {
  for (auto& handle : m_handles) {
    dlclose(handle);
  }
}

void LibraryManager::load(const ListView<Library>& libraries) {
  for (auto i = 0; i < libraries.size(); ++i) {
    const auto& library = libraries[i];
    auto filepath = m_string_manager.lookup(library.filepath);
    auto handle = dlopen(filepath, RTLD_NOW);

    if (handle != nullptr) {
      m_handles.push_back(handle);
      auto symbol = dlsym(handle, "setup");
      if (symbol != nullptr) {
        ((setup)symbol)(this);
      }
    } else {
      std::cerr << "Library error: " << dlerror() << std::endl;
    }
  }
}
