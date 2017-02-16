#include "hans/plugins.hpp"
#include <dlfcn.h>
#include <iostream>

using namespace hans;

typedef void (*setup)(PluginManager*);

PluginManager::PluginManager(StringManager& strings) : m_strings(strings) {
}

PluginManager::PluginManager(StringManager& strings, const Plugins& plugins)
    : m_strings(strings) {
  for (const auto filepath : plugins.filepaths) {
    auto handle = dlopen(m_strings.lookup(filepath), RTLD_NOW);

    if (handle != nullptr) {
      m_handles.push_back(handle);
      auto symbol = dlsym(handle, "__hans_plugin_init");
      if (symbol != nullptr) {
        ((setup)symbol)(this);
      }
    } else {
      std::cerr << "Plugin error: " << dlerror() << std::endl;
    }
  }
}

PluginManager::~PluginManager() {
  m_constructors.clear();
  m_destructors.clear();
  m_serializers.clear();
  m_sizes.clear();
  m_objects.clear();

  for (auto& handle : m_handles) {
    dlclose(handle);
  }
}

static size_t find_or_throw(std::vector<hash>& objects, hash name) {
  auto it = std::find(objects.begin(), objects.end(), name);
  if (it == objects.end()) {
    throw std::runtime_error("Object not found");
  }
  return it - objects.begin();
}

Object* PluginManager::construct(hash name, ObjectDef::ID id,
                                 const std::string& state) {
  auto i = find_or_throw(m_objects, name);
  auto& constructor = m_constructors.at(i);
  return static_cast<Object*>(constructor(id, state));
}

Object* PluginManager::construct(hash name) {
  return construct(name, 0, "");
}

std::string PluginManager::serialize(hash name, Object* instance) {
  auto i = find_or_throw(m_objects, name);
  auto& serializer = m_serializers.at(i);
  return serializer(instance);
}

void PluginManager::destruct(hash name, Object* instance) {
  auto i = find_or_throw(m_objects, name);
  auto& destructor = m_destructors.at(i);
  destructor(instance);
}
