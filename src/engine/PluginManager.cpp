#include "hans/engine/PluginManager.hpp"
#include <dlfcn.h>
#include <iostream>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

typedef void (*setup)(PluginManager*);

PluginManager::PluginManager(StringManager& string_manager,
                             std::vector<ObjectDef>& objects)
    : m_string_manager(string_manager), m_objects(objects) {
}

PluginManager::PluginManager(StringManager& string_manager,
                             std::vector<ObjectDef>& objects,
                             const std::vector<Plugin>& plugins)
    : m_string_manager(string_manager), m_objects(objects) {
  for (auto i = 0; i < plugins.size(); ++i) {
    const auto& plugin = plugins[i];
    auto filepath = m_string_manager.lookup(plugin.filepath);
    auto handle = dlopen(filepath, RTLD_NOW);

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

void PluginManager::destroy(hash name, Object* instance) {
  for (const auto& object : m_objects) {
    if (name == object.name) {
      object.destroy(instance);
      break;
    }
  }
}

std::string PluginManager::serialize(hash name, Object* instance) {
  for (const auto& object : m_objects) {
    if (name == object.name) {
      return object.serialize(instance);
    }
  }
  throw std::runtime_error("Object not found");
}

Object* PluginManager::create(hash name, ObjectDef::ID id,
                              const std::string& state) {
  for (const auto& object : m_objects) {
    if (name == object.name) {
      return static_cast<Object*>(object.create(id, state));
    }
  }
  return nullptr;
}

Object* PluginManager::create(hash name) {
  return create(name, 0, "");
}

PluginManager::~PluginManager() {
  for (auto& object : m_objects) {
    object.size = 0;
    object.create = nullptr;
    object.destroy = nullptr;
    object.serialize = nullptr;
  }

  for (auto& handle : m_handles) {
    dlclose(handle);
  }
}
