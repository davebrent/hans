#include "hans/engine/PluginManager.hpp"
#include <dlfcn.h>
#include <iostream>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

typedef void (*setup)(PluginManager*);

PluginManager::PluginManager(StringManager& string_manager,
                             ListView<ObjectDef> objects,
                             ListView<Plugin> plugins)
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

PluginManager::~PluginManager() {
  for (auto& handle : m_handles) {
    dlclose(handle);
  }
}
