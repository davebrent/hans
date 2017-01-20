#ifndef HANS_PLUGINMANAGER_H_
#define HANS_PLUGINMANAGER_H_

#include <cereal/archives/portable_binary.hpp>
#include <cereal/cereal.hpp>
#include <cereal/external/base64.hpp>
#include <functional>
#include <iostream>
#include <sstream>
#include <vector>
#include <cstring>
#include "hans/common/StringManager.hpp"
#include "hans/common/types.hpp"

#define HANS_PLUGIN_INIT extern "C" void __hans_plugin_init

namespace hans {
namespace engine {

class PluginManager {
 public:
  PluginManager(common::StringManager& string_manager,
                common::ListView<ObjectDef> objects);

  PluginManager(common::StringManager& string_manager,
                common::ListView<ObjectDef> objects,
                common::ListView<Plugin> plugins);
  ~PluginManager();

  template <typename State, typename Object>
  bool add_object(const char* name) {
    auto size = sizeof(State);

    auto create = [](ObjectDef::ID id, const std::string& state) {
      auto instance = new Object(id);
      std::memset(&instance->state, 0, sizeof(State));

      if (state.size() != 0) {
        auto buffer = cereal::base64::decode(state);
        std::istringstream is(buffer, std::ios::binary);
        cereal::PortableBinaryInputArchive ar(is);
        ar(instance->state);
      }

      return instance;
    };

    auto destroy = [](void* instance) {
      delete static_cast<Object*>(instance);
    };

    auto serialize = [](void* instance) {
      auto obj = static_cast<Object*>(instance);
      std::ostringstream os;

      {
        cereal::PortableBinaryOutputArchive ar(os);
        ar(obj->state);
      }

      auto str = os.str();
      auto ptr = reinterpret_cast<unsigned const char*>(str.c_str());
      return cereal::base64::encode(ptr, str.size());
    };

    auto found = false;
    auto hash = m_string_manager.intern(name);

    for (auto& object : m_objects) {
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
  common::ListView<ObjectDef> m_objects;
};

} // namespace engine
} // namespace hans

#endif // HANS_PLUGINMANAGER_H_
