#ifndef HANS_PLUGINMANAGER_H_
#define HANS_PLUGINMANAGER_H_

#include <algorithm>
#include <cereal/archives/portable_binary.hpp>
#include <cereal/cereal.hpp>
#include <cereal/external/base64.hpp>
#include <cstring>
#include <functional>
#include <iostream>
#include <sstream>
#include <vector>
#include "hans/common/StringManager.hpp"
#include "hans/common/primitives.hpp"

#define HANS_PLUGIN_INIT extern "C" void __hans_plugin_init

namespace hans {
namespace engine {

class Object;

class PluginManager {
 public:
  using Constructor = std::function<void*(ObjectDef::ID, const std::string&)>;
  using Destructor = std::function<void(void*)>;
  using Serializer = std::function<std::string(void*)>;

  PluginManager(const PluginManager& other) = delete;
  PluginManager(common::StringManager& strings);
  PluginManager(common::StringManager& strings, const Plugins& plugins);
  ~PluginManager();

  Object* construct(hash name);
  Object* construct(hash name, ObjectDef::ID id, const std::string& state);
  void destruct(hash name, Object* object);
  std::string serialize(hash name, Object* object);

  template <typename State, typename Object>
  bool add_object(const char* name) {
    auto hash = m_strings.intern(name);
    auto it = std::find(m_objects.begin(), m_objects.end(), hash);
    if (it != m_objects.end()) {
      return false;
    }

    auto constructor = [](ObjectDef::ID id, const std::string& state) {
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

    auto destructor = [](void* instance) {
      auto object = static_cast<Object*>(instance);
      delete object;
    };

    auto serializer = [](void* instance) {
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

    m_objects.push_back(hash);
    m_sizes.push_back(sizeof(Object));
    m_constructors.push_back(constructor);
    m_destructors.push_back(destructor);
    m_serializers.push_back(serializer);
    return true;
  }

 private:
  common::StringManager& m_strings;
  std::vector<hash> m_objects;
  std::vector<Constructor> m_constructors;
  std::vector<Destructor> m_destructors;
  std::vector<Serializer> m_serializers;
  std::vector<size_t> m_sizes;
  std::vector<void*> m_handles;
};

} // namespace engine
} // namespace hans

#endif // HANS_PLUGINMANAGER_H_
