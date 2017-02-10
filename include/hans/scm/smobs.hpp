#ifndef HANS_GUILE_SMOBS_H
#define HANS_GUILE_SMOBS_H

#include <libguile.h>
#include <functional>
#include <new>
#include <sstream>
#include <utility>
#include <vector>
#include "hans/engine/hasher.hpp"
#include "hans/engine/primitives.hpp"
#include "hans/engine/serialize.hpp"
#include "hans/sequencer/serialize.hpp"

namespace hans {
namespace scm {
namespace detail {

class Smobs {
 public:
  using Id = uint16_t;
  using Create = std::function<void(void*, SCM)>;
  using Getter = std::function<void(void*, cereal::XMLOutputArchive&)>;
  using Setter = std::function<void(void*, cereal::XMLInputArchive&)>;
  using Save = std::function<void(void*, cereal::PortableBinaryOutputArchive&)>;
  using Load = std::function<void(void*, cereal::PortableBinaryInputArchive&)>;
  using Destroy = std::function<void(void*)>;

  struct Factory {
    Id id;
    const char* name;
    hash type;
    size_t size;
    Create create;
    Getter get;
    Setter set;
    Save save;
    Load load;
    Destroy destroy;
    Factory(const char* name, size_t size, Create create, Getter get,
            Setter set, Save save, Load load, Destroy destroy);
  };

  Smobs(Smobs const&) = delete;
  void operator=(Smobs const&) = delete;
  static Smobs& get();

  scm_t_bits tag;
  std::vector<Factory> factories;

  Id id();
  void add(Factory smob);
  Factory* lookup(const char* name);
  Factory* lookup(Id id);

  void define_enum(const char* name,
                   std::vector<std::pair<const char*, size_t>> values);
  SCM scm_to_enum(SCM scope, SCM key);
  SCM enum_to_scm(const char* scope, size_t value);

 private:
  Smobs();
  std::vector<Id> m_ids;
  std::vector<hash> m_hashes;

  std::vector<hash> m_enum_scopes;
  std::vector<std::pair<size_t, size_t>> m_enum_ranges;
  std::vector<hash> m_enum_hashes;
  std::vector<int> m_enum_values;
  std::vector<SCM> m_enum_symbols;
};

} // namespace detail

template <typename T>
void smob(const char* name, detail::Smobs::Create create,
          detail::Smobs::Destroy destroy) {
  auto get = [](void* instance, cereal::XMLOutputArchive& ar) {
    T& object = *reinterpret_cast<T*>(instance);
    ar(object);
  };

  auto set = [](void* instance, cereal::XMLInputArchive& ar) {
    T& object = *reinterpret_cast<T*>(instance);
    ar(object);
  };

  auto save = [](void* instance, cereal::PortableBinaryOutputArchive& ar) {
    T& object = *reinterpret_cast<T*>(instance);
    ar(object);
  };

  auto load = [](void* instance, cereal::PortableBinaryInputArchive& ar) {
    T& object = *reinterpret_cast<T*>(instance);
    ar(object);
  };

  auto& smobs = detail::Smobs::get();
  auto size = sizeof(T);

  detail::Smobs::Factory smob(name, size, create, get, set, save, load,
                              destroy);
  smobs.add(std::move(smob));
}

template <typename T>
void smob(const char* name, detail::Smobs::Create create) {
  auto destroy = [](void* instance) { reinterpret_cast<T*>(instance)->~T(); };
  smob<T>(name, create, destroy);
}

template <typename T>
void smob(const char* name, detail::Smobs::Destroy destroy) {
  auto create = [](void* bytes, SCM args) { new (bytes) T(); };
  smob<T>(name, create, destroy);
}

template <typename T>
void smob(const char* name) {
  auto create = [](void* bytes, SCM args) { new (bytes) T(); };
  auto destroy = [](void* instance) { reinterpret_cast<T*>(instance)->~T(); };
  smob<T>(name, create, destroy);
}

template <typename T>
T& to_cpp(SCM smob) {
  auto& smobs = detail::Smobs::get();
  scm_assert_smob_type(smobs.tag, smob);
  return *reinterpret_cast<T*>(SCM_SMOB_DATA(smob));
}

void define_enum(const char* scope,
                 std::vector<std::pair<const char*, size_t>> values);

} // namespace scm
} // namespace hans

#endif // HANS_GUILE_SMOBS_H
