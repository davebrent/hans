#ifndef HANS_COMMON_SMOBS_H
#define HANS_COMMON_SMOBS_H

#include <libguile.h>
#include <functional>
#include <new>
#include <sstream>
#include <vector>
#include "hans/common/hasher.hpp"
#include "hans/common/serialize.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace scm {
namespace detail {

class Smobs {
 public:
  using Id = uint16_t;
  using Create = std::function<void(void*, SCM)>;
  using Getter = std::function<void(void*, cereal::XMLOutputArchive&)>;
  using Setter = std::function<void(void*, cereal::XMLInputArchive&)>;
  using Destroy = std::function<void(void*)>;

  struct Factory {
    Id id;
    const char* name;
    hash type;
    size_t size;
    Create create;
    Getter get;
    Setter set;
    Destroy destroy;
    Factory(const char* name, size_t size, Create create, Getter get,
            Setter set, Destroy destroy);
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

 private:
  Smobs();
  std::vector<Id> m_ids;
  std::vector<hash> m_hashes;
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

  auto& smobs = detail::Smobs::get();
  auto size = sizeof(T);

  detail::Smobs::Factory smob(name, size, create, get, set, destroy);
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

} // namespace scm
} // namespace hans

#endif // HANS_COMMON_SMOBS_H
