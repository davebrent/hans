#include "hans/common/smobs.hpp"
#include <cstring>
#include <sstream>
#include "hans/common/procedure.hpp"

using namespace hans;
using namespace hans::scm;

static SCM hans_object_make(SCM smob_name, SCM args) {
  auto& smobs = detail::Smobs::get();
  auto name = scm_to_locale_string(scm_symbol_to_string(smob_name));
  auto factory = smobs.lookup(name);
  if (factory == nullptr) {
    std::free(name);
    return SCM_UNDEFINED;
  }

  auto bytes = scm_gc_calloc(factory->size, factory->name);
  std::free(name);
  factory->create(bytes, args);

  auto smob = scm_new_smob(smobs.tag, reinterpret_cast<scm_t_bits>(bytes));
  SCM_SET_SMOB_FLAGS(smob, factory->id);
  return smob;
}

static SCM hans_object_mark(SCM smob) {
  // Hans objects should not contain scheme values
  return SCM_BOOL_F;
}

static size_t hans_object_free(SCM smob) {
  auto factory = detail::Smobs::get().lookup(SCM_SMOB_FLAGS(smob));
  if (factory == nullptr) {
    return 0;
  }

  auto bytes = reinterpret_cast<void*>(SCM_SMOB_DATA(smob));
  factory->destroy(bytes);
  scm_gc_free(bytes, factory->size, factory->name);
  return 0;
}

static int hans_object_print(SCM smob, SCM port, scm_print_state* pstate) {
  auto type = SCM_SMOB_FLAGS(smob);
  auto factory = detail::Smobs::get().lookup(type);
  if (factory == nullptr) {
    return 0;
  }

  auto name = scm_from_locale_string(factory->name);
  scm_puts("#<hans-object ", port);
  scm_display(name, port);
  scm_puts(">", port);
  return 1;
}

static SCM hans_object_set(SCM smob, SCM data) {
  auto& smobs = detail::Smobs::get();
  if (SCM_SMOB_PREDICATE(smobs.tag, smob) == 0) {
    return SCM_BOOL_F;
  }

  auto factory = smobs.lookup(SCM_SMOB_FLAGS(smob));
  if (factory == nullptr) {
    return SCM_BOOL_F;
  }

  auto buffer = scm_to_locale_string(data);
  std::istringstream ss(buffer);
  std::free(buffer);

  cereal::XMLInputArchive ar(ss);
  auto bytes = reinterpret_cast<void*>(SCM_SMOB_DATA(smob));
  factory->set(bytes, ar);
  return SCM_BOOL_T;
}

static SCM hans_object_get(SCM smob) {
  auto& smobs = detail::Smobs::get();
  if (SCM_SMOB_PREDICATE(smobs.tag, smob) == 0) {
    return SCM_BOOL_F;
  }

  auto factory = smobs.lookup(SCM_SMOB_FLAGS(smob));
  if (factory == nullptr) {
    return SCM_BOOL_F;
  }

  std::ostringstream ss;

  {
    cereal::XMLOutputArchive::Options opts(
        std::numeric_limits<float>::max_digits10, true, true);
    cereal::XMLOutputArchive ar(ss, opts);
    auto bytes = reinterpret_cast<void*>(SCM_SMOB_DATA(smob));
    factory->get(bytes, ar);
  }

  auto str = ss.str();
  return scm_from_utf8_stringn(str.c_str(), str.size());
}

static SCM hans_object_type(SCM smob) {
  auto& smobs = detail::Smobs::get();
  if (SCM_SMOB_PREDICATE(smobs.tag, smob) == 0) {
    return SCM_BOOL_F;
  }

  auto factory = smobs.lookup(SCM_SMOB_FLAGS(smob));
  if (factory == nullptr) {
    return SCM_UNDEFINED;
  }

  return scm_from_utf8_symbol(factory->name);
}

static SCM hans_object_p(SCM smob, SCM type) {
  auto& smobs = detail::Smobs::get();

  if (SCM_SMOB_PREDICATE(smobs.tag, smob) == 0) {
    return SCM_BOOL_F;
  }

  if (scm_is_true(scm_symbol_p(type)) == 1) {
    auto name = scm_to_locale_string(scm_symbol_to_string(type));
    auto factory = smobs.lookup(name);
    std::free(name);

    if (factory == nullptr) {
      return SCM_BOOL_F;
    }

    return scm_from_bool(factory->id == SCM_SMOB_FLAGS(smob));
  }

  return SCM_BOOL_T;
}

static SCM hans_objects() {
  SCM names = SCM_EOL;

  auto& smobs = detail::Smobs::get();
  for (auto& factory : smobs.factories) {
    auto name = scm_from_utf8_symbol(factory.name);
    names = scm_append(scm_list_2(names, scm_list_1(name)));
  }

  return names;
}

detail::Smobs::Factory::Factory(const char* _name, size_t _size,
                                detail::Smobs::Create _create,
                                detail::Smobs::Getter _get,
                                detail::Smobs::Setter _set,
                                detail::Smobs::Destroy _destroy)
    : name(_name),
      size(_size),
      create(_create),
      get(_get),
      set(_set),
      destroy(_destroy) {
}

detail::Smobs::Smobs() {
  // Zero size so that hans SMOB'S can vary in size
  tag = scm_make_smob_type("hans-object", 0);

  scm_set_smob_mark(tag, hans_object_mark);
  scm_set_smob_free(tag, hans_object_free);
  scm_set_smob_print(tag, hans_object_print);

  scm::procedure<hans_object_make>("make-hans-object", 2, 0, 0);
  scm::procedure<hans_object_p>("hans-object?", 1, 1, 0);
  scm::procedure<hans_object_set>("%set-hans-object!", 2, 0, 0);
  scm::procedure<hans_object_get>("%hans-object-get", 1, 0, 0);
  scm::procedure<hans_object_type>("hans-object-type", 1, 1, 0);
  scm::procedure<hans_objects>("hans-objects", 0, 0, 0);
}

detail::Smobs::Id detail::Smobs::id() {
  return m_ids.size();
}

detail::Smobs& detail::Smobs::get() {
  static detail::Smobs instance;
  return instance;
}

void detail::Smobs::add(detail::Smobs::Factory smob) {
  if (lookup(smob.name) == nullptr) {
    smob.id = m_ids.size();
    m_ids.push_back(smob.id);
    m_hashes.push_back(common::hasher(smob.name));
    factories.push_back(std::move(smob));
  }
}

detail::Smobs::Factory* detail::Smobs::lookup(const char* name) {
  auto _hash = common::hasher(name);
  auto i = 0;
  for (auto hash : m_hashes) {
    if (_hash == hash) {
      return &factories.at(i);
    }
    i++;
  }
  return nullptr;
}

detail::Smobs::Factory* detail::Smobs::lookup(detail::Smobs::Id id) {
  auto i = 0;
  for (auto _id : m_ids) {
    if (_id == id) {
      return &factories.at(i);
    }
    i++;
  }
  return nullptr;
}
