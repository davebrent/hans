#include "hans/scm/smobs.hpp"
#include <cstring>
#include <fstream>
#include <sstream>
#include "hans/scm/procedure.hpp"

using namespace hans;
using namespace hans::scm;

static SCM hans_primitive_make(SCM smob_name, SCM args) {
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

static SCM hans_primitive_mark(SCM smob) {
  // Hans objects should not contain scheme values
  return SCM_BOOL_F;
}

static size_t hans_primitive_free(SCM smob) {
  auto factory = detail::Smobs::get().lookup(SCM_SMOB_FLAGS(smob));
  if (factory == nullptr) {
    return 0;
  }

  auto bytes = reinterpret_cast<void*>(SCM_SMOB_DATA(smob));
  factory->destroy(bytes);
  scm_gc_free(bytes, factory->size, factory->name);
  return 0;
}

static int hans_primitive_print(SCM smob, SCM port, scm_print_state* pstate) {
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

static SCM hans_primitive_set(SCM smob, SCM data) {
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

static SCM hans_primitive_get(SCM smob) {
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

static SCM hans_primitive_save(SCM smob, SCM filepath) {
  auto& smobs = detail::Smobs::get();
  if (SCM_SMOB_PREDICATE(smobs.tag, smob) == 0) {
    return SCM_BOOL_F;
  }

  auto factory = smobs.lookup(SCM_SMOB_FLAGS(smob));
  if (factory == nullptr) {
    return SCM_BOOL_F;
  }

  auto path = scm_to_locale_string(filepath);
  std::ofstream of(path, std::ios::binary);
  std::free(path);

  {
    cereal::PortableBinaryOutputArchive ar(of);
    auto bytes = reinterpret_cast<void*>(SCM_SMOB_DATA(smob));
    factory->save(bytes, ar);
  }

  of.close();
  return SCM_BOOL_T;
}

static SCM hans_primitive_load(SCM smob, SCM filepath) {
  auto& smobs = detail::Smobs::get();
  if (SCM_SMOB_PREDICATE(smobs.tag, smob) == 0) {
    return SCM_BOOL_F;
  }

  auto factory = smobs.lookup(SCM_SMOB_FLAGS(smob));
  if (factory == nullptr) {
    return SCM_BOOL_F;
  }

  auto path = scm_to_locale_string(filepath);
  std::ifstream is(path, std::ios::binary);
  std::free(path);

  cereal::PortableBinaryInputArchive ar(is);
  auto bytes = reinterpret_cast<void*>(SCM_SMOB_DATA(smob));
  factory->load(bytes, ar);
  return SCM_BOOL_T;
}

static SCM hans_primitive_type(SCM smob) {
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

static SCM hans_primitive_p(SCM smob, SCM type) {
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

static SCM hans_primitive_enum(SCM scope, SCM key) {
  auto& smobs = detail::Smobs::get();
  return smobs.scm_to_enum(scope, key);
}

static SCM hans_primitives() {
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
                                detail::Smobs::Save _save,
                                detail::Smobs::Load _load,
                                detail::Smobs::Destroy _destroy)
    : name(_name),
      size(_size),
      create(_create),
      get(_get),
      set(_set),
      save(_save),
      load(_load),
      destroy(_destroy) {
}

detail::Smobs::Smobs() {
  // Zero size so that hans SMOB'S can vary in size
  tag = scm_make_smob_type("hans-primitive", 0);

  scm_set_smob_mark(tag, hans_primitive_mark);
  scm_set_smob_free(tag, hans_primitive_free);
  scm_set_smob_print(tag, hans_primitive_print);

  scm::procedure<hans_primitive_make>("make-hans-primitive", 2, 0, 0);
  scm::procedure<hans_primitive_p>("hans-primitive?", 1, 1, 0);
  scm::procedure<hans_primitive_set>("%set-hans-primitive!", 2, 0, 0);
  scm::procedure<hans_primitive_get>("%hans-primitive-get", 1, 0, 0);
  scm::procedure<hans_primitive_save>("hans-primitive->file", 2, 0, 0);
  scm::procedure<hans_primitive_load>("file->hans-primitive", 2, 0, 0);
  scm::procedure<hans_primitive_type>("hans-primitive-type", 1, 1, 0);
  scm::procedure<hans_primitive_enum>("hans-primitive-enum", 2, 0, 0);
  scm::procedure<hans_primitives>("hans-primitives", 0, 0, 0);
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
    m_hashes.push_back(engine::hasher(smob.name));
    factories.push_back(std::move(smob));
  }
}

detail::Smobs::Factory* detail::Smobs::lookup(const char* name) {
  auto _hash = engine::hasher(name);
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

void detail::Smobs::define_enum(
    const char* scope, std::vector<std::pair<const char*, size_t>> values) {
  auto start = m_enum_hashes.size();
  auto end = start + values.size();

  m_enum_scopes.push_back(engine::hasher(scope));
  m_enum_ranges.push_back({start, end});

  for (const auto& pair : values) {
    auto name = std::get<0>(pair);
    auto value = std::get<1>(pair);

    m_enum_hashes.push_back(engine::hasher(name));
    m_enum_values.push_back(value);

    auto symbol = scm_permanent_object(scm_from_utf8_symbol(name));
    m_enum_symbols.push_back(symbol);
  }
}

SCM detail::Smobs::scm_to_enum(SCM scope, SCM key) {
  auto i = 0;
  auto scope_str = scm_symbol_to_string(scope);
  auto str = scm_to_locale_string(scope_str);
  auto scope_hash = engine::hasher(str);
  std::free(str);

  for (const auto& _scope : m_enum_scopes) {
    if (_scope != scope_hash) {
      i++;
      continue;
    }

    auto sym = scm_symbol_to_string(key);
    auto str = scm_to_locale_string(sym);
    auto hash = engine::hasher(str);
    std::free(str);

    auto range = m_enum_ranges.at(i);
    auto start = std::get<0>(range);
    auto end = std::get<1>(range);

    for (auto s = start; s < end; ++s) {
      auto _hash = m_enum_hashes.at(s);
      if (_hash == hash) {
        return scm_from_int(m_enum_values.at(s));
      }
    }
  }

  return SCM_BOOL_F;
}

SCM detail::Smobs::enum_to_scm(const char* scope, size_t value) {
  auto i = 0;
  auto scope_hash = engine::hasher(scope);
  for (const auto& _scope : m_enum_scopes) {
    if (_scope != scope_hash) {
      i++;
      continue;
    }

    auto range = m_enum_ranges.at(i);
    auto start = std::get<0>(range);
    auto end = std::get<1>(range);

    for (auto s = start; s < end; ++s) {
      auto _value = m_enum_values.at(s);
      if (_value == value) {
        return m_enum_symbols.at(s);
      }
    }
  }

  return SCM_BOOL_F;
}

void scm::define_enum(const char* name,
                      std::vector<std::pair<const char*, size_t>> values) {
  auto& smobs = detail::Smobs::get();
  smobs.define_enum(name, values);
}

extern "C" {
void scm_init_hans_scm() {
  detail::Smobs::get();
}
}
