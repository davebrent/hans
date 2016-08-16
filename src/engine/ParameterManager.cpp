#include "hans/engine/ParameterManager.hpp"
#include <cassert>
#include <stdexcept>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

void ParameterManager::use(ListView<Parameter> parameters,
                           ListView<Parameter::Value> values) {
  m_parameters = parameters;
  m_values = values;
}

Parameter ParameterManager::make(const ObjectDef::ID object,
                                 const hash name) const {
  for (const auto& parameter : m_parameters) {
    if (parameter.name == name && parameter.object == object) {
      return parameter;
    }
  }
  throw std::runtime_error("ParameterManager: Unknown parameter");
}

void ParameterManager::set(const Parameter& parameter,
                           const Parameter::Length& component,
                           const Parameter::Value& value) {
  assert(parameter.offset + component < m_values.size());
  m_values[parameter.offset + component] = value;
}

Parameter::Value ParameterManager::get(
    const Parameter& parameter, const Parameter::Length& component) const {
  assert(parameter.offset + component < m_values.size());
  return m_values[parameter.offset + component];
}
