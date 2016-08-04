#include "hans/engine/ParameterManager.hpp"
#include <cassert>
#include <stdexcept>

using namespace hans;
using namespace hans::common;
using namespace hans::engine;

void ParameterManager::use(ListView<Parameter> parameters,
                           ListView<Parameter::Value> values) {
  m_parameters = &parameters[0];
  m_parameters_len = parameters.size();
  m_values = &values[0];
  m_values_len = values.size();
}

Parameter ParameterManager::make(const ObjectDef::ID object,
                                 const hash name) const {
  auto parameters = m_parameters;
  for (auto i = 0; i < m_parameters_len; ++i) {
    if (parameters[i].name == name && parameters[i].object == object) {
      return parameters[i];
    }
  }
  throw std::runtime_error("ParameterManager: Unknown parameter");
}

void ParameterManager::set(const Parameter& parameter,
                           const Parameter::Length& component,
                           const Parameter::Value& value) {
  assert(parameter.offset + component < m_values_len);
  m_values[parameter.offset + component] = value;
}

Parameter::Value ParameterManager::get(
    const Parameter& parameter, const Parameter::Length& component) const {
  assert(parameter.offset + component < m_values_len);
  return m_values[parameter.offset + component];
}
