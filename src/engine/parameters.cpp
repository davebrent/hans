#include "hans/engine/parameters.hpp"
#include <cassert>
#include <stdexcept>

using namespace hans;
using namespace hans::engine;

ParameterManager::ParameterManager(std::vector<Parameter>& parameters,
                                   std::vector<Parameter::Value>& values)
    : m_parameters(parameters), m_values(values) {
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

bool ParameterManager::set(const ObjectDef::ID object, const hash name,
                           const Parameter::Length component,
                           const Parameter::Value value) {
  auto index = 0;

  for (auto& parameter : m_parameters) {
    if (parameter.object == object && parameter.name == name) {
      auto& parameter = m_parameters[index];
      set(parameter, component, value);
      return true;
    }

    index++;
  }

  return false;
}

Parameter::Value ParameterManager::get(
    const Parameter& parameter, const Parameter::Length& component) const {
  assert(parameter.offset + component < m_values.size());
  return m_values[parameter.offset + component];
}
