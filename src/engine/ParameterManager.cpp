#include "hans/engine/ParameterManager.hpp"
#include <cassert>
#include <stdexcept>

using namespace hans;

void engine::ParameterManager::use(
    common::ListView<hans_parameter> parameters,
    common::ListView<hans_parameter_value> values) {
  m_parameters = &parameters[0];
  m_parameters_len = parameters.size();
  m_values = &values[0];
  m_values_len = values.size();
}

hans_parameter engine::ParameterManager::make(const hans_instance_id object,
                                              const hans_hash name) const {
  auto parameters = m_parameters;
  for (auto i = 0; i < m_parameters_len; ++i) {
    if (parameters[i].name == name && parameters[i].object == object) {
      return parameters[i];
    }
  }
  throw std::runtime_error("ParameterManager: Unknown parameter");
}

void engine::ParameterManager::set(const hans_parameter& parameter,
                                   const hans_parameter_size& component,
                                   const hans_parameter_value& value) {
  assert(parameter.offset + component < m_values_len);
  m_values[parameter.offset + component] = value;
}

hans_parameter_value engine::ParameterManager::get(
    const hans_parameter& parameter,
    const hans_parameter_size& component) const {
  assert(parameter.offset + component < m_values_len);
  return m_values[parameter.offset + component];
}
