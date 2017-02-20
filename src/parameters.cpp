#include "hans/parameters.hpp"
#include <cassert>
#include <stdexcept>

using namespace hans;

ParameterManager::ParameterManager(std::vector<Parameter>& parameters,
                                   std::vector<Parameter::Value>& values)
    : _parameters(parameters), _values(values) {
}

Parameter ParameterManager::make(const ObjectDef::ID object,
                                 const hash name) const {
  for (const auto& parameter : _parameters) {
    if (parameter.name == name && parameter.object == object) {
      return parameter;
    }
  }
  throw std::runtime_error("ParameterManager: Unknown parameter");
}

void ParameterManager::set(const Parameter& parameter,
                           const Parameter::Length& component,
                           const Parameter::Value& value) {
  assert(parameter.offset + component < _values.size());
  _values[parameter.offset + component] = value;
}

void ParameterManager::update() {
  std::lock_guard<std::mutex> lck(_mutex);
  for (const auto& cmd : _commands) {
    for (auto& parameter : _parameters) {
      if (parameter.object == cmd.object && parameter.name == cmd.name) {
        set(parameter, cmd.component, cmd.value);
        break;
      }
    }
  }
}

void ParameterManager::set(const ObjectDef::ID object, const hash name,
                           const Parameter::Length component,
                           const Parameter::Value value) {
  Command cmd;
  cmd.object = object;
  cmd.name = name;
  cmd.component = component;
  cmd.value = value;

  {
    std::lock_guard<std::mutex> lck(_mutex);
    _commands.push_back(cmd);
  }
}

Parameter::Value ParameterManager::get(
    const Parameter& parameter, const Parameter::Length& component) const {
  assert(parameter.offset + component < _values.size());
  return _values[parameter.offset + component];
}
