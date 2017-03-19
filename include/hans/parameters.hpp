#ifndef HANS_PARAMETERS_H_
#define HANS_PARAMETERS_H_

#include <cstdint>
#include <mutex>
#include <vector>
#include "hans/primitives.hpp"

namespace hans {

class ParameterManager {
 public:
  ParameterManager(const ParameterManager& other) = delete;
  /// Set the manager to read from a given array of values
  ParameterManager(std::vector<Parameter>& parameters,
                   std::vector<Parameter::Value>& values);

  /// Make an objects parameter by name
  Parameter make(const ObjectDef::ID object, const hash name) const;

  /// Retrieve the value for a parameters component
  Parameter::Value get(const Parameter& parameter,
                       const Parameter::Length component) const;

  /// Set the value of a parameters component
  void set(const Parameter& parameter, const Parameter::Length component,
           const Parameter::Value value);

  void set(const ObjectDef::ID object, const hash name,
           const Parameter::Length component, const Parameter::Value value);

  void update();

 private:
  struct Command {
    ObjectDef::ID object;
    hash name;
    Parameter::Length component;
    Parameter::Value value;
  };

  std::vector<Parameter>& _parameters;
  std::vector<Parameter::Value>& _values;
  std::vector<Command> _commands;
  std::mutex _mutex;
};

} // namespace hans

#endif // HANS_PARAMETERS_H_
