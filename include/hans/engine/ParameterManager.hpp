#ifndef HANS_ENGINE_PARAMETERMANAGER_H_
#define HANS_ENGINE_PARAMETERMANAGER_H_

#include <cstdint>
#include <vector>
#include "hans/common/primitives.hpp"

namespace hans {
namespace engine {

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
                       const Parameter::Length& component) const;

  /// Set the value of a parameters component
  void set(const Parameter& parameter, const Parameter::Length& component,
           const Parameter::Value& value);

  bool set(const ObjectDef::ID object, const hash name,
           const Parameter::Length component, const Parameter::Value value);

 private:
  std::vector<Parameter>& m_parameters;
  std::vector<Parameter::Value>& m_values;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PARAMETERMANAGER_H_
