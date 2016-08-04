#ifndef HANS_ENGINE_PARAMETERMANAGER_H_
#define HANS_ENGINE_PARAMETERMANAGER_H_

#include <cstdint>
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"
#include "hans/engine/types.hpp"

namespace hans {
namespace engine {

class ParameterManager {
 public:
  /// Set the manager to read from a given array of values
  void use(common::ListView<Parameter> parameters,
           common::ListView<Parameter::Value> values);

  /// Make an objects parameter by name
  Parameter make(const ObjectDef::ID object, const hash name) const;

  /// Retrieve the value for a parameters component
  Parameter::Value get(const Parameter& parameter,
                       const Parameter::Length& component) const;

  /// Set the value of a parameters component
  void set(const Parameter& parameter, const Parameter::Length& component,
           const Parameter::Value& value);

 private:
  Parameter* m_parameters = nullptr;
  Parameter::Value* m_values = nullptr;
  size_t m_parameters_len = 0;
  size_t m_values_len = 0;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PARAMETERMANAGER_H_
