#ifndef HANS_ENGINE_PARAMETERMANAGER_H_
#define HANS_ENGINE_PARAMETERMANAGER_H_

#include <cstdint>
#include "hans/common/ListView.hpp"
#include "hans/common/types.hpp"

namespace hans {
namespace engine {

class ParameterManager {
 public:
  /// Set the manager to read from a given array of values
  void use(common::ListView<hans_parameter> parameters,
           common::ListView<hans_parameter_value> values);

  /// Make an objects parameter by name
  hans_parameter make(const hans_instance_id object,
                      const hans_hash name) const;

  /// Retrieve the value for a parameters component
  hans_parameter_value get(const hans_parameter& parameter,
                           const hans_parameter_size& component) const;

  /// Set the value of a parameters component
  void set(const hans_parameter& parameter,
           const hans_parameter_size& component,
           const hans_parameter_value& value);

 private:
  hans_parameter* m_parameters = nullptr;
  hans_parameter_value* m_values = nullptr;
  size_t m_parameters_len = 0;
  size_t m_values_len = 0;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PARAMETERMANAGER_H_
