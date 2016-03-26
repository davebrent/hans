#ifndef HANS_ENGINE_PARAMETERMANAGER_H_
#define HANS_ENGINE_PARAMETERMANAGER_H_

#include "hans/common/types.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace hans {
namespace engine {

class ParameterManager {
 public:
  /// A raw collection of parameters and their values
  typedef struct {
    uint16_t count;
    uint16_t size;
    uint16_t capacity;
    hans_hash* names;
    hans_instance_id* instances;
    hans_parameter_size* sizes;
    // XXX: Needs two bytes padding between sizes and values
    hans_parameter_value* values;
  } data;

  explicit ParameterManager(std::vector<hans_parameter> parameters);
  ~ParameterManager();

  /// Clear all internal state
  void clear();

  /// Set the number of parameter components
  void set_capacity(unsigned capacity);
  uint16_t set_objects(const std::vector<hans_object_id>& object_ids);

  /// Create all the parameters for an object into an array of resources,
  // returns the number of created parameters
  int make(hans_object_resource* resources, const hans_object_id object_id,
           const hans_instance_id instance_id);

  /// Retrieve the value for a parameter
  hans_parameter_value get(const hans_parameter_handle& parameter,
                           const hans_parameter_size& component) const;

  /// Set the value for a given parameters component
  void set(const hans_parameter_handle& parameter,
           const hans_parameter_size& component,
           const hans_parameter_value& value);

  void copy(const hans::engine::ParameterManager& parameters);

 private:
  typedef struct {
    void* data;
    size_t bytes;
  } blob;

  /// Internal class state
  ParameterManager::data* m_data;

  /// Internal class state represented as a blob of data
  ParameterManager::blob m_blob;

  std::vector<hans_parameter> m_parameters;
};

} // namespace engine
} // namespace hans

#endif // HANS_ENGINE_PARAMETERMANAGER_H_
