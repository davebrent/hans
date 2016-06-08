#ifndef HANS_COMMON_TYPES_PARAMETERS_H_
#define HANS_COMMON_TYPES_PARAMETERS_H_

#include <stdint.h>

/// Value type of a parameter
typedef float hans_parameter_value;

/// Size of a parameter
typedef uint8_t hans_parameter_size;

/// A parameter instance
typedef struct {
  hans_instance_id object;
  hans_hash name;
  hans_parameter_size size;
  size_t offset;
} hans_parameter;

#endif // HANS_COMMON_TYPES_PARAMETERS_H_
