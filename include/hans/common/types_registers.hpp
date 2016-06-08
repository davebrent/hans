#ifndef HANS_COMMON_TYPES_REGISTERS_H_
#define HANS_COMMON_TYPES_REGISTERS_H_

#include <stdint.h>

typedef struct {
  hans_instance_id object;
  /// The type of object
  hans_object_type type;
  /// The graph the register belongs to
  hans_instance_id graph;
  /// The inlet or outlet index
  uint8_t index;
  /// Data bin
  uint16_t bin;
  bool readonly;
} hans_register;

#endif // HANS_COMMON_TYPES_REGISTERS_H_
