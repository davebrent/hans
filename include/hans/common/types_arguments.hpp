#ifndef HANS_COMMON_TYPES_ARGUMENTS_H_
#define HANS_COMMON_TYPES_ARGUMENTS_H_

#include <stdint.h>

enum hans_argument_type { HANS_BOOL, HANS_NUMBER, HANS_STRING };

/// A key value object argument to create an object
typedef struct {
  hans_hash name;
  hans_argument_type type;
  union {
    float number;
    bool boolean;
    hans_hash string;
  };
} hans_argument;

/// A key value object argument to create an object
typedef struct {
  int32_t length;
  hans_argument* data;
} hans_arguments;

#endif // HANS_COMMON_TYPES_ARGUMENTS_H_
