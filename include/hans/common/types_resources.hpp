#ifndef HANS_COMMON_TYPES_RESOURCES_H_
#define HANS_COMMON_TYPES_RESOURCES_H_

#include <stdint.h>

enum hans_resource_type {
  HANS_PARAMETER,
  HANS_SHADER,
  HANS_AUDIO_BUFFER,
  HANS_FRAME_BUFFER,
  HANS_INLET,
  HANS_OUTLET
};

typedef struct {
  hans_resource_type type;
  size_t amount;
} hans_resource_request;

/// A key value object argument to create an object
typedef struct {
  hans_hash name;
  hans_resource_type type;
  union {
    hans_parameter parameter;
    hans_shader_instance shader;
    hans_fbo frame_buffer;
    hans_audio_buffer* audio_buffer;
    hans_register inlet;
    hans_register outlet;
  };
} hans_resource;

#endif // HANS_COMMON_TYPES_RESOURCES_H_
