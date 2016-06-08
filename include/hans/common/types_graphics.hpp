#ifndef HANS_COMMON_TYPES_GRAPHICS_H_
#define HANS_COMMON_TYPES_GRAPHICS_H_

#include <stdint.h>

enum hans_shader_type { HANS_SHADER_VERTEX, HANS_SHADER_FRAGMENT };

typedef struct {
  hans_shader_type type;
  hans_hash name;
  hans_hash code;
} hans_shader;

typedef uint16_t hans_shader_handle;
typedef int16_t hans_shader_program_handle;

typedef struct {
  hans_shader_handle handle;
  hans_hash name;
} hans_shader_instance;

typedef struct {
  hans_shader_program_handle handle;
} hans_shader_program_instance;

typedef struct {
  hans_instance_id object;
  bool stencil_buffer;
  size_t start;
  size_t end;
} hans_fbo;

enum hans_fbo_attachment_type {
  HANS_COLOR_ATTACHMENT,
  HANS_DEPTH_ATTACHMENT,
  HANS_STENCIL_ATTACHMENT
};

typedef struct {
  hans_fbo_attachment_type type;
  uint32_t width;
  uint32_t height;
  uint32_t components;
} hans_fbo_attachment;

#endif // HANS_COMMON_TYPES_GRAPHICS_H_
