#ifndef HANS_SCRIPT_TYPES_H_
#define HANS_SCRIPT_TYPES_H_

#include <libguile.h>
#include "hans/engine/object.hpp"

typedef struct {
  hans_hash path;
  hans_fbo fbo;
  hans_register outlet;
  float width;
  float height;
  SCM draw;
} script_data;

#endif // HANS_SCRIPT_TYPES_H_
